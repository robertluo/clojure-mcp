(ns clojure-mcp.repl-tools.filesystem.grep
  "Support for searching file contents using regular expressions."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as shell]))

(defn grep-with-command
  "Uses the system grep command to search file contents.
   Uses extended regular expression syntax (-E flag) for advanced patterns.
   Supports multiple file types with brace expansion (e.g., \"*.{clj,md}\").
   
   Arguments:
   - path: Directory to search in
   - pattern: Regular expression pattern to search for
   - include: Optional file pattern to include
   - max-results: Maximum number of results to return
   
   Returns a map with search results or error information."
  [path pattern include max-results]
  (let [start-time (System/currentTimeMillis)
        ;; Parse the include pattern to handle multiple extensions
        include-patterns (if (and include (.contains include "{"))
                           (let [pattern-match (re-find #"\*\.\{(.+)\}" include)]
                             (when pattern-match
                               (map #(str "*." %) (clojure.string/split (second pattern-match) #","))))
                           [include])
        ;; Build command with multiple --include flags if needed
        cmd-args (as-> ["grep" "-E" "-l" "-r" "--exclude-dir=.*"] args
                   (if (and include (> (count include-patterns) 1))
                     (concat args (mapv #(str "--include=" %) include-patterns))
                     (if include
                       (concat args [(str "--include=" include)])
                       args))
                   (concat args [pattern path]))
        result (apply shell/sh cmd-args)
        exit-code (:exit result)]
    (if (or (zero? exit-code) (= exit-code 1)) ;; 1 means no matches, which is not an error
      (let [end-time (System/currentTimeMillis)
            duration (- end-time start-time)
            matching-files (when (seq (:out result))
                             (str/split-lines (:out result)))
            file-times (when matching-files
                         (map (fn [file-path]
                                {:path file-path
                                 :mtime (.lastModified (io/file file-path))})
                              matching-files))
            ;; Sort by modification time (newest first) and limit results
            sorted-limited-files (as-> file-times $
                                   (sort-by :mtime > $)
                                   (take max-results $)
                                   (mapv :path $))]
        {:filenames sorted-limited-files
         :numFiles (count sorted-limited-files)
         :durationMs duration})
      {:error (str "grep error: " (:err result))
       :durationMs (- (System/currentTimeMillis) start-time)})))

(defn grep-with-java
  "Fallback implementation that uses Java to search file contents.
   This implementation recursively traverses the directory structure,
   reading each file line by line and checking for matches using regular expressions.
   
   Features:
   - Pure Java implementation that works without external commands
   - Supports file filtering with glob-like patterns
   - Stops searching a file as soon as a match is found to improve performance
   - Sorts results by file modification time
   - Handles errors gracefully by skipping problematic files
   
   Arguments:
   - path: Directory to search in
   - pattern: Regular expression pattern to search for
   - include: Optional file pattern to include (e.g. \"*.clj\", \"*.{clj,cljs}\")
   - max-results: Maximum number of results to return
   
   Returns a map with:
   - :filenames - Vector of matching file paths sorted by modification time
   - :numFiles - Number of files containing matches
   - :durationMs - Time taken for the search in milliseconds"
  [path pattern include max-results]
  (let [start-time (System/currentTimeMillis)
        dir-file (io/file path)
        matches (atom [])
        include-pattern (when include
                          (re-pattern (str ".*\\.("
                                           (-> include
                                               (str/replace #"^\*\." "")
                                               (str/replace #"\*\.\{(.+)\}" "$1")
                                               (str/replace #"," "|"))
                                           ")$")))
        pattern-regex (re-pattern pattern)]

    ;; Recursive function to search files
    (letfn [(search-directory [dir]
              (when (< (count @matches) max-results)
                (doseq [file (.listFiles dir)]
                  (when (< (count @matches) max-results)
                    (cond
                      (and (.isDirectory file)
                           (not (str/starts-with? (.getName file) ".")))
                      (search-directory file)

                      (.isFile file)
                      (let [file-path (.getAbsolutePath file)
                            file-name (.getName file)]
                        (when (or (nil? include-pattern)
                                  (re-matches include-pattern file-name))
                          (try
                            (with-open [rdr (io/reader file)]
                              (loop [lines (line-seq rdr)]
                                (when (and (seq lines) (< (count @matches) max-results))
                                  (if (re-find pattern-regex (first lines))
                                    (swap! matches conj {:path file-path
                                                         :mtime (.lastModified file)})
                                    (recur (rest lines))))))
                            (catch Exception _ nil)))))))))]

      ;; Start the search
      (search-directory dir-file)

      (let [end-time (System/currentTimeMillis)
            duration (- end-time start-time)
            sorted-results (->> @matches
                                (sort-by :mtime >)
                                (map :path)
                                vec)]
        {:filenames sorted-results
         :numFiles (count sorted-results)
         :durationMs duration}))))

(defn grep-files
  "Fast content search tool that searches file contents using regular expressions.
   
   Arguments:
   - path: Directory to search in (defaults to current working directory)
   - pattern: Regular expression pattern to search for
   - include: Optional file pattern to include (e.g. \"*.clj\", \"*.{clj,cljs}\")
   - max-results: Maximum number of results to return (default: 1000)
   
   Returns a map with:
   - :filenames - Vector of files containing the pattern, sorted by modification time
   - :numFiles - Number of matching files found
   - :durationMs - Time taken for the search in milliseconds"
  [path pattern & {:keys [include max-results] :or {include nil max-results 1000}}]
  (let [start-time (System/currentTimeMillis)
        dir-file (io/file path)]
    (if (and (.exists dir-file) (.isDirectory dir-file))
      (try
        ;; Check if grep is available
        (let [grep-result (shell/sh "which" "grep")
              grep-available? (zero? (:exit grep-result))]
          (if grep-available?
            ;; Use system grep for performance
            (grep-with-command path pattern include max-results)
            ;; Fallback to Java implementation if grep is not available
            (grep-with-java path pattern include max-results)))
        (catch Exception e
          {:error (.getMessage e)
           :durationMs (- (System/currentTimeMillis) start-time)}))
      {:error (str path " is not a valid directory")
       :durationMs 0})))

(comment
  ;; Test the grep function
  (grep-files "." "defn" :include "*.clj")
  (grep-files "src" "defn" :include "*.clj"))
