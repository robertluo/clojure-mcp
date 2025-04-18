(ns clojure-mcp.repl-tools.filesystem.core
  "Core filesystem operations for Clojure REPL tools.
   Provides file listing, reading, searching, and information gathering functions."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn list-directory
  "Lists files and directories at the specified path.
   Returns a map with :files and :directories vectors."
  [path]
  (let [dir (io/file path)]
    (if (.exists dir)
      (if (.isDirectory dir)
        (let [contents (.listFiles dir)
              files (filter #(.isFile %) contents)
              dirs (filter #(.isDirectory %) contents)]
          {:files (mapv #(.getName %) files)
           :directories (mapv #(.getName %) dirs)
           :full-path (.getAbsolutePath dir)})
        {:error (str path " is not a directory")})
      {:error (str path " does not exist")})))

(defn read-file-contents
  "Reads the contents of a file at the specified path.
   Returns the file contents as a string.
   
   Optional arguments:
   - :max-lines - Maximum number of lines to read (default: nil, reads all lines)
   - :max-line-length - Maximum length per line before truncation (default: nil, no truncation)
   - :offset - Line number to start reading from (0-indexed, default: 0)
   
   If the file doesn't exist or cannot be read, returns an error map."
  [path & {:keys [max-lines max-line-length offset]
           :or {max-line-length nil offset 0}}]
  (let [file (io/file path)]
    (if (.exists file)
      (if (.isFile file)
        (try
          (if (and (nil? max-lines) (zero? offset) (nil? max-line-length))
            ;; Simple case - just read the whole file
            {:content (slurp file)
             :path (.getAbsolutePath file)
             :truncated? false}
            ;; Complex case with limits
            (let [size (.length file)
                  lines (with-open [rdr (io/reader file)]
                          (doall
                           (cond->> (drop offset (line-seq rdr))
                             max-lines (take (inc max-lines))
                             true (map
                                   (fn [line]
                                     (if (and max-line-length (> (count line) max-line-length))
                                       (str (subs line 0 max-line-length) "...")
                                       line))))))
                  truncated-by-lines? (and max-lines (> (count lines) max-lines))
                  content-lines (if truncated-by-lines? (take max-lines lines) lines)
                  content (str/join "\n" content-lines)]
              {:content content
               :path (.getAbsolutePath file)
               :truncated? truncated-by-lines?
               :truncated-by (when truncated-by-lines? "max-lines")
               :size size
               :line-count (count content-lines)
               :offset offset
               :max-line-length max-line-length
               :line-lengths-truncated? (and max-line-length
                                             (some #(.contains % "...") lines))}))
          (catch Exception e
            {:error (str "Error reading file: " (.getMessage e))}))
        {:error (str path " is not a file")})
      {:error (str path " does not exist")})))

;; Removed file-info function

;; Removed format-file-info function

(defn directory-tree
  "Creates a recursive tree view of directory structure using simple indentation.
   
   Arguments:
   - path: Root directory path to start from
   - depth: Current depth level (used for indentation)
   - max-depth: Maximum depth to traverse (nil for unlimited)
   
   Returns a string representation of the directory tree with proper indentation.
   Filters out temporary files like Emacs backup files (ending with ~).
   Adds a \"...\" indicator when directories are truncated due to max-depth."
  [path & {:keys [depth max-depth] :or {depth 0 max-depth nil}}]
  (let [dir (io/file path)]
    (if (and (.exists dir) (.isDirectory dir))
      (let [indent (apply str (repeat depth "  "))
            contents (.listFiles dir)
            ; Filter out temporary files (like Emacs backup files)
            is-temp-file? (fn [file]
                            (let [name (.getName file)]
                              (or (.endsWith name "~") ; Emacs backup
                                  (.startsWith name ".") ; Hidden files
                                  (.startsWith name "#") ; Emacs auto-save
                                  (.contains name ".#")))) ; Emacs lock files
            dirs (filter #(and (.isDirectory %) (not (is-temp-file? %))) contents)
            files (filter #(and (.isFile %) (not (is-temp-file? %))) contents)
            result (StringBuilder.)
            continue? (or (nil? max-depth) (< depth max-depth))]

        ;; Root entry
        (when (zero? depth)
          (.append result (str (.getAbsolutePath dir) "\n")))

        ;; Directory entries
        (doseq [d (sort-by #(.getName %) dirs)]
          (.append result (str indent "- " (.getName d) "/\n"))
          (cond
            ;; If max-depth is set and we're at the limit, add truncation indicator
            (and max-depth (= depth max-depth))
            (.append result (str indent "  - ...\n"))

            ;; Otherwise process subdirectory if we should continue
            continue?
            (let [subtree (directory-tree (.getAbsolutePath d)
                                          :depth (inc depth)
                                          :max-depth max-depth)]
              (when (and subtree (not (map? subtree)) (not (empty? subtree)))
                ;; Indent each line of the subtree with the current indentation
                (let [subtree-lines (str/split-lines subtree)
                      indented-lines (map #(str indent "  " %) subtree-lines)]
                  (.append result (str (str/join "\n" indented-lines) "\n")))))))

        ;; File entries
        (let [sorted-files (sort-by #(.getName %) files)]
          (doseq [f sorted-files]
            (.append result (str indent "- " (.getName f) "\n"))))

        (.toString result))
      {:error (str path " is not a valid directory")})))

;; Removed directory-tree-json function

;; Removed search-files function (replaced by glob-files)

(defn glob-pattern->regex
  "Convert glob pattern to regex pattern.
   Simple implementation supporting common glob features:
   - * matches any sequence except /
   - ** matches any sequence including /
   - ? matches any single character
   - [abc] matches any character in the set
   - [!abc] or [^abc] matches any character not in the set"
  [glob-pattern]
  (-> glob-pattern
      (str/replace #"\\." "\\\\.") ; Escape literal dots
      (str/replace #"\\/" "\\\\/") ; Escape literal slashes
      (str/replace #"\\\\\*\\\\\*" "DOUBLE_STAR") ; Preserve escaped ** temporarily
      (str/replace #"\\\\\*" "SINGLE_STAR") ; Preserve escaped * temporarily
      (str/replace #"\\\\\?" "SINGLE_QMARK") ; Preserve escaped ? temporarily
      (str/replace #"\*\*" ".*") ; ** matches anything including /
      (str/replace #"\*" "[^/]*") ; * matches anything except /
      (str/replace #"\?" ".") ; ? matches any single character
      (str/replace #"DOUBLE_STAR" "\\*\\*") ; Restore escaped **
      (str/replace #"SINGLE_STAR" "\\*") ; Restore escaped *
      (str/replace #"SINGLE_QMARK" "\\?")))

(defn glob-files
  "Fast file pattern matching using glob patterns.
   
   Arguments:
   - dir: Root directory to start search
   - pattern: Glob pattern string (e.g. \"**/*.clj\")
   
   Returns a map with:
   - :filenames - Vector of matching file paths sorted by modification time
   - :numFiles - Number of matching files found
   - :durationMs - Time taken for the search in milliseconds
   - :truncated - Whether the results were truncated"
  [dir pattern & {:keys [max-results] :or {max-results 1000}}]
  (let [start-time (System/currentTimeMillis)
        dir-file (io/file dir)]
    (if (and (.exists dir-file) (.isDirectory dir-file))
      (try
        (let [pattern-regex (re-pattern (glob-pattern->regex pattern))
              root-path (.getAbsolutePath dir-file)
              matches (atom [])
              truncated (atom false)]

          ; Function to recursively collect files
          (letfn [(collect-files [file]
                    (when (< (count @matches) max-results)
                      (if (.isDirectory file)
                        ; For directories, process all children
                        (doseq [child (.listFiles file)]
                          (collect-files child))
                        ; For files, check if they match the pattern
                        (let [abs-path (.getAbsolutePath file)
                              rel-path (-> abs-path
                                           (str/replace (str root-path "/") ""))]
                          (when (re-matches pattern-regex rel-path)
                            (swap! matches conj {:path abs-path
                                                 :mtime (.lastModified file)}))))))]

            ; Start collection from root directory
            (collect-files dir-file)

            ; Check if truncated
            (when (>= (count @matches) max-results)
              (reset! truncated true))

            (let [end-time (System/currentTimeMillis)
                  duration (- end-time start-time)
                  sorted-results (->> @matches
                                      (sort-by :mtime >)
                                      (map :path)
                                      vec)]
              {:filenames sorted-results
               :numFiles (count sorted-results)
               :durationMs duration
               :truncated @truncated})))
        (catch Exception e
          {:error (.getMessage e)
           :durationMs (- (System/currentTimeMillis) start-time)}))
      {:error (str dir " is not a valid directory")
       :durationMs 0})))
