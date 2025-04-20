(ns clojure-mcp.tools.glob-files.core
  "Core implementation for the glob-files tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import
   (java.nio.file FileVisitResult FileSystems Files Path Paths SimpleFileVisitor)
   (java.nio.file.attribute BasicFileAttributes)))

(defn glob-files
  "Fast file pattern matching using glob patterns like **.clj or src/**/*.cljs.

   Arguments:
   - path: Root directory to start the search from
   - pattern: Glob pattern (e.g. \"**/*.clj\", \"src/**/*.tsx\")
   - max-results: Maximum number of results to return (default: 1000)

   Returns a map with:
   - :filenames - Sequence of matching file paths, sorted by modification time (newest first)
   - :numFiles - Number of matching files found
   - :durationMs - Duration of the search in milliseconds
   - :truncated - Boolean indicating if results were truncated due to max-results limit"
  [dir pattern & {:keys [max-results] :or {max-results 1000}}]
  (let [start (System/currentTimeMillis)
        dir-file (io/file dir)]
    (if (and (.exists dir-file) (.isDirectory dir-file))
      (try
        ;; Special case for "**/*.ext" pattern to also include root files
        (let [should-check-root? (re-matches #"^\*\*/\*\.\w+$" pattern)
              root-pattern (when should-check-root?
                             (subs pattern 3)) ; Remove the "**/" prefix

              path (Paths/get (.getAbsolutePath dir-file) (into-array String []))
              matcher (try
                        (.getPathMatcher (FileSystems/getDefault) (str "glob:" pattern))
                        (catch Exception e
                          ;; Handle invalid glob pattern
                          (let [end (System/currentTimeMillis)]
                            (throw (ex-info
                                    (str "Invalid glob pattern: " (.getMessage e))
                                    {:durationMs (- end start)})))))
              root-matcher (when should-check-root?
                             (try
                               (.getPathMatcher (FileSystems/getDefault) (str "glob:" root-pattern))
                               (catch Exception _ nil)))
              matches (atom [])
              truncated (atom false)]

          ;; Add special handling for root files if using the "**/*.ext" pattern
          (when (and should-check-root? root-matcher)
            (doseq [file (.listFiles dir-file)]
              (when (and (.isFile file)
                         (< (count @matches) max-results)
                         (.matches root-matcher
                                   (Paths/get (.getName file) (into-array String []))))
                (swap! matches conj {:path (str file)
                                     :mtime (.lastModified file)}))))

          (Files/walkFileTree
           path
           (proxy [SimpleFileVisitor] []
             (preVisitDirectory [dir _attrs]
               (let [dirName (str (.getFileName dir))]
                 (if (.startsWith dirName ".")
                   FileVisitResult/SKIP_SUBTREE
                   FileVisitResult/CONTINUE)))
             (visitFile [file _attrs]
               (let [rel (.relativize path file)]
                 (when (and (< (count @matches) max-results)
                            (.matches matcher rel)
                            ;; Skip root files we've already added with the special case
                            (not (and should-check-root?
                                      (= (.getNameCount rel) 1)
                                      root-matcher
                                      (.matches root-matcher (.getFileName rel)))))
                   (swap! matches conj {:path (str file)
                                        :mtime (.lastModified (.toFile file))}))
                 (when (>= (count @matches) max-results)
                   (reset! truncated true))
                 FileVisitResult/CONTINUE))))

          (let [end (System/currentTimeMillis)
                duration (- end start)
                sorted-results (->> @matches
                                    (sort-by :mtime >)
                                    (map :path)
                                    vec)]
            {:filenames sorted-results
             :numFiles (count sorted-results)
             :durationMs duration
             :truncated @truncated}))

        (catch Exception e
          (if (ex-data e)
            (let [data (ex-data e)]
              ;; For invalid glob pattern, return empty results rather than an error
              {:filenames []
               :numFiles 0
               :durationMs (:durationMs data)})
            ;; For other exceptions, return an error
            {:error (.getMessage e)
             :durationMs (- (System/currentTimeMillis) start)})))
      {:error (str dir " is not a valid directory")
       :durationMs 0})))

(comment
  ;; Example usage for testing in REPL
  (let [cwd (System/getProperty "user.dir")]
    (glob-files
     cwd
     "**/top_level*.clj")))