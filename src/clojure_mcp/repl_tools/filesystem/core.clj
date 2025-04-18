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

(defn file-info
  "Gets detailed information about a file or directory.
   Returns a map with file/directory metadata including size, timestamps, and permissions."
  [path]
  (let [file (io/file path)]
    (if (.exists file)
      {:name (.getName file)
       :path (.getAbsolutePath file)
       :size (.length file)
       :directory? (.isDirectory file)
       :last-modified (.lastModified file)
       :readable? (.canRead file)
       :writable? (.canWrite file)
       :executable? (.canExecute file)
       :hidden? (.isHidden file)
       :parent (when-let [p (.getParent file)] p)}
      {:error (str path " does not exist")})))

(defn format-file-info
  "Formats file information into a human-readable string.
   
   Arguments:
   - info: File information map from file-info function
   
   Returns a formatted string with file details"
  [info]
  (if (:error info)
    (:error info)
    (let [{:keys [name path size directory? last-modified
                  readable? writable? executable? hidden? parent]} info
          date-format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")]
      (with-out-str
        (println "File Information:")
        (println "===============================")
        (println "Name:" name)
        (println "Path:" path)
        (println "Type:" (if directory? "Directory" "File"))
        (when-not directory?
          (println "Size:" size "bytes"))
        (println "Last Modified:" (.format date-format (java.util.Date. last-modified)))
        (println "Parent Directory:" parent)
        (println "Permissions:"
                 (str (if readable? "r" "-")
                      (if writable? "w" "-")
                      (if executable? "x" "-")))
        (when hidden?
          (println "Hidden: Yes"))))))

(defn search-files
  "Search for files matching pattern in directory and subdirectories.
   
   Arguments:
   - dir: Root directory to start search
   - pattern: String pattern to match filenames against
   - exclude-patterns: Optional sequence of patterns to exclude
   
   Returns a map with :matches vector of matching file paths."
  [dir pattern & {:keys [exclude-patterns] :or {exclude-patterns []}}]
  (let [root (io/file dir)]
    (if (and (.exists root) (.isDirectory root))
      (let [pattern-re (re-pattern (str "(?i).*" pattern ".*"))
            exclude-res (mapv #(re-pattern (str "(?i).*" % ".*")) exclude-patterns)
            excluded? (fn [path]
                        (some #(re-matches % path) exclude-res))
            matches (atom [])]
        (letfn [(search [file]
                  (when (.isDirectory file)
                    (doseq [child (.listFiles file)]
                      (let [path (.getAbsolutePath child)]
                        (if (.isDirectory child)
                          (when-not (excluded? path)
                            (search child))
                          (when (and (re-matches pattern-re (.getName child))
                                     (not (excluded? path)))
                            (swap! matches conj path)))))))]
          (search root)
          {:matches (sort @matches)
           :count (count @matches)
           :search-root (.getAbsolutePath root)
           :pattern pattern}))
      {:error (str dir " is not a valid directory")})))
