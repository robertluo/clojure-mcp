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
