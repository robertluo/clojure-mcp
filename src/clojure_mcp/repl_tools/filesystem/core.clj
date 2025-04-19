(ns clojure-mcp.repl-tools.filesystem.core
  "Core filesystem operations for Clojure REPL tools.
   Provides file listing, reading, searching, and information gathering functions."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import
   (java.nio.file FileVisitResult FileSystems Files Path Paths SimpleFileVisitor)
   (java.nio.file.attribute BasicFileAttributes)))

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

;; MOVED to clojure-mcp.tools.directory-tree.core
(defn directory-tree
  "Creates a recursive tree view of directory structure using simple indentation.
   
   Arguments:
   - path: Root directory path to start from
   - depth: Current depth level (used for indentation)
   - max-depth: Maximum depth to traverse (nil for unlimited)
   
   Returns a string representation of the directory tree with proper indentation.
   Filters out temporary files like Emacs backup files (ending with ~).
   Adds a \"...\" indicator when directories are truncated due to max-depth.
   
   @deprecated This function has been moved to clojure-mcp.tools.directory-tree.core"
  [path & {:keys [depth max-depth] :or {depth 0 max-depth nil}}]
  ;; Delegate to the new implementation to maintain backward compatibility
  (require 'clojure-mcp.tools.directory-tree.core)
  ((resolve 'clojure-mcp.tools.directory-tree.core/directory-tree)
   path :depth depth :max-depth max-depth))

(defn glob-files
  [dir pattern & {:keys [max-results] :or {max-results 1000}}]
  (let [start (System/currentTimeMillis)
        dir-file (io/file dir)]
    (if (and (.exists dir-file) (.isDirectory dir-file))
      (try
        (let [path (Paths/get (.getAbsolutePath dir-file) (into-array String []))
              matcher (.getPathMatcher (FileSystems/getDefault) (str "glob:" pattern))
              matches (atom [])
              truncated (atom false)]

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
                            (.matches matcher rel))
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
          {:error (.getMessage e)
           :durationMs (- (System/currentTimeMillis) start)}))
      {:error (str dir " is not a valid directory")
       :durationMs 0})))

(comment
  (let [cwd (System/getProperty "user.dir")]
    (glob-files
     cwd
     "**/top_level*.clj")))

