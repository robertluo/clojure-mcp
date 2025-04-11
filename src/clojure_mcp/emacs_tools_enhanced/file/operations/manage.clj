(ns clojure-mcp.emacs-tools-enhanced.file.operations.manage
  "File management operations for Emacs.
   
   This namespace provides functions for managing files in Emacs (find, delete,
   rename, copy, etc.)."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure-mcp.emacs-tools-enhanced.file.core :refer [emacs-eval with-file]])
  (:import [java.nio.file Files Path Paths LinkOption FileSystems]
           [java.nio.file.attribute BasicFileAttributes PosixFileAttributes PosixFilePermissions]))

(defn find-files
  "Finds files matching a pattern in a directory.
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the matching file paths"
  [directory pattern]
  (let [result (emacs-eval 
               (format "(let ((files nil))
                         (dolist (file (directory-files-recursively \"%s\" \"%s\" nil))
                           (push file files))
                         (mapconcat 'identity (nreverse files) \"\n\"))"
                       (str/replace directory "\"" "\\\"")
                       (str/replace pattern "\"" "\\\"")))]
    (if (or (not result) (str/starts-with? result "Error:"))
      {:success false
       :message [(or result "No files found")]
       :content []}
      (let [files (if (not-empty result)
                    (str/split result #"\n")
                    [])]
        {:success true
         :message [(str "Found " (count files) " files matching \"" pattern "\" in " directory)]
         :content files}))))

(defn with-files
  "Applies a function to a collection of files.
   The function should take a file path as its first argument.
   Returns a map of file paths to results."
  [file-paths f & args]
  (into {} 
        (map (fn [path] 
               [path (apply f path args)])
             file-paths)))

(defn delete-file
  "Deletes a file with confirmation in Emacs.
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the path of the deleted file (if successful)"
  [file-path & {:keys [no-confirm] :or {no-confirm false}}]
  (let [result (emacs-eval 
               (format "(condition-case err
                         (progn
                           (delete-file \"%s\" %s)
                           t)
                         (error nil))"
                       (str/replace file-path "\"" "\\\"")
                       (if no-confirm "t" "nil")))]
    (let [success? (= result "t")]
      (if success?
        {:success true
         :message [(str "Successfully deleted file: " file-path)]
         :content [(str file-path)]}
        {:success false
         :message [(str "Failed to delete file: " file-path)]
         :content []}))))

(defn move-file
  "Moves or renames files and directories.
   Takes source and destination paths.
   Fails if the destination already exists.
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the old and new file paths (if successful)"
  [source destination]
  (try
    (let [src-file (io/file source)
          dest-file (io/file destination)]
      (cond
        (not (.exists src-file))
        {:success false
         :message [(str "Error: Source does not exist: " source)]
         :content []}
        
        (.exists dest-file)
        {:success false
         :message [(str "Error: Destination already exists: " destination)]
         :content []}
        
        :else
        (if (.renameTo src-file dest-file)
          {:success true
           :message [(str "Successfully moved " source " to " destination)]
           :content [source, destination]}
          {:success false
           :message [(str "Failed to move " source " to " destination)]
           :content []})))
    (catch Exception e
      {:success false
       :message [(str "Error moving file: " source " - " (.getMessage e))]
       :content []})))

(defn copy-file
  "Copies a file from source-path to dest-path.
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the source and destination paths (if successful)"
  [source-path dest-path & {:keys [no-confirm] :or {no-confirm false}}]
  (let [result (emacs-eval 
               (format "(condition-case err
                         (progn
                           (copy-file \"%s\" \"%s\" %s)
                           t)
                         (error nil))"
                       (str/replace source-path "\"" "\\\"")
                       (str/replace dest-path "\"" "\\\"")
                       (if no-confirm "t" "nil")))]
    (let [success? (= result "t")]
      (if success?
        {:success true
         :message [(str "Successfully copied file from " source-path " to " dest-path)]
         :content [source-path, dest-path]}
        {:success false
         :message [(str "Failed to copy file from " source-path " to " dest-path)]
         :content []}))))

(defn create-directory
  "Creates a new directory or ensures it exists.
   Creates parent directories if needed.
   Succeeds silently if directory already exists.
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the path of the created directory (if successful)"
  [path]
  (try
    (let [dir (io/file path)
          existed (.exists dir)
          result (.mkdirs dir)]
      (cond
        existed 
        {:success true
         :message [(str "Directory already exists: " path)]
         :content [(str path)]}
        
        result 
        {:success true
         :message [(str "Successfully created directory: " path)]
         :content [(str path)]}
        
        :else 
        {:success false
         :message [(str "Failed to create directory: " path)]
         :content []}))
    (catch Exception e
      {:success false
       :message [(str "Error creating directory: " path " - " (.getMessage e))]
       :content []})))

(defn list-directory
  "Lists directory contents with [FILE] or [DIR] prefixes.
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing directory entries with [FILE] or [DIR] prefixes"
  [path]
  (try
    (let [dir (io/file path)]
      (if (and (.exists dir) (.isDirectory dir))
        (let [files (.listFiles dir)
              sorted-files (sort-by #(.getName %) files)
              lines (for [f sorted-files]
                      (format "%s %s" 
                              (if (.isDirectory f) "[DIR]" "[FILE]")
                              (.getPath f)))]
          (if (seq lines)
            (str/join "\n" lines)
            (format "Directory is empty: %s" path)))
        (format "Path does not exist or is not a directory: %s" path)))
    (catch Exception e
      (format "Error listing directory: %s - %s" path (.getMessage e)))))

(defn get-file-info
  "Gets detailed metadata about a file or directory.
   Returns a formatted string with the file's metadata.
   Provides information about size, creation time, modification time,
   access time, type (file/directory), and permissions (if available).
   Returns an error message if the file doesn't exist or an error occurs."
  [path]
  (try
    (let [file (io/file path)]
      (if (.exists file)
        (let [path-obj (Paths/get (.toURI file))
              attrs (Files/readAttributes path-obj BasicFileAttributes (into-array LinkOption []))
              is-directory (.isDirectory file)
              file-size (.size attrs)
              creation-time (.toMillis (.creationTime attrs))
              last-modified (.toMillis (.lastModifiedTime attrs))
              last-access (.toMillis (.lastAccessTime attrs))
              
              ;; Try to get POSIX permissions if supported
              posix-attrs (try
                            (Files/readAttributes path-obj PosixFileAttributes (into-array LinkOption []))
                            (catch UnsupportedOperationException _ nil))
              
              permissions (if posix-attrs 
                            (PosixFilePermissions/toString (.permissions posix-attrs))
                            "Not available")
              
              lines ["File Information:"
                     (format "Path: %s" path)
                     (format "Type: %s" (if is-directory "Directory" "File"))
                     (format "Size: %d bytes" file-size)
                     (format "Created: %s" (java.util.Date. creation-time))
                     (format "Modified: %s" (java.util.Date. last-modified))
                     (format "Accessed: %s" (java.util.Date. last-access))
                     (format "Permissions: %s" permissions)]]
          {:success true
           :content lines
           :message []})
        {:success false
         :content []
         :message [(format "Error: Path does not exist: %s" path)]}))
    (catch Exception e
      {:success false
       :content []
       :message [(format "Error getting file info: %s - %s" path (.getMessage e))]})))
