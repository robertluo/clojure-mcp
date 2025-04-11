(ns clojure-mcp.emacs-tools-enhanced.file.operations.manage
  "File management operations for Emacs.
   
   This namespace provides functions for managing files in Emacs (find, delete,
   rename, copy, etc.)."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure-mcp.emacs-tools-enhanced.file.core :refer [emacs-eval with-file]]))

(defn find-files
  "Finds files matching a pattern in a directory.
   Returns a list of full file paths."
  [directory pattern]
  (let [result (emacs-eval 
               (format "(let ((files nil))
                         (dolist (file (directory-files-recursively \"%s\" \"%s\" nil))
                           (push file files))
                         (mapconcat 'identity (nreverse files) \"\n\"))"
                       (str/replace directory "\"" "\\\"")
                       (str/replace pattern "\"" "\\\"")))]
    (when (not-empty result)
      (str/split result #"\n"))))

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
   Returns true if successful, false otherwise."
  [file-path & {:keys [no-confirm] :or {no-confirm false}}]
  (let [result (emacs-eval 
               (format "(condition-case err
                         (progn
                           (delete-file \"%s\" %s)
                           t)
                         (error nil))"
                       (str/replace file-path "\"" "\\\"")
                       (if no-confirm "t" "nil")))]
    (= result "t")))

(defn rename-file
  "Renames a file from old-path to new-path.
   Returns true if successful, false otherwise."
  [old-path new-path & {:keys [no-confirm] :or {no-confirm false}}]
  (let [result (emacs-eval 
               (format "(condition-case err
                         (progn
                           (rename-file \"%s\" \"%s\" %s)
                           t)
                         (error nil))"
                       (str/replace old-path "\"" "\\\"")
                       (str/replace new-path "\"" "\\\"")
                       (if no-confirm "t" "nil")))]
    (= result "t")))

(defn copy-file
  "Copies a file from source-path to dest-path.
   Returns true if successful, false otherwise."
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
    (= result "t")))

(defn create-directory
  "Creates a new directory or ensures it exists.
   Creates parent directories if needed.
   Succeeds silently if directory already exists.
   Returns a success or error message string."
  [path]
  (try
    (let [dir (io/file path)
          existed (.exists dir)
          result (.mkdirs dir)]
      (cond
        existed (format "Directory already exists: %s" path)
        result (format "Successfully created directory: %s" path)
        :else (format "Failed to create directory: %s" path)))
    (catch Exception e
      (format "Error creating directory: %s - %s" path (.getMessage e)))))

(defn list-directory
  "Lists directory contents with [FILE] or [DIR] prefixes.
   Returns a formatted string with one item per line.
   Returns an error message if directory doesn't exist or an error occurs."
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
