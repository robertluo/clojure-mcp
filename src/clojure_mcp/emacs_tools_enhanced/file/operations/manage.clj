(ns clojure-mcp.emacs-tools-enhanced.file.operations.manage
  "File management operations for Emacs.
   
   This namespace provides functions for managing files in Emacs (find, delete,
   rename, copy, etc.)."
  (:require [clojure.string :as str]
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
