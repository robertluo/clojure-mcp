(ns clojure-mcp.emacs-tools-enhanced.file.operations.read
  "File reading operations for Emacs.
   
   This namespace provides functions for reading file content in Emacs."
  (:require [clojure.string :as str]
            [clojure-mcp.emacs-tools-enhanced.file.core :refer [emacs-eval]]))

(defn read-file
  "Reads the entire content of a file using Emacs.
   Opens the file in Emacs and retrieves its content without any user interaction.
   Uses special settings to avoid prompts and blocking behavior."
  [file-path]
  (let [result (emacs-eval 
                (format "(condition-case err
                           (progn
                             (let ((auto-revert-verbose nil)  ;; No messages about reverting
                                   (revert-without-query t)   ;; Don't confirm reverting
                                   (inhibit-message t))       ;; Suppress messages
                               (find-file-noselect \"%s\" t)  ;; Use noselect to avoid displaying
                               (with-current-buffer (find-buffer-visiting \"%s\")
                                 (let ((modified-p (buffer-modified-p)))
                                   ;; Force revert without any user interaction
                                   (revert-buffer t t t)
                                   ;; Restore modified state if needed
                                   (set-buffer-modified-p modified-p)
                                   ;; Use buffer-substring-no-properties to avoid text properties
                                   (buffer-substring-no-properties (point-min) (point-max))))))
                           (error (format \"Error: %%s\" (error-message-string err))))"
                         (str/replace file-path "\"" "\\\"")
                         (str/replace file-path "\"" "\\\"")))]
    ;; Check if result starts with "Error:"
    (if (and result (str/starts-with? result "Error:"))
      result  ;; Return the error message
      result)))

(defn file-exists?
  "Checks if a file or directory exists using Emacs.
   Returns true if the path exists (either as a file or directory),
   otherwise returns false."
  [file-path]
  (= "t" (emacs-eval (format "(if (file-exists-p \"%s\") \"t\" \"nil\")" 
                            (str/replace file-path "\"" "\\\"")))))

(defn read-multiple-files
  "Reads the contents of multiple files simultaneously.
   Returns a sequence of maps with :path, :content, and :exists keys.
   
   This is more efficient than reading files one by one when you need to analyze 
   or compare multiple files. Each file's content is returned with its path as a reference.
   Failed reads for individual files won't stop the entire operation.
   
   Note: While the function checks if paths exist (including directories), it only
   attempts to read content from regular files. Directories will be marked as existing
   but will result in an error message in the content field."
  [file-paths]
  ;; Simple implementation using the existing functions
  (mapv (fn [path]
          {:path path
           :content (if (file-exists? path)
                     (read-file path)
                     "File does not exist")
           :exists (file-exists? path)})
        file-paths))
