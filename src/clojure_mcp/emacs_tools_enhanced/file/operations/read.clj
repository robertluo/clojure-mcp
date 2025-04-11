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
                                   (buffer-string)))))
                           (error (format \"Error: %%s\" (error-message-string err))))"
                         (str/replace file-path "\"" "\\\"")
                         (str/replace file-path "\"" "\\\"")))]
    ;; Check if result starts with "Error:"
    (if (and result (str/starts-with? result "Error:"))
      result  ;; Return the error message
      result)))

(defn file-exists?
  "Checks if a file exists using Emacs."
  [file-path]
  (= "t" (emacs-eval (format "(if (file-exists-p \"%s\") \"t\" \"nil\")" 
                            (str/replace file-path "\"" "\\\"")))))
