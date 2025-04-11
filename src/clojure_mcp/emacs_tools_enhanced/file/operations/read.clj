(ns clojure-mcp.emacs-tools-enhanced.file.operations.read
  "File reading operations for Emacs.
   
   This namespace provides functions for reading file content in Emacs."
  (:require [clojure.string :as str]
            [clojure-mcp.emacs-tools-enhanced.file.core :refer [emacs-eval success-result error-result]]))

(defn read-file
  "Reads the entire content of a file using Emacs.
   Opens the file in Emacs and retrieves its content without any user interaction.
   Uses special settings to avoid prompts and blocking behavior.
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the file content"
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
      (error-result result)
      (success-result [(str "Successfully read file: " file-path)]))))

(defn file-exists?
  "Checks if a file or directory exists using Emacs.
   
   Returns a map with:
   - :success - Always true (operation completed)
   - :message - Array of message strings about the operation
   - :content - Array containing a boolean indicating if the file exists"
  [file-path]
  (let [exists? (= "t" (emacs-eval (format "(if (file-exists-p \"%s\") \"t\" \"nil\")" 
                                          (str/replace file-path "\"" "\\\""))))]
    (success-result [(if exists?
                       "true"
                       "false")]
                    (if exists? 
                      (str "File exists: " file-path) 
                      (str "File does not exist: " file-path)))))

(defn format-file [{:keys [path exists content]}]
   (str "<file-content path='" path "' "
        "exists='" (pr-str exists) "' "
        (if exists
          (str "length='" (pr-str (count content)) "'>" content "</file-content>" )
          "/>")))

#_(format-file {:path "/src/file.clj" :exists false :content "Igot lots of stuff in me \n asdfa "})

(defn read-multiple-files
  "Reads the contents of multiple files simultaneously.
   
   This is more efficient than reading files one by one when you need to analyze 
   or compare multiple files. Each file's content is returned with its path as a reference.
   Failed reads for individual files won't stop the entire operation.
   
   Returns a map with:
   - :success - Always true (operation completed, even if individual files fail)
   - :message - Array of message strings about the operation
   - :content - Array containing maps with :path, :content, and :exists keys"
  [file-paths]
  (let [results (mapv (fn [path]
                        (let [exists-result (file-exists? path)
                              exists? (= "true" (first (:content exists-result)))
                              read-result (if exists?
                                            (read-file path)
                                            (error-result "File does not exist"))]
                          {:path path
                           :content (if exists? (first (:content read-result)) "")
                           :exists exists?}))
                      file-paths)]
    (success-result (map format-file results) (str "Read " (count file-paths) " files"))))

