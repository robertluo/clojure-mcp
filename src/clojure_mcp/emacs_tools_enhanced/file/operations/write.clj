(ns clojure-mcp.emacs-tools-enhanced.file.operations.write
  "File writing operations for Emacs.
   
   This namespace provides functions for writing content to files in Emacs."
  (:require [clojure.string :as str]
            [clojure-mcp.emacs-tools-enhanced.file.core :refer [with-file]]))

(defn write-file
  "Writes content to a file, handling both new and existing files.
   If the file exists and overwrite is false, returns an error message.
   Otherwise, creates or overwrites the file with the given content.
   
   Options:
   - overwrite: Whether to overwrite existing files (default: true)
   - highlight: Whether to highlight changes in the buffer (default: true)
   - flash: Whether to flash the modeline (default: true)
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the path of the written file"
  [file-path content & {:keys [overwrite highlight flash] 
                      :or {overwrite true highlight true flash true}}]
  (let [result (with-file file-path 
                (format "(progn
                          %s
                          (erase-buffer)
                          (insert \"%s\")
                          (save-buffer)
                          %s
                          %s)"
                        (if overwrite
                          ""
                          "(when (file-exists-p (buffer-file-name))
                             (error \"File exists and overwrite is not enabled\"))")  
                        (str/replace content "\"" "\\\"")
                        (if highlight
                          "(let ((overlay (make-overlay (point-min) (point-max))))
                             (overlay-put overlay 'face 'highlight)
                             (overlay-put overlay 'priority 100)
                             (run-with-timer 2.0 nil 
                               (lambda () (delete-overlay overlay))))"
                          "")
                        (if flash
                          "(let ((bg (face-background 'mode-line)))
                             (set-face-background 'mode-line \"red\")
                             (run-with-timer 0.1 nil
                               (lambda ()
                                 (set-face-background 'mode-line bg))))"
                          "")))]
    (cond
      ;; If result is already a map with :success key, normalize it
      (and (map? result) (contains? result :success))
      (-> result
          (update :message #(if (string? %) [%] (or % [])))
          (assoc :content [(str file-path)]))
      
      ;; If result is a string
      (string? result)
      (if (str/starts-with? result "Error:")
        {:success false
         :message [result]
         :content []}
        {:success true
         :message [(str "Successfully wrote to file: " file-path)]
         :content [(str file-path)]})
      
      ;; Otherwise
      :else
      {:success true
       :message [(str "File written: " file-path)]
       :content [(str file-path)]})))

(defn append-to-file
  "Appends content to the end of a file with optional highlighting.
   
   Options:
   - highlight-duration: Duration of the highlight effect in seconds (default: 2.0)
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the path of the file that was appended to"
  [file-path content & {:keys [highlight-duration] :or {highlight-duration 2.0}}]
  (let [result (with-file file-path 
                (format "(progn
                          (goto-char (point-max))
                          (let ((start (point)))
                            (insert \"\n%s\")
                            (let ((overlay (make-overlay start (point-max))))
                              (overlay-put overlay 'face 'highlight)
                              (overlay-put overlay 'priority 100)
                              (run-with-timer %s nil 
                                (lambda () (delete-overlay overlay)))))
                          (save-buffer))"
                        (-> content
                            (str/replace "\\" "\\\\")
                            (str/replace "\"" "\\\"")
                            (str/replace "\n" "\\n"))
                        highlight-duration))]
    (cond
      ;; If result is already a map with :success key, normalize it
      (and (map? result) (contains? result :success))
      (-> result
          (update :message #(if (string? %) [%] (or % [])))
          (assoc :content [(str file-path)]))
      
      ;; If result is a string
      (string? result)
      (if (str/starts-with? result "Error:")
        {:success false
         :message [result]
         :content []}
        {:success true
         :message [(str "Successfully appended to file: " file-path)]
         :content [(str file-path)]})
      
      ;; Otherwise
      :else
      {:success true
       :message [(str "Appended to file: " file-path)]
       :content [(str file-path)]})))

(defn save-file
  "Saves the file if it's open in a buffer.
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the path of the saved file"
  [file-path]
  (let [result (with-file file-path "(save-buffer)")]
    (cond
      ;; If result is already a map with :success key, normalize it
      (and (map? result) (contains? result :success))
      (-> result
          (update :message #(if (string? %) [%] (or % [])))
          (assoc :content [(str file-path)]))
      
      ;; If result is a string
      (string? result)
      (if (str/starts-with? result "Error:")
        {:success false
         :message [result]
         :content []}
        {:success true
         :message [(str "Successfully saved file: " file-path)]
         :content [(str file-path)]})
      
      ;; Otherwise
      :else
      {:success true
       :message [(str "File saved: " file-path)]
       :content [(str file-path)]})))