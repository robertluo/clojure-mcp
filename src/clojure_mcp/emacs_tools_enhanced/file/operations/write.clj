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
   - flash: Whether to flash the modeline (default: true)"
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
    result))

(defn append-to-file
  "Appends content to the end of a file with optional highlighting.
   
   Options:
   - highlight-duration: Duration of the highlight effect in seconds (default: 2.0)"
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
    result))

(defn save-file
  "Saves the file if it's open in a buffer."
  [file-path]
  (let [result (with-file file-path "(save-buffer)")]
    result))
