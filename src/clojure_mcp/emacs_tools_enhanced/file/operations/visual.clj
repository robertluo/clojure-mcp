(ns clojure-mcp.emacs-tools-enhanced.file.operations.visual
  "Visual operations for files in Emacs.
   
   This namespace provides functions for visual operations on files in Emacs,
   such as highlighting and flashing."
  (:require [clojure.string :as str]
            [clojure-mcp.emacs-tools-enhanced.file.core :refer [with-file]]))

(defn flash-file
  "Flashes the file in Emacs to draw attention to it.
   Uses both overlay highlighting and modeline flashing.
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the path of the flashed file"
  [file-path & {:keys [duration] :or {duration 1.0}}]
  (let [result (with-file file-path 
                (format "(progn
                          ;; Highlight entire buffer
                          (let ((overlay (make-overlay (point-min) (point-max))))
                            (overlay-put overlay 'face 'secondary-selection)
                            (overlay-put overlay 'priority 100)
                            (run-with-timer %s nil 
                              (lambda () (delete-overlay overlay))))
                          ;; Flash modeline
                          (let ((bg (face-background 'mode-line)))
                            (set-face-background 'mode-line \"red\")
                            (run-with-timer 0.1 nil
                              (lambda ()
                                (set-face-background 'mode-line bg)))))"
                        duration))]
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
         :message [(str "Successfully flashed file: " file-path)]
         :content [(str file-path)]})
      
      ;; Otherwise
      :else
      {:success true
       :message [(str "File flashed: " file-path)]
       :content [(str file-path)]})))