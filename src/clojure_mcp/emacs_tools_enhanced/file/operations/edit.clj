(ns clojure-mcp.emacs-tools-enhanced.file.operations.edit
  "File editing operations for Emacs.
   
   This namespace provides functions for editing file content in Emacs."
  (:require [clojure.string :as str]
            [clojure-mcp.emacs-tools-enhanced.file.core :refer [emacs-eval with-file]]))

(defn dry-run-edits
  "Generates a diff preview of the changes that would be made by the edits.
   
   Parameters:
   - file-path: Path to the file to edit
   - edits: Sequence of maps with :old-text and :new-text keys
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the diff output"
  [file-path edits]
  (let [elisp-code (format 
                   "(progn
                      (let ((diff-output \"\"))
                        (with-temp-buffer
                          (insert-file-contents \"%s\")
                          (let ((orig-content (buffer-string)))
                            (dolist (edit '(%s))
                              (let ((old-text (car edit))
                                    (new-text (cdr edit)))
                                (goto-char (point-min))
                                (while (search-forward old-text nil t)
                                  (replace-match new-text nil t))))
                            
                            (let ((modified-content (buffer-string)))
                              (with-temp-buffer
                                (insert orig-content)
                                (let ((orig-buf (current-buffer)))
                                  (with-temp-buffer
                                    (insert modified-content)
                                    (let ((modified-buf (current-buffer)))
                                      (diff-buffers orig-buf modified-buf nil 'noasync)
                                      (with-current-buffer \"*Diff*\"
                                        (setq diff-output (buffer-string))))))))))
                        diff-output))"
                   (str/replace file-path "\"" "\\\"")
                   (str/join " " 
                           (for [{:keys [old-text new-text]} edits]
                             (when (and old-text new-text)
                               (format "(\"%s\" . \"%s\")"
                                     (str/replace old-text "\"" "\\\"") 
                                     (str/replace new-text "\"" "\\\""))))))
        diff-result (emacs-eval elisp-code)]
    {:success true
     :message ["Dry run completed"]
     :content [diff-result]}))

(defn edit-file
  "Makes multiple text replacements in a file with optional visual highlighting.
   
   Takes a sequence of edits, where each edit is a map with :old-text and :new-text keys.
   Applies edits sequentially in the order provided.
   
   Options:
   - highlight-duration: Duration of the highlight effect in seconds (default: 2.0)
   - dry-run: Preview changes using git-style diff format (default: false)
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing results data (if any)"
  [file-path edits & {:keys [highlight-duration dry-run] 
                     :or {highlight-duration 2.0 dry-run false}}]
  (if (empty? edits)
    {:success true, :message ["No edits to apply"], :content []}
    (if dry-run
      ;; For dry-run, use our dry-run-edits helper
      (dry-run-edits file-path edits)
      ;; For actual edits, apply one by one using elisp directly
      (let [elisp-code (format 
                      "(progn
                         (let ((total-changes 0))
                           %s
                           (save-buffer)
                           (format \"Applied %%d changes\" total-changes)))"
                      (str/join "\n" 
                              (map (fn [{:keys [old-text new-text]}]
                                     (format "
                                     (save-excursion
                                       (goto-char (point-min))
                                       (let ((count 0))
                                         (while (search-forward \"%s\" nil t)
                                           (setq count (1+ count))
                                           (let ((start (match-beginning 0)))
                                             (replace-match \"%s\" nil t)
                                             (let ((overlay (make-overlay start (point))))
                                               (overlay-put overlay 'face 'highlight)
                                               (overlay-put overlay 'priority 100)
                                               (run-with-timer %s nil 
                                                 (lambda () (delete-overlay overlay))))))
                                         (setq total-changes (+ total-changes count))))"
                                           (str/replace old-text "\"" "\\\"")
                                           (str/replace new-text "\"" "\\\"")
                                           highlight-duration))
                                   (filter (fn [{:keys [old-text new-text]}]
                                            (and old-text new-text)) 
                                           edits))))
            result (with-file file-path elisp-code)]
        (cond
          ;; If result is already a map with :success key, normalize it
          (and (map? result) (contains? result :success))
          (-> result
              (update :message #(if (string? %) [%] (or % [])))
              (assoc :content (if (contains? result :value)
                               (if (sequential? (:value result))
                                 (mapv str (:value result))
                                 [(str (:value result))])
                               [])))
          
          ;; If result is a string
          (string? result)
          (if (str/starts-with? result "Error:")
            {:success false
             :message [result]
             :content []}
            {:success true
             :message [(if (re-find #"^Applied \d+ changes$" result)
                         result
                         "Edit operation completed")]
             :content [result]})
          
          ;; Otherwise
          :else
          {:success true
           :message ["Edit operation completed"]
           :content [(str result)]})))))

