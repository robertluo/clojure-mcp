(ns clojure-mcp.emacs-tools-enhanced.file.operations.edit
  "File editing operations for Emacs.
   
   This namespace provides functions for editing file content in Emacs."
  (:require [clojure.string :as str]
            [clojure-mcp.emacs-tools-enhanced.file.core :refer [emacs-eval with-file error-result success-result]]))

(defn- generate-edit-code
  "Generates the Elisp code for applying edits.
   
   Parameters:
   - edits: Sequence of maps with :old-text and :new-text keys
   - with-highlighting: Whether to include highlighting code (default: false)
   - highlight-duration: Duration of the highlight effect in seconds (default: 2.0)
   
   Returns a string containing Elisp code that applies the edits."
  [edits & {:keys [with-highlighting highlight-duration] 
            :or {with-highlighting false highlight-duration 2.0}}]
  (str/join "\n" 
           (map (fn [{:keys [old-text new-text]}]
                  (format 
                   "(save-excursion
                      (goto-char (point-min))
                      (let ((count 0)
                            (found nil))
                        (while (search-forward \"%s\" nil t)
                          (setq count (1+ count))
                          (setq found t)
                          (let ((start (match-beginning 0)))
                            (replace-match \"%s\" nil t)
                            %s))
                        (when found
                          (setq applied-edits (1+ applied-edits)))
                        (setq total-changes (+ total-changes count))))"
                   (str/replace old-text "\"" "\\\"")
                   (str/replace new-text "\"" "\\\"")
                   (if with-highlighting
                     (format "(let ((overlay (make-overlay start (point))))
                               (overlay-put overlay 'face 'highlight)
                               (overlay-put overlay 'priority 100)
                               (run-with-timer %s nil 
                                 (lambda () (delete-overlay overlay))))" 
                             highlight-duration)
                     "")))
                (filter (fn [{:keys [old-text new-text]}]
                         (and old-text new-text)) 
                        edits))))

(defn dry-run-edits
  "Generates a diff preview of the changes that would be made by the edits.
   Uses the same search-and-replace logic as the actual edit function.
   Runs in the background without disturbing the current buffer focus.
   
   Parameters:
   - file-path: Path to the file to edit
   - edits: Sequence of maps with :old-text and :new-text keys
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the diff output"
  [file-path edits]
  (let [edit-code (generate-edit-code edits)
        elisp-code (format 
                   "(progn
                      (let ((diff-output \"\")
                            (applied-edits 0)
                            (total-changes 0))
                        (with-temp-buffer
                          (insert-file-contents \"%s\")
                          (let ((orig-content (buffer-string)))
                            
                            ;; Apply edits using the shared code
                            %s
                            
                            (let ((modified-content (buffer-string)))
                              ;; Create temporary files for diff
                              (let ((temp-orig (make-temp-file \"orig-content\"))
                                    (temp-mod (make-temp-file \"modified-content\")))
                                (with-temp-file temp-orig
                                  (insert orig-content))
                                (with-temp-file temp-mod
                                  (insert modified-content))
                                
                                ;; Generate diff using external diff command
                                (setq diff-output 
                                      (shell-command-to-string 
                                        (format \"diff -u %%s %%s\" temp-orig temp-mod)))
                                
                                ;; Clean up temp files
                                (delete-file temp-orig)
                                (delete-file temp-mod)))))
                        
                        ;; Return the diff
                        diff-output))"
                   (str/replace file-path "\"" "\\\"")
                   edit-code)
        diff-result (emacs-eval elisp-code)]
    (success-result [diff-result] "Dry run completed")))

(defn edit-file
  "Makes multiple text replacements in a file with enhanced feedback.
   
   Takes a sequence of edits, where each edit is a map with :old-text and :new-text keys.
   Applies edits sequentially in the order provided.
   Always returns diff information to verify which edits were applied.
   Always reverts the buffer first to avoid conflicts with files changed on disk.
   
   Options:
   - highlight-duration: Duration of the highlight effect in seconds (default: 2.0)
   - dry-run: Preview changes using git-style diff format (default: false)
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing diff and results data
   - :applied-edits - Number of edits that were successfully applied
   - :total-changes - Total number of changes made (may be higher than applied-edits if text appears multiple times)"
  [file-path edits & {:keys [highlight-duration dry-run] 
                     :or {highlight-duration 2.0 dry-run false}}]
  (if (empty? edits)
    (success-result [] "No edits to apply")
    (let [;; Always generate a diff first for reference
          diff-result (dry-run-edits file-path edits)]
      (if dry-run
        ;; For dry-run, just return the diff
        diff-result
        ;; For actual edits, apply and then return with diff
        (let [edit-code (generate-edit-code edits 
                                           :with-highlighting true 
                                           :highlight-duration highlight-duration)
              elisp-code (format 
                        "(progn
                           (let ((applied-edits 0)
                                 (total-changes 0))
                             %s
                             (save-buffer)
                             (format \"Applied %%d of %%d edits with %%d total changes\" 
                                     applied-edits %d total-changes)))"
                        edit-code
                        (count (filter #(and (:old-text %) (:new-text %)) edits)))
              
              ;; Use with-file with revert-first strategy to avoid disk conflicts
              result (with-file file-path elisp-code :conflict-strategy :revert-first)
              
              processed-result (cond
                ;; If result is a map with :success key and it succeeded
                (and (map? result) 
                     (contains? result :success) 
                     (:success result))
                (let [result-text (first (:content result))
                      ;; Extract the applied-edits count using regex
                      applied-regex (re-find #"Applied (\d+) of \d+ edits with (\d+) total changes" result-text)
                      applied-count (if applied-regex
                                     (Integer/parseInt (second applied-regex))
                                     0)
                      total-changes (if applied-regex
                                    (Integer/parseInt (nth applied-regex 2))
                                    0)]
                  {:success true
                   :message [result-text]
                   :content (:content diff-result)  ;; Use the diff as content
                   :applied-edits applied-count
                   :total-changes total-changes})
                
                ;; If result is a map with :success key but it failed
                (and (map? result) (contains? result :success))
                (assoc result 
                       :applied-edits 0
                       :total-changes 0)
                
                ;; Otherwise create an error result
                :else
                (error-result (str "Unexpected error while applying edits: " result)))]
          
          ;; Return the processed result
          processed-result)))))

(comment
  (edit-file "/tmp/clojure-test-files/fibonacci.clj"
             [{:old-text "(defn fibonacci-seq\n"
               :new-text "(defn fibonacci-seq-farmer\n"}])
  )
