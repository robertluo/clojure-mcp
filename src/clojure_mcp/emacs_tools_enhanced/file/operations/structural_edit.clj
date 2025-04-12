(ns clojure-mcp.emacs-tools-enhanced.file.operations.structural-edit
  "Structural editing operations for Clojure code files in Emacs.
   
   This namespace provides operations that understand and manipulate the
   structure of Clojure code, rather than treating it as plain text.
   Functions here leverage Emacs' structural editing capabilities to
   make precise changes to code while respecting its form."
  (:require [clojure.string :as str]
            [clojure-mcp.emacs-tools-enhanced.file.core :refer [emacs-eval with-file error-result success-result]]))

;; -------------------------------------------------------------------------
;; Top-level Form Editing
;; -------------------------------------------------------------------------

(defn top-level-form-edit-dry-run
  "Generates a diff preview of the changes that would be made by the form edit.
   
   Parameters:
   - file-path: Path to the file containing the form
   - form-prefix: A unique prefix (first line or two) that identifies the form
   - new-implementation: String with the new form implementation
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - Array containing the diff output"
  [file-path form-prefix new-implementation]
  (let [elisp-code (format 
                  "(progn
                     (let ((diff-output \"\")
                           (found nil))
                       (with-temp-buffer
                         (insert-file-contents \"%s\")
                         (let ((orig-content (buffer-string)))
                           
                           ;; Try to find and replace the form
                           (goto-char (point-min))
                           (if (search-forward \"%s\" nil t)
                               (let ((original-pos (point)))
                                 ;; Move to start of line where we found the match
                                 (beginning-of-line)
                                 (let ((line-start-pos (point)))
                                   ;; Move back to where we found the text
                                   (goto-char original-pos)
                                   
                                   ;; Search backwards for the opening paren
                                   (while (and (> (point) line-start-pos)
                                             (not (eq (char-before) ?\\()))
                                     (backward-char 1))
                                   
                                   ;; Go back one more to include the paren
                                   (when (eq (char-before) ?\\()
                                     (backward-char 1))
                                   
                                   ;; This is the start of our form
                                   (let ((form-start (point)))
                                     ;; Now capture to the end of the form
                                     (forward-sexp 1)
                                     (let ((form-end (point)))
                                       (delete-region form-start form-end)
                                       (goto-char form-start)
                                       (insert \"%s\")
                                       (ignore-errors (indent-region form-start (point)))
                                       (setq found t)))))
                             (setq found nil))
                           
                           (if found
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
                                   (delete-file temp-mod)))
                             (setq diff-output \"Form not found\"))))
                       
                       ;; Return the diff
                       diff-output))"
                  (str/replace file-path "\"" "\\\"")
                  (str/replace form-prefix "\"" "\\\"")
                  (str/replace new-implementation "\"" "\\\""))
        diff-result (emacs-eval elisp-code)]
    
    (if (= diff-result "Form not found")
      (error-result "Form not found")
      (success-result [diff-result] "Dry run completed"))))

(defn top-level-form-edit
  "Edits any top-level form in a Clojure file, replacing it with a new implementation.
   
   This function allows you to modify any top-level form (def, defn, ns, deftest etc.) 
   in source files by providing a prefix (part of the form content) to identify it.
   It preserves formatting and whitespace in the rest of the file.
   
   The prefix should be part of the actual form after the opening parenthesis,
   and does not need to include any metadata that appears before the form.
   
   Parameters:
   - file-path: Path to the file containing the form
   - form-prefix: A unique prefix that identifies the form (without metadata)
   - new-implementation: String with the new form implementation
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Array of message strings about the operation
   - :content - (for dry-run only) Array containing the diff output"
  [file-path form-prefix new-implementation & {:keys [dry-run] :or {dry-run false}}]
  (if dry-run
    (top-level-form-edit-dry-run file-path form-prefix new-implementation)
    (let [elisp-code (format 
                     "(progn
                       (goto-char (point-min))
                       (if (search-forward \"%s\" nil t)
                           (let ((original-pos (point)))
                             ;; Move to start of line where we found the match
                             (beginning-of-line)
                             (let ((line-start-pos (point)))
                               ;; Move back to where we found the text
                               (goto-char original-pos)
                               
                               ;; Search backwards for the opening paren
                               (while (and (> (point) line-start-pos)
                                         (not (eq (char-before) ?\\()))
                                 (backward-char 1))
                               
                               ;; Go back one more to include the paren
                               (when (eq (char-before) ?\\()
                                 (backward-char 1))
                               
                               ;; This is the start of our form
                               (let ((form-start (point)))
                                 ;; Now capture to the end of the form
                                 (forward-sexp 1)
                                 (let ((form-end (point)))
                                   (delete-region form-start form-end)
                                   (goto-char form-start)
                                   (insert \"%s\")
                                   (ignore-errors (paredit-reindent-defun))
                                   (ignore-errors (indent-region form-start (point)))
                                   (save-buffer)
                                   \"Form successfully replaced\"))))
                         \"Form not found\"))"
                     (str/replace form-prefix "\"" "\\\"")
                     (str/replace new-implementation "\"" "\\\""))
          result (with-file file-path elisp-code :conflict-strategy :revert-first)]
      
      (if (:success result)
        (let [message (first (:content result))]
          (if (= message "Form successfully replaced")
            (success-result [] "Top-level form successfully replaced")
            (error-result "Top-level form not found")))
        result))))

(comment
  ;; Example usage:
  
  ;; Edit a top-level form
  (top-level-form-edit 
   "/tmp/clojure-test-files/core.clj"
   "(defn fibonacci-seq"  ;; Prefix to identify the form (no need to include metadata)
   "(defn fibonacci-seq
      \"Returns a lazy sequence of Fibonacci numbers\"
      ([]
       (fibonacci-seq 0 1))
      ([a b]
       (lazy-seq
        (cons a (fibonacci-seq b (+ a b))))))")  ;; New implementation
  
  ;; Preview changes with dry-run
  (top-level-form-edit 
   "/tmp/clojure-test-files/core.clj"
   "(defn fibonacci-seq"
   "(defn fibonacci-seq-updated
      \"Updated implementation\"
      [n]
      (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1]))))"
   :dry-run true)
  
  ;; For a form with metadata, we only need to match part of the form itself
  (top-level-form-edit
   "/tmp/clojure-test-files/core.clj"
   "(defn old-function"  ;; No need to include the ^:deprecated metadata
   "(defn old-function
      \"Updated deprecated function\"
      [x]
      (inc x))")
  )
