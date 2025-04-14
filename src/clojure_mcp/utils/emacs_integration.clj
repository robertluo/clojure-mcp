(ns clojure-mcp.utils.emacs-integration
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]))

(defn emacs-eval
  "Evaluates elisp code in Emacs using emacsclient with improved error handling.
   
   Arguments:
   - elisp-code: String containing Emacs Lisp code to be evaluated
   
   Returns:
   - On success: The result of evaluation with quotes stripped
   - On failure: A map with :error and :message keys describing the failure"
  [elisp-code]
  (try
    (let [result (shell/sh "emacsclient" "--eval" elisp-code)]
      (if (zero? (:exit result))
        ;; Success case - strip quotes and return result
        (-> (:out result)
            (str/trim)
            (str/replace #"^\"" "")
            (str/replace #"\"$" ""))
        ;; Non-zero exit case
        {:error :emacs-error
         :message (format "Emacs client returned error code %d: %s" 
                          (:exit result) 
                          (or (not-empty (:err result)) (:out result)))}))
    (catch Exception e
      {:error :execution-error
       :message (format "Failed to execute emacsclient: %s" (.getMessage e))})))


#_"(let ((overlay (make-overlay start (point))))
                               (overlay-put overlay 'face 'highlight)
                               (overlay-put overlay 'priority 100)
                               (run-with-timer %s nil 
                                 (lambda () (delete-overlay overlay))))"

(defn highlight-region
  "Highlights a region in a file in Emacs temporarily.
   
   Arguments:
   - file-path: Path to the file
   - start: Start position
   - end: End position
   - duration: (optional) How long to show the highlight in seconds (default: 2 seconds)
   
   Notes:
   - This function enables auto-revert-mode in the visited buffer to ensure
     changes made externally are automatically reflected
   
   Returns:
   - On success: true
   - On failure: Map with :error and :message keys"
  ([file-path start end]
   (highlight-region file-path start end 2.0)) ; Default to 2 seconds
  ([file-path start end duration]
   (let [highlight-code (format "(let ((overlay (make-overlay %d %d)))
                                  (overlay-put overlay 'face 'highlight)
                                  (overlay-put overlay 'priority 100)
                                  (run-with-timer %s nil 
                                    (lambda () (delete-overlay overlay))))"
                                start end (float duration))
         elisp-code (format "(progn
                               (find-file \"%s\")
                               (switch-to-buffer (current-buffer))
                               (auto-revert-mode 1)
                               (let ((revert-without-query '(\".*\")))
                                 (revert-buffer t t t))
                               (goto-char %d)
                               %s
                               t)"
                             (str/replace file-path #"\\" "\\\\") ; Escape backslashes
                             start
                             highlight-code)
         result (emacs-eval elisp-code)]
     (if (map? result)
       ;; Error was returned
       result
       ;; Success
       true))))

(defn ensure-auto-revert
  "Ensures that a file is open in Emacs with auto-revert-mode enabled.
   This helps Emacs automatically refresh buffers when files are modified externally.
   
   Arguments:
   - file-path: Path to the file to open and configure
   
   Notes:
   - This function finds or opens the file in Emacs
   - Enables auto-revert-mode for the buffer
   - Configures revert-without-query to avoid prompts
   - Performs an immediate revert to ensure the buffer is up-to-date
   
   Returns:
   - On success: true
   - On failure: Map with :error and :message keys"
  [file-path]
  (let [elisp-code (format "(progn
                             (find-file \"%s\")
                             (switch-to-buffer (current-buffer))
                             (auto-revert-mode 1)
                             (let ((revert-without-query '(\".*\")))
                               (revert-buffer t t t))
                             t)"
                           (str/replace file-path #"\\" "\\\\"))
        result (emacs-eval elisp-code)]
    (if (map? result)
      ;; Error was returned
      result
      ;; Success
      true)))

 (defn temporary-highlight
  "Alias for highlight-region for backwards compatibility.
   
   Arguments:
   - file-path: Path to the file
   - start: Start position
   - end: End position
   - duration: How long to show the highlight in seconds
   
   See highlight-region for more details."
  [file-path start end duration]
  (highlight-region file-path start end duration))

;; Example usage
(comment
  ;; Evaluate an Emacs command
  (emacs-eval "(+ 1 2 3)")
  
  ;; Ensure auto-revert is enabled for a file
  (ensure-auto-revert "/path/to/file.clj")
  
  ;; Highlight a form for 2 seconds (default)
  ;; This will also enable auto-revert-mode in the buffer
  (highlight-region "/path/to/file.clj" 12 53)
  
  ;; Highlight a form for 5 seconds
  ;; This will also enable auto-revert-mode in the buffer
  (highlight-region "/path/to/file.clj" 12 53 5)
  
  ;; Using the temporary-highlight alias (same behavior as highlight-region)
  (temporary-highlight "/path/to/file.clj" 12 53 2)
)