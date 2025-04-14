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
  "Highlights a region in a file in Emacs.
   
   Arguments:
   - file-path: Path to the file
   - start: Start position
   - end: End position
   - duration: (optional) How long to show the highlight in seconds (default: permanent)
   
   Notes:
   - This function enables auto-revert-mode in the visited buffer to ensure
     changes made externally are automatically reflected
   
   Returns:
   - On success: true
   - On failure: Map with :error and :message keys"
  ([file-path start end]
   (highlight-region file-path start end nil))
  ([file-path start end duration]
   (let [highlight-code (if duration
                          ;; Duration overlay with timer
                          (format "(let ((overlay (make-overlay %d %d)))
                                    (overlay-put overlay 'face 'highlight)
                                    (overlay-put overlay 'priority 100)
                                    (run-with-timer %s nil 
                                      (lambda () (delete-overlay overlay))))"
                                  start end (float duration))
                          ;; Permanent highlighting using mark
                          "(setq mark-active t)")
         elisp-code (format "(progn
                               (find-file \"%s\")
                               (switch-to-buffer (current-buffer))
                               (let ((revert-without-query '(\".*\")))
                                 (revert-buffer t t t))
                               ;; Enable auto-revert-mode to ensure the buffer automatically picks up
                               ;; external changes to the file (like those made by our tools)
                               (auto-revert-mode 1)
                               (goto-char %d)
                               (push-mark %d t t)
                               %s
                               t)"
                             (str/replace file-path #"\\" "\\\\") ; Escape backslashes
                             start
                             end
                             highlight-code)
         result (emacs-eval elisp-code)]
     (if (map? result)
       ;; Error was returned
       result
       ;; Success
       true))))

(defn temporary-highlight
  "Highlights a region in a file in Emacs for a specified duration.
   
   Arguments:
   - file-path: Path to the file
   - start: Start position
   - end: End position
   - duration: How long to show the highlight in seconds
   
   Notes:
   - This function enables auto-revert-mode in the visited buffer to ensure
     changes made externally are automatically reflected
   
   Returns:
   - On success: true
   - On failure: Map with :error and :message keys"
  [file-path start end duration]
  (highlight-region file-path start end duration))

;; Example usage
(comment
  ;; Evaluate an Emacs command
  (emacs-eval "(+ 1 2 3)")
  
  ;; Highlight a form directly - permanent
  ;; This will also enable auto-revert-mode in the buffer
  (highlight-region "/path/to/file.clj" 12 53)
  
  ;; Highlight a form for 2 seconds
  ;; This will also enable auto-revert-mode in the buffer
  (temporary-highlight "/path/to/file.clj" 12 53 2)
  
  ;; Alternative direct call with duration
  (highlight-region "/path/to/file.clj" 12 53 2)
)
