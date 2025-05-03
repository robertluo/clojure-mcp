(ns clojure-mcp.utils.emacs-integration
  "Utilities for integrating with Emacs editor.
   Provides asynchronous notifications and highlighting capabilities."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.tools.logging :as log]))

;; ===== Core Elisp Evaluation Functions =====

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
        (let [output (-> (:out result)
                         (str/trim)
                         (str/replace #"^\"" "")
                         (str/replace #"\"$" ""))]
          (log/debug "Emacs eval succeeded with result:" (pr-str output))
          output)
        ;; Non-zero exit case
        (let [error-msg (format "Emacs client returned error code %d: %s"
                                (:exit result)
                                (or (not-empty (:err result)) (:out result)))]
          (log/error "Emacs eval failed:" error-msg)
          {:error :emacs-error
           :message error-msg})))
    (catch Exception e
      (let [error-msg (format "Failed to execute emacsclient: %s" (.getMessage e))]
        (log/error e "Exception during emacs-eval:" error-msg)
        {:error :execution-error
         :message error-msg}))))

(defn emacs-eval-async
  "Asynchronously evaluates elisp code in Emacs using emacsclient.
   Does not block and does not capture the output.
   
   Arguments:
   - elisp-code: String containing Emacs Lisp code to be evaluated
   
   Returns:
   - A future object that can be deref'd if needed (but usually ignored)
   
   Note: This function is designed to be non-blocking by using Clojure's future."
  [elisp-code]
  (future
    (try
      (log/debug "Inside future: executing emacsclient")
      (let [result (shell/sh "emacsclient" "--eval" elisp-code)]
        (if (zero? (:exit result))
          (log/debug "Async emacs eval succeeded with exit code 0")
          (log/error "Async emacs eval failed with exit code" (:exit result)
                     "error:" (pr-str (or (not-empty (:err result)) (:out result))))))
      (catch Exception e
        (let [error-msg (format "Failed to execute emacsclient asynchronously: %s" (.getMessage e))]
          (log/error e "Exception during async emacs-eval:" error-msg)
          {:error :execution-error
           :message error-msg})))))

;; ===== Elisp Code Generation Functions =====

(defn gen-file-visit-code [file-path]
  (format "(find-file \"%s\")"
          (str/replace file-path #"\\" "\\\\")))

(defn gen-auto-revert-setup-code []
  "(progn
     (switch-to-buffer (current-buffer))
     (auto-revert-mode 1)
     (let ((revert-without-query '(\".*\")))
       (revert-buffer t t t)))")

(defn gen-goto-position-code [position]
  (format "(goto-char %d)" position))

(defn gen-highlight-code [start end duration]
  (format "(with-silent-modifications 
            (let ((overlay (make-overlay %d %d)))
            (overlay-put overlay 'face 'highlight)
            (overlay-put overlay 'priority 100)
            (run-with-timer %s nil 
              (lambda () (with-silent-modifications (delete-overlay overlay))))))"
          start end (float duration)))

;; ===== Main Emacs Integration Functions =====

(defn highlight-region
  "Asynchronously highlights a region in a file in Emacs.
   This function is non-blocking and returns immediately.
   
   Arguments:
   - file-path: Path to the file
   - start: Start position
   - end: End position
   - duration: (optional) How long to show the highlight in seconds (default: 2 seconds)
   
   Notes:
   - This function enables auto-revert-mode in the buffer to ensure changes are reflected
   - This is an asynchronous operation that won't block execution
   
   Returns:
   - A future object (can be ignored)"
  ([file-path start end]
   (highlight-region file-path start end 2.0))
  ([file-path start end duration]
   (log/info "highlight-region called with file-path:" (pr-str file-path)
             ", start:" start ", end:" end ", duration:" duration)
   (let [elisp-code (format "(progn
                             %s
                             %s
                             %s
                             %s
                             t)"
                            (gen-file-visit-code file-path)
                            (gen-auto-revert-setup-code)
                            (gen-goto-position-code start)
                            (gen-highlight-code start end duration))]
     (log/debug "Generated elisp code:" (pr-str elisp-code))
     (emacs-eval-async elisp-code))))

(defn ensure-auto-revert
  "Asynchronously ensures that a file is open in Emacs with auto-revert-mode enabled.
   This function is non-blocking and returns immediately.
   
   Arguments:
   - file-path: Path to the file to open and configure
   
   Notes:
   - This function finds or opens the file in Emacs
   - Enables auto-revert-mode for the buffer
   - Configures revert-without-query to avoid prompts
   - Performs an immediate revert to ensure the buffer is up-to-date
   - This is an asynchronous operation that won't block execution
   
   Returns:
   - A future object (can be ignored)"
  [file-path]
  (log/info "ensure-auto-revert called with file-path:" (pr-str file-path))
  (let [elisp-code (format "(progn
                           %s
                           %s
                           t)"
                           (gen-file-visit-code file-path)
                           (gen-auto-revert-setup-code))]
    (log/debug "Generated elisp code:" (pr-str elisp-code))
    (emacs-eval-async elisp-code)))

(defn config-enables-emacs-notifications?
  "Returns true if the config has enabled emacs notifications.
   Tests for the presence and value of :enable-emacs-notifications key in the config map."
  [config]
  (and config (:enable-emacs-notifications config)))

(defn notify-emacs-file-changed
  "Notifies Emacs that a file has been changed.
   Opens the file and enables auto-revert-mode.
   
   Arguments:
   - config: Configuration map that may enable/disable Emacs notifications
   - file-path: Path to the file that changed
   
   Returns:
   - nil if notifications are disabled in config
   - A future object if notifications are enabled (can be ignored)"
  [config file-path]
  (log/debug "notify-emacs-file-changed called with file-path:" (pr-str file-path))
  (when (config-enables-emacs-notifications? config)
    (ensure-auto-revert file-path)))

(defn notify-emacs-region-changed
  "Notifies Emacs that a region in a file has been changed.
   Opens the file, enables auto-revert-mode, and highlights the changed region.
   
   Arguments:
   - config: Configuration map that may enable/disable Emacs notifications
   - file-path: Path to the file that changed
   - start: Start position of the changed region
   - end: End position of the changed region
   - duration: (optional) How long to show the highlight in seconds (default: 2 seconds)
   
   Returns:
   - nil if notifications are disabled in config
   - A future object if notifications are enabled (can be ignored)"
  ([config file-path start end]
   (notify-emacs-region-changed config file-path start end 2.0))
  ([config file-path start end duration]
   (log/debug "notify-emacs-region-changed called with file-path:" (pr-str file-path)
              ", start:" start ", end:" end ", duration:" duration)
   (when (config-enables-emacs-notifications? config)
     (highlight-region file-path start end duration))))

(comment
  ;; Core evaluation
  (emacs-eval "(+ 1 2 3)")
  (emacs-eval-async "(message \"Hello from Clojure (async)\")")

  ;; Use highlight-region (now always async)
  (def file-path "~/workspace/llempty/clojure-mcp/README.md")
  (highlight-region file-path 12 53)
  (highlight-region file-path 12 53 5.0)

  ;; Use ensure-auto-revert (now always async)
  (ensure-auto-revert file-path)
)
