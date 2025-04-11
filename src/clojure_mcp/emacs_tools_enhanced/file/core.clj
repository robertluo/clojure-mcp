(ns clojure-mcp.emacs-tools-enhanced.file.core
  "Core functionality for Emacs file interactions.
   
   This namespace provides the core elisp evaluation and file handling functions
   that are used by all other file operation namespaces."
  (:require [clojure.string :as str]
            [clojure.java.shell :as shell]))

;; -------------------------------------------------------------------------
;; Result helpers
;; -------------------------------------------------------------------------

(defn error-result [error-message]
  {:success false
   :messages []
   :contents [error-message]})

(defn success-result [contents & messages]
  {:success true
   :messages (or messages [])
   :contents contents})

;; -------------------------------------------------------------------------
;; Core Elisp Evaluation
;; -------------------------------------------------------------------------

(defn emacs-eval
  "Evaluates elisp code in Emacs using emacsclient with improved error handling.
   Returns the result of evaluation with quotes stripped."
  [elisp-code]
  (let [result (shell/sh "emacsclient" "--eval" elisp-code)]
    (when (zero? (:exit result))
      (-> (:out result)
          (str/trim)
          (str/replace #"^\"" "")
          (str/replace #"\"$" "")))))

(defn with-file
  "Executes a function with elisp code on a file with improved error handling.
   Returns a map with :success, :message, and optionally :value fields."
  [file-path elisp-code & {:keys [conflict-strategy] :or {conflict-strategy :error}}]
  (try
    (let [wrapped-code (format "(condition-case err
                                  (progn
                                    (find-file \"%s\")
                                    ;; Check for conflicts and handle based on strategy
                                    %s
                                    (let ((result %s))
                                      (list t \"Success\" result)))
                                  (error (list nil (error-message-string err))))"
                               (str/replace file-path "\"" "\\\"")
                               (case conflict-strategy
                                 :error (str "(when (and (buffer-modified-p)
                                             (file-exists-p (buffer-file-name))
                                             (not (verify-visited-file-modtime (current-buffer))))
                                     (error \"File has been modified externally and in Emacs. Please resolve conflicts manually.\"))")
                                 :force-save "(set-buffer-modified-p t) ;; Force buffer to be considered modified
                                             (let ((revert-without-query '(\".*\")))
                                               (set-visited-file-modtime (current-time))) ;; Update buffer's idea of modification time"
                                 :revert-first "(let ((revert-without-query '(\".*\")))
                                                  (revert-buffer t t t)) ;; Revert without confirmation"
                                 ;; Default case for unknown strategies
                                 (str "(when (and (buffer-modified-p)
                                             (file-exists-p (buffer-file-name))
                                             (not (verify-visited-file-modtime (current-buffer))))
                                     (error \"File has been modified externally and in Emacs. Please resolve conflicts manually.\"))"))
                               elisp-code)
          result (emacs-eval wrapped-code)
          parsed (read-string result)]
      (if (first parsed)
        {:success true
         :message (second parsed)
         :value (nth parsed 2)}
        {:success false
         :message (second parsed)}))
    (catch Exception e
      {:success false
       :message (str "Error: " (.getMessage e))})))
