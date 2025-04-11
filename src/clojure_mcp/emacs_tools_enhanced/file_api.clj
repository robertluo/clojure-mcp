(ns clojure-mcp.emacs-tools-enhanced.file-api
  "File-centric API for Emacs interaction.
   
   This namespace provides a comprehensive set of functions for working with files
   directly in Emacs, abstracting away buffer management and focusing on file operations.
   All functions operate directly on files and handle opening, editing, and saving
   automatically."
  (:require [clojure.string :as str]
            [clojure.java.shell :as shell]))

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

;; Removed file-eval function in favor of with-file

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

;; -------------------------------------------------------------------------
;; File Content Operations
;; -------------------------------------------------------------------------

(defn read-file
  "Reads the entire content of a file using Emacs.
   Opens the file in Emacs and retrieves its content without any user interaction.
   Uses special settings to avoid prompts and blocking behavior."
  [file-path]
  (let [result (emacs-eval 
                (format "(condition-case err
                           (progn
                             (let ((auto-revert-verbose nil)  ;; No messages about reverting
                                   (revert-without-query t)   ;; Don't confirm reverting
                                   (inhibit-message t))       ;; Suppress messages
                               (find-file-noselect \"%s\" t)  ;; Use noselect to avoid displaying
                               (with-current-buffer (find-buffer-visiting \"%s\")
                                 (let ((modified-p (buffer-modified-p)))
                                   ;; Force revert without any user interaction
                                   (revert-buffer t t t)
                                   ;; Restore modified state if needed
                                   (set-buffer-modified-p modified-p)
                                   (buffer-string)))))
                           (error (format \"Error: %%s\" (error-message-string err))))"
                         (str/replace file-path "\"" "\\\"")
                         (str/replace file-path "\"" "\\\"")))]
    ;; Check if result starts with "Error:"
    (if (and result (str/starts-with? result "Error:"))
      result  ;; Return the error message
      result)))

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

(defn dry-run-edits
  "Generates a diff preview of the changes that would be made by the edits.
   
   Parameters:
   - file-path: Path to the file to edit
   - edits: Sequence of maps with :old-text and :new-text keys
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Message about dry run
   - :diff - Git-style diff output"
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
     :message "Dry run completed"
     :diff diff-result}))

(defn edit-file
  "Makes multiple text replacements in a file with optional visual highlighting.
   
   Takes a sequence of edits, where each edit is a map with :old-text and :new-text keys.
   Applies edits sequentially in the order provided.
   
   Options:
   - highlight-duration: Duration of the highlight effect in seconds (default: 2.0)
   - dry-run: Preview changes using git-style diff format (default: false)"
  [file-path edits & {:keys [highlight-duration dry-run] 
                     :or {highlight-duration 2.0 dry-run false}}]
  (if (empty? edits)
    {:success true, :message "No edits to apply"}
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
        result))))

(defn replace-in-file
  "Replaces all occurrences of a string with another string in a file,
   with optional visual highlighting.
   
   Options:
   - highlight-duration: Duration of the highlight effect in seconds (default: 2.0)"
  [file-path old-text new-text & {:keys [highlight-duration] :or {highlight-duration 2.0}}]
  (let [result (with-file file-path 
                (format "(progn
                          (save-excursion
                            (goto-char (point-min))
                            (let ((count 0))
                              (while (search-forward \"%s\" nil t)
                                (setq count (1+ count))
                                (let ((start (match-beginning 0)))
                                  (replace-match \"%s\" nil t)
                                  (let ((end (point))
                                        (overlay (make-overlay start end)))
                                    (overlay-put overlay 'face 'highlight)
                                    (overlay-put overlay 'priority 100)
                                    (run-with-timer %s nil 
                                      (lambda () (delete-overlay overlay))))))
                              (message \"Updated %%d occurrences\" count)))
                          (save-buffer))"
                        (str/replace old-text "\"" "\\\"")
                        (str/replace new-text "\"" "\\\"")
                        highlight-duration))]
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

;; -------------------------------------------------------------------------
;; Line Operations
;; -------------------------------------------------------------------------

;; Line-based operations have been removed as requested

(defn find-files
  "Finds files matching a pattern in a directory.
   Returns a list of full file paths."
  [directory pattern]
  (let [result (emacs-eval 
               (format "(let ((files nil))
                         (dolist (file (directory-files-recursively \"%s\" \"%s\" nil))
                           (push file files))
                         (mapconcat 'identity (nreverse files) \"\n\"))"
                       (str/replace directory "\"" "\\\"")
                       (str/replace pattern "\"" "\\\"")))]
    (when (not-empty result)
      (str/split result #"\n"))))

(defn with-files
  "Applies a function to a collection of files.
   The function should take a file path as its first argument.
   Returns a map of file paths to results."
  [file-paths f & args]
  (into {} 
        (map (fn [path] 
               [path (apply f path args)])
             file-paths)))

(defn delete-file
  "Deletes a file with confirmation in Emacs.
   Returns true if successful, false otherwise."
  [file-path & {:keys [no-confirm] :or {no-confirm false}}]
  (let [result (emacs-eval 
               (format "(condition-case err
                         (progn
                           (delete-file \"%s\" %s)
                           t)
                         (error nil))"
                       (str/replace file-path "\"" "\\\"")
                       (if no-confirm "t" "nil")))]
    (= result "t")))

(defn rename-file
  "Renames a file from old-path to new-path.
   Returns true if successful, false otherwise."
  [old-path new-path & {:keys [no-confirm] :or {no-confirm false}}]
  (let [result (emacs-eval 
               (format "(condition-case err
                         (progn
                           (rename-file \"%s\" \"%s\" %s)
                           t)
                         (error nil))"
                       (str/replace old-path "\"" "\\\"")
                       (str/replace new-path "\"" "\\\"")
                       (if no-confirm "t" "nil")))]
    (= result "t")))

(defn flash-file
  "Flashes the file in Emacs to draw attention to it.
   Uses both overlay highlighting and modeline flashing."
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
    result))

(defn copy-file
  "Copies a file from source-path to dest-path.
   Returns true if successful, false otherwise."
  [source-path dest-path & {:keys [no-confirm] :or {no-confirm false}}]
  (let [result (emacs-eval 
               (format "(condition-case err
                         (progn
                           (copy-file \"%s\" \"%s\" %s)
                           t)
                         (error nil))"
                       (str/replace source-path "\"" "\\\"")
                       (str/replace dest-path "\"" "\\\"")
                       (if no-confirm "t" "nil")))]
    (= result "t")))

;; -------------------------------------------------------------------------
;; Advanced Operations
;; -------------------------------------------------------------------------

(defn file-exists?
  "Checks if a file exists using Emacs."
  [file-path]
  (= "t" (emacs-eval (format "(if (file-exists-p \"%s\") \"t\" \"nil\")" 
                            (str/replace file-path "\"" "\\\"")))))

(defn save-file
  "Saves the file if it's open in a buffer."
  [file-path]
  (let [result (with-file file-path "(save-buffer)")]
    result))

;; -------------------------------------------------------------------------
;; Public API
;; -------------------------------------------------------------------------

(defn file-api
  "Returns a map of all file-centric functions for manipulating files in Emacs."
  []
  {:read-file       read-file
   :write-file      write-file
   :edit-file       edit-file        ;; Add our new edit-file function
   :replace-in-file replace-in-file
   :append          append-to-file
   :with-file       with-file
   :file-exists?    file-exists?
   :save-file       save-file
   :flash-file      flash-file
   :delete-file     delete-file
   :rename-file     rename-file
   :copy-file       copy-file
   :find-files      find-files
   :with-files      with-files})

;; Export the API for easy access
(def files (file-api))
