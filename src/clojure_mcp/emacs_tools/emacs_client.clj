(ns clojure-mcp.emacs-tools.emacs-client
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]))

;; Core Plumbing Functions
;; -----------------------

(defn emacs-eval
  "Evaluates elisp code in Emacs using emacsclient.
   Returns the result of evaluation with quotes stripped."
  [elisp-code]
  (let [result (shell/sh "emacsclient" "--eval" elisp-code)]
    (when (zero? (:exit result))
      (-> (:out result)
          (str/trim)
          (str/replace #"^\"" "")
          (str/replace #"\"$" "")))))

(defn emacs-eval-and-focus
  "Evaluates elisp code in Emacs and ensures the frame is focused."
  [elisp-code]
  (emacs-eval (format "(progn
                         %s
                         (select-frame-set-input-focus (selected-frame))
                         (raise-frame)
                         nil)"
                      elisp-code)))

;; Buffer Management - Main API
;; ----------------------------

(defn emacs-buffer-exists?
  "Check if a buffer exists in Emacs."
  [buffer-name]
  (= "t" (emacs-eval (format "(if (get-buffer \"%s\") \"t\" \"nil\")" buffer-name))))

(defn emacs-create-buffer
  "Creates a new buffer in Emacs with the given name if it doesn't exist."
  [buffer-name]
  (when-not (emacs-buffer-exists? buffer-name)
    (emacs-eval-and-focus (format "(generate-new-buffer \"%s\")" buffer-name))))

(defn emacs-current-buffer
  "Returns the name of the buffer currently displayed in the selected window."
  []
  (emacs-eval "(buffer-name (window-buffer (selected-window)))"))

(defn emacs-buffer-content
  "Gets the entire content of a buffer."
  [buffer-name]
  (emacs-eval (format "(with-current-buffer \"%s\" (buffer-string))" buffer-name)))

(defn emacs-replace-buffer-content
  "Replaces the entire content of a buffer."
  [buffer-name content]
  (emacs-eval-and-focus
   (format "(with-current-buffer \"%s\" 
             (erase-buffer)
             (insert \"%s\"))"
           buffer-name
           (str/replace content "\"" "\\\""))))

(defn emacs-buffer-update
  "Updates all occurrences of old-string with new-string in the buffer."
  [buffer-name old-string new-string]
  (emacs-eval-and-focus
   (format "(with-current-buffer \"%s\"
             (save-excursion
               (goto-char (point-min))
               (while (search-forward \"%s\" nil t)
                 (replace-match \"%s\" nil t))))"
           buffer-name
           (str/replace old-string "\"" "\\\"")
           (str/replace new-string "\"" "\\\""))))

(defn emacs-switch-to-buffer
  "Switches to a buffer in Emacs and brings the window to focus."
  [buffer-name]
  (emacs-eval-and-focus (format "(switch-to-buffer \"%s\")" buffer-name)))

(defn emacs-save-buffer
  "Saves buffer content to a file."
  [buffer-name file-path]
  (emacs-eval-and-focus
   (format "(with-current-buffer \"%s\"
             (write-file \"%s\"))"
           buffer-name
           file-path)))

(defn emacs-with-current-buffer
  "Applies function f to the name of the current buffer.
   Useful for operations on the buffer the user is currently viewing."
  [f]
  (let [buffer-name (emacs-current-buffer)]
    (f buffer-name)))

;; File Operations
;; --------------

(defn emacs-open-file
  "Opens a file in Emacs and brings the window to focus."
  [file-path]
  (emacs-eval-and-focus (format "(find-file \"%s\")" file-path)))

;; Example Usage
;; ------------
#_
(comment
  ;; Create and work with a new buffer
  (def buffer-name "*clojure-emacs-demo*")
  (emacs-create-buffer buffer-name)
  (emacs-replace-buffer-content buffer-name "(defn hello [name]\n  (str \"Hello, \" name \"!\"))")
  (emacs-switch-to-buffer buffer-name)
  
  ;; Get buffer content
  (emacs-buffer-content buffer-name)
  
  ;; Update buffer content
  (emacs-buffer-update buffer-name "Hello, " "Greetings, ")
  
  ;; Save to file
  (emacs-save-buffer buffer-name "/tmp/hello.clj")
  
  ;; Open existing file
  (emacs-open-file "/tmp/hello.clj")
  
  ;; Work with current buffer
  (emacs-with-current-buffer emacs-buffer-content)
  (emacs-with-current-buffer #(emacs-buffer-update % "name" "person"))
  ;; Line and region operations
  (emacs-current-line-number)
  (emacs-goto-line-column buffer-name 3 5) ;; Go to line 3, column 5
  (emacs-replace-region buffer-name 2 4 ";;; This replaces lines 2-4")
  
  ;; Get file path
  (emacs-buffer-file-path buffer-name)
  
)



;; TEST TEXT - PLEASE VERIFY THIS IS VISIBLE

;; Test highlight function



(defn emacs-insert-at-position
  "Inserts text at a specific position in a buffer and brings it to focus.
   The position is specified as a character offset from the beginning of the buffer."
  [buffer-name position text]
  (emacs-eval-and-focus
   (format "(progn
              (switch-to-buffer \"%s\")
              (save-excursion
                (goto-char %d)
                (insert \"%s\")))"
           buffer-name
           position
           (str/replace text "\"" "\\\""))))

(defn emacs-append-to-buffer
  "Appends content to the end of a buffer and brings it to focus.
   Properly handles escaping of special characters."
  [buffer-name content]
  (let [escaped-content (-> content
                            (str/replace "\\" "\\\\")
                            (str/replace "\"" "\\\"")
                            (str/replace "\n" "\\n"))]
    (emacs-eval-and-focus
     (format "(with-current-buffer \"%s\"
                (goto-char (point-max))
                (insert \"\n\n%s\"))"
             buffer-name
             escaped-content))))

(defn emacs-remove-function
  "Removes a function definition from a buffer given the function name.
   Uses elisp directly for more reliable function detection and removal.
   Returns true if the function was found and removed, false otherwise."
  [buffer-name function-name]
  (let [result (emacs-eval 
                (format "(with-current-buffer \"%s\"
                           (let ((found nil))
                             (save-excursion
                               (goto-char (point-min))
                               (while (and (not found)
                                           (re-search-forward \"(defn\\\\s-+%s\\\\b\" nil t))
                                 (goto-char (match-beginning 0))
                                 (when (looking-at \"(defn\")
                                   (setq found t)
                                   (let ((start (point)))
                                     (forward-sexp 1)
                                     (delete-region start (point))))))
                             (if found \"t\" \"nil\")))"
                        buffer-name
                        function-name))]
    (= "t" result)))


(defn emacs-highlight-region
  "Highlights a region of text in a buffer.
   Uses the given face (default is 'highlight) and brings the buffer to focus.
   Returns the overlay object ID that can be used for later removal."
  [buffer-name start end & {:keys [face duration] :or {face 'highlight duration nil}}]
  (emacs-eval-and-focus
   (format "(progn
              (switch-to-buffer \"%s\")
              (let ((overlay (make-overlay %d %d)))
                (overlay-put overlay 'face '%s)
                (overlay-put overlay 'priority 100)
                (overlay-put overlay 'emacs-client-overlay t)
                %s
                (overlay-start overlay)))"
           buffer-name
           start
           end
           face
           (if duration
             (format "(run-with-timer %s nil (lambda () (delete-overlay overlay)))" duration)
             ""))))

(defn emacs-clear-highlights
  "Clears all highlights created by emacs-highlight-region in a buffer."
  [buffer-name]
  (emacs-eval-and-focus
   (format "(with-current-buffer \"%s\"
              (remove-overlays (point-min) (point-max) 'emacs-client-overlay t))"
           buffer-name)))

(defn emacs-replace-buffer-content-with-highlight
  "Replaces the entire content of a buffer and highlights the change.
   The highlight will automatically fade after the specified duration (in seconds)."
  [buffer-name content & {:keys [highlight-duration] :or {highlight-duration 1.5}}]
  (emacs-eval-and-focus
   (format "(progn
              (switch-to-buffer \"%s\")
              (let ((old-point (point)))
                (erase-buffer)
                (insert \"%s\")
                (let ((overlay (make-overlay (point-min) (point-max))))
                  (overlay-put overlay 'face 'highlight)
                  (overlay-put overlay 'priority 100)
                  (overlay-put overlay 'emacs-client-overlay t)
                  (run-with-timer %s nil (lambda () (delete-overlay overlay))))
                (goto-char old-point)))"
           buffer-name
           (str/replace content "\"" "\\\"")
           highlight-duration)))

(defn emacs-buffer-update-with-highlight
  "Updates all occurrences of old-string with new-string in the buffer,
   and highlights each change with a temporary highlight."
  [buffer-name old-string new-string & {:keys [highlight-duration] :or {highlight-duration 1.5}}]
  (emacs-eval-and-focus
   (format "(with-current-buffer \"%s\"
              (save-excursion
                (goto-char (point-min))
                (while (search-forward \"%s\" nil t)
                  (let ((start (match-beginning 0)))
                    (replace-match \"%s\" nil t)
                    (let ((end (point))
                          (overlay (make-overlay start end)))
                      (overlay-put overlay 'face 'highlight)
                      (overlay-put overlay 'priority 100)
                      (overlay-put overlay 'emacs-client-overlay t)
                      (run-with-timer %s nil (lambda () (delete-overlay overlay)))))))"
           buffer-name
           (str/replace old-string "\"" "\\\"")
           (str/replace new-string "\"" "\\\"")
           highlight-duration)))

(defn emacs-append-to-buffer-with-highlight
  "Appends content to the end of a buffer and highlights the new content.
   Properly handles escaping of special characters."
  [buffer-name content & {:keys [highlight-duration] :or {highlight-duration 1.5}}]
  (let [escaped-content (-> content
                            (str/replace "\\" "\\\\")
                            (str/replace "\"" "\\\"")
                            (str/replace "\n" "\\n"))]
    (emacs-eval-and-focus
     (format "(with-current-buffer \"%s\"
                (let ((start (point-max)))
                  (goto-char start)
                  (insert \"\n\n%s\")
                  (let ((overlay (make-overlay start (point-max))))
                    (overlay-put overlay 'face 'highlight)
                    (overlay-put overlay 'priority 100)
                    (overlay-put overlay 'emacs-client-overlay t)
                    (run-with-timer %s nil (lambda () (delete-overlay overlay))))))"
             buffer-name
             escaped-content
             highlight-duration))))

(defn emacs-with-operation-highlight
  "Executes a function that performs buffer operations and highlights the changes.
   f should be a function that takes buffer-name as its first argument."
  [buffer-name f & {:keys [highlight-duration] :or {highlight-duration 1.5}}]
  (let [before-content (emacs-buffer-content buffer-name)
        result (f buffer-name)
        after-content (emacs-buffer-content buffer-name)]
    (when (not= before-content after-content)
      (emacs-eval-and-focus
       (format "(with-current-buffer \"%s\"
                  (let ((overlay (make-overlay (point-min) (point-max))))
                    (overlay-put overlay 'face 'highlight)
                    (overlay-put overlay 'priority 100)
                    (overlay-put overlay 'emacs-client-overlay t)
                    (run-with-timer %s nil (lambda () (delete-overlay overlay)))))"
               buffer-name
               highlight-duration)))
    result))

(defn emacs-with-operation-highlight-improved
  "Executes a function that performs buffer operations and highlights the changes.
   Uses a more noticeable highlight with a different color.
   f should be a function that takes buffer-name as its first argument."
  [buffer-name f & {:keys [highlight-duration highlight-face] 
                    :or {highlight-duration 2.0
                         highlight-face 'secondary-selection}}]
  (let [before-content (emacs-buffer-content buffer-name)
        result (f buffer-name)
        after-content (emacs-buffer-content buffer-name)]
    (when (not= before-content after-content)
      (emacs-eval-and-focus
       (format "(with-current-buffer \"%s\"
                  (let ((overlay (make-overlay (point-min) (point-max))))
                    (overlay-put overlay 'face '%s)
                    (overlay-put overlay 'priority 100)
                    (overlay-put overlay 'emacs-client-overlay t)
                    (message \"Highlighting changes in buffer...\")
                    (run-with-timer %s nil 
                      (lambda () 
                        (delete-overlay overlay)
                        (message \"Highlight removed\"))))"
               buffer-name
               highlight-face
               highlight-duration)))
    result))

(defn emacs-with-specific-change-highlight
  "Executes a function and then highlights only what changed in the buffer.
   More effective for targeted changes."
  [buffer-name f & {:keys [highlight-duration] 
                   :or {highlight-duration 3.0}}]
  (let [before-content (emacs-buffer-content buffer-name)
        result (f buffer-name)
        after-content (emacs-buffer-content buffer-name)]
    (if (not= before-content after-content)
      ;; Use a diff approach to find what changed
      (emacs-eval-and-focus
       (format "(with-current-buffer \"%s\"
                  (let ((inhibit-modification-hooks t)
                        (temp-buffer (generate-new-buffer \" *temp*\")))
                    (unwind-protect
                        (progn
                          (with-current-buffer temp-buffer
                            (insert \"%s\")
                            (diff-buffers temp-buffer \"%s\" nil)
                            ;; Flash the diff buffer briefly
                            (display-buffer \"*Diff*\")
                            (run-with-timer %s nil 
                              (lambda () 
                                (kill-buffer \"*Diff*\")
                                (message \"Change highlighted\")))))
                      (kill-buffer temp-buffer))))"
               buffer-name
               (str/replace before-content "\"" "\\\"") 
               buffer-name
               highlight-duration))
      (emacs-eval "(message \"No changes detected\")"))
    result))

(defn emacs-highlight-recent-change
  "Highlights the most recent change in a buffer.
   Uses a pulsing effect to make it more noticeable."
  [buffer-name]
  (emacs-eval-and-focus
   (format "(with-current-buffer \"%s\"
              (let ((pulse-iterations 10)
                    (pulse-delay 0.05))
                (goto-char (point-min))
                (pulse-momentary-highlight-region 
                  (point-min) 
                  (save-excursion
                    (forward-line 3)
                    (point)))))"
           buffer-name)))

(defn emacs-flash-modeline
  "Flashes the modeline to indicate a change has happened.
   This is very noticeable regardless of where the change occurred."
  [buffer-name & {:keys [flash-count delay] :or {flash-count 3 delay 0.1}}]
  (emacs-eval-and-focus
   (format "(with-current-buffer \"%s\"
              (let ((bg (face-background 'mode-line))
                    (count %d))
                (defun flash-mode-line-once ()
                  (set-face-background 'mode-line \"red\")
                  (run-with-timer %s nil
                    (lambda ()
                      (set-face-background 'mode-line bg)
                      (setq count (1- count))
                      (when (> count 0)
                        (run-with-timer %s nil 'flash-mode-line-once)))))
                (flash-mode-line-once)))"
           buffer-name
           flash-count
           delay
           delay)))

(defn emacs-with-operation-highlight-and-flash
  "Executes a function, highlights changes, and flashes the modeline for extra visibility."
  [buffer-name f & {:keys [highlight-duration highlight-face flash-count flash-delay] 
                    :or {highlight-duration 2.0
                         highlight-face 'secondary-selection
                         flash-count 3
                         flash-delay 0.1}}]
  (let [before-content (emacs-buffer-content buffer-name)
        result (f buffer-name)
        after-content (emacs-buffer-content buffer-name)]
    (when (not= before-content after-content)
      ;; Apply the highlight
      (emacs-eval-and-focus
       (format "(with-current-buffer \"%s\"
                  (let ((overlay (make-overlay (point-min) (point-max))))
                    (overlay-put overlay 'face '%s)
                    (overlay-put overlay 'priority 100)
                    (overlay-put overlay 'emacs-client-overlay t)
                    (run-with-timer %s nil (lambda () (delete-overlay overlay)))))"
               buffer-name
               highlight-face
               highlight-duration))
      ;; Flash the modeline
      (emacs-flash-modeline buffer-name :flash-count flash-count :delay flash-delay))
    result))
