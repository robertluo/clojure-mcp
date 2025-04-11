;; Emacs Highlight Extensions
;; ==========================
;;
;; A collection of functions for highlighting changes in Emacs buffers
;; with visual feedback.

(ns clojure-mcp.emacs-tools.emacs-highlight
  (:require [clojure.string :as str]
            [clojure-mcp.emacs-tools.emacs-client :as emacs]))

;; Core highlighting functions
;; --------------------------

(defn highlight-region
  "Highlights a region of text in a buffer.
   Uses the given face (default is 'highlight) and brings the buffer to focus.
   Returns the overlay object ID that can be used for later removal."
  [buffer-name start end & {:keys [face duration] :or {face 'highlight duration nil}}]
  (emacs/emacs-eval-and-focus
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

(defn clear-highlights
  "Clears all highlights created by highlight-region in a buffer."
  [buffer-name]
  (emacs/emacs-eval-and-focus
   (format "(with-current-buffer \"%s\"
              (remove-overlays (point-min) (point-max) 'emacs-client-overlay t))"
           buffer-name)))

(defn flash-modeline
  "Flashes the modeline to indicate a change has happened.
   This is very noticeable regardless of where the change occurred."
  [buffer-name & {:keys [flash-count delay] :or {flash-count 2 delay 0.1}}]
  (emacs/emacs-eval-and-focus
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

;; Enhanced buffer operations with highlighting
;; -------------------------------------------

(defn append-with-highlight
  "Appends content to the end of a buffer with highlighting.
   Properly handles escaping of special characters and provides visual feedback."
  [buffer-name content & {:keys [highlight-duration highlight-face flash]
                          :or {highlight-duration 2.0
                               highlight-face 'secondary-selection
                               flash true}}]
  (let [escaped-content (-> content
                            (str/replace "\\" "\\\\")
                            (str/replace "\"" "\\\"")
                            (str/replace "\n" "\\n"))]
    (emacs/emacs-eval-and-focus
     (format "(with-current-buffer \"%s\"
                (let ((start (point-max)))
                  (goto-char start)
                  (insert \"\n\n%s\")
                  (let ((overlay (make-overlay start (point-max))))
                    (overlay-put overlay 'face '%s)
                    (overlay-put overlay 'priority 100)
                    (overlay-put overlay 'emacs-client-overlay t)
                    (run-with-timer %s nil (lambda () (delete-overlay overlay)))))))"
             buffer-name
             escaped-content
             highlight-face
             highlight-duration))
    ;; Optional modeline flash
    (when flash
      (flash-modeline buffer-name))))

(defn update-with-highlight
  "Updates occurrences of old-string with new-string in the buffer with highlighting.
   Highlights only the changed text and provides visual feedback."
  [buffer-name old-string new-string & {:keys [highlight-duration highlight-face flash]
                                         :or {highlight-duration 2.0
                                              highlight-face 'secondary-selection
                                              flash true}}]
  (emacs/emacs-eval-and-focus
   (format "(with-current-buffer \"%s\"
              (save-excursion
                (goto-char (point-min))
                (let ((count 0))
                  (while (search-forward \"%s\" nil t)
                    (setq count (1+ count))
                    (let ((start (match-beginning 0)))
                      (replace-match \"%s\" nil t)
                      (let ((end (point))
                            (overlay (make-overlay start end)))
                        (overlay-put overlay 'face '%s)
                        (overlay-put overlay 'priority 100)
                        (overlay-put overlay 'emacs-client-overlay t)
                        (run-with-timer %s nil (lambda () (delete-overlay overlay))))))
                  (message \"Updated %%d occurrences\" count)))))"
           buffer-name
           (str/replace old-string "\"" "\\\"")
           (str/replace new-string "\"" "\\\"")
           highlight-face
           highlight-duration))
  ;; Optional modeline flash
  (when flash
    (flash-modeline buffer-name)))

;; Usage Examples
(comment
  ;; Basic usage
  (append-with-highlight "*buffer*" "New content")
  (update-with-highlight "*buffer*" "old text" "new text")
  
  ;; Customized usage
  (append-with-highlight "*buffer*" "Custom highlight content"
                        :highlight-duration 3.0
                        :highlight-face 'region
                        :flash false)
  
  (update-with-highlight "*buffer*" "find this" "replace with this"
                         :highlight-duration 2.5
                         :highlight-face 'secondary-selection
                         :flash true)
)
