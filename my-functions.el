;; My own functions
;; ------------------------------
;; Here are my own custom functions


;; For-each-occurence
;; -------
;;
(defun for-each-occurence (begin end place start step)
  "Replace placeholder with incremented list

Replaces a regexp placeholder with a list of numbers incremented in steps"
  (interactive "r\nsPlaceholder: \nnStart: \nnStep: ")
  (toggle-case-fold-search)
  (let ((count start))
    (save-excursion
      (while (re-search-forward place nil t)
	(narrow-to-region start end)
	(replace-match (number-to-string count) nil t)
	(setq count (+ count step))))
    (toggle-case-fold-search)))

;; Kill-other-buffers
;; ------------------------------
;; Kill all buffers but the open one and unsplit the frame
(defun kill-other-buffers ()
  "Kills all buffers but the active one and unsplits the frame"
  (interactive) 
  (when (yes-or-no-p "Are you sure you want to kill all other buffers? ")
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (delete-other-windows)
    (delete-other-frames)))



;; Kill the current buffer and delete the file that was open
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))



;; indent whole buffer
;; ------------------------------
;; Indents the whole buffer
;; Similar to C-x h TAB
(defun indent-buffer ()
  (interactive) 
  (save-excursion
    (mark-whole-buffer)
    (indent-region (point-min) (point-max) nil)))


(defun open-prev-buffer-on-right ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))
(defun open-prev-buffer-below ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))
  



(provide 'my-functions)
