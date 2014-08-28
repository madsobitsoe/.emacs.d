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
	(narrow-to-region start end)
        (let ((count start))
          (save-excursion
            (while (re-search-forward place nil t)
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


;; Yank-on-right
;; -------------
;;
(defun yank-on-right (start end &optional margin)
  "Yank the current kill, inserting it to the right of the
current region.  Rectangle editing can be used to place blocks of
text in columns alongside each other.  But that usually requires
finding the longest lines and then padding top or bottom lines to
match.  This function produces the same effect without the
hassle."
  (interactive "r\np")
  (goto-char start)
  (end-of-line)
  (let ((lines (split-string (current-kill 0) "\n"))
	(width (current-column)))
    (while (< (point) end)
      (end-of-line 2)
      (setq width (max width (current-column))))
    (setq width (+ margin width))
    (goto-char start)
    (push-mark end)
    (while (and (< (point) (mark)) lines)
      (move-to-column width t)
      (insert (car lines))
      (setq lines (cdr lines))
      (forward-line))
    (pop-mark)))


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



;; Yank-on-right
;; -------------
;;
(defun yank-on-right (start end &optional margin)
  "Yank the current kill, inserting it to the right of the
current region.  Rectangle editing can be used to place blocks of
text in columns alongside each other.  But that usually requires
finding the longest lines and then padding top or bottom lines to
match.  This function produces the same effect without the
hassle."
  (interactive "r\np")
  (goto-char start)
  (end-of-line)
  (let ((lines (split-string (current-kill 0) "\n"))
	(width (current-column)))
    (while (< (point) end)
      (end-of-line 2)
      (setq width (max width (current-column))))
    (setq width (+ margin width))
    (goto-char start)
    (push-mark end)
    (while (and (< (point) (mark)) lines)
      (move-to-column width t)
      (insert (car lines))
      (setq lines (cdr lines))
      (forward-line))
    (pop-mark)))



;; Open shell in new window on right
(defun open-shell-on-right ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (eshell))

(defun open-shell-below ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (eshell))


(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

 
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;; Move line down               
(defun move-line-down ()        
  (interactive)                 
  (let ((col (current-column))) 
    (save-excursion             
      (forward-line)            
      (transpose-lines 1))      
    (forward-line)              
    (move-to-column col)))      
                                
  ;; move line up
  (defun move-line-up ()
    (interactive)
    (let ((col (current-column)))
    (save-excursion
        (transpose-lines 1))
        (forward-line -2)
      (move-to-column col)))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))


(provide 'my-functions)
