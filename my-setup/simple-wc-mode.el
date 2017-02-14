;;; latex-wc-mode.el --- show wc-like information in status bar

;; Copyright (C) 2017 Mads Obitsø

;; Author: Mads Obitsø <madsobitsoe@gmail.com>
;; Version: 0.1

;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:
;;
;; A simple minor-mode to display the wordcount of the current latex buffer in the status bar.


;; Version 0.1
;; * initial release

;; Plan
;; * Add timer to automatically run script in intervals
;; * Refactor!


; Save the last calculated 
(make-variable-buffer-local
 (defvar current-count "L: 0 W: 0 C: 0 P: 0" "The current string to be displayed in modeline."))

;; add string to mode-line construct
(setq mode-line-position (assq-delete-all 'latex-wc-mode mode-line-position))
(setq mode-line-position
      (append
       mode-line-position
       '((latex-wc-mode
	  (:eval
	   (format " %s" current-count))))
	 ))



;; Functions for stripping off the latex markup


; Delete the preamble. We don't want to count it.
(defun latex-delete-preamble ()
  (interactive)
  (goto-char (point-min))
  (re-search-forward "\\\\begin\{document\}" nil t)
  (forward-line)
  (delete-region (point-min) (point)))

; Delete \begin{something} and \end{something} lines
(defun latex-delete-begin-and-end-lines ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\\\\\(begin\\|end\\)\{.*?\}\\([.*?]\\)?" nil t)
    (kill-whole-line)))


;; Used to replace things like \verb!something!
(defun replace-latex-markup (token delim)
  (goto-char (point-min))
  (setq rx (format "\\(\\\\verb\\)\\([!-=]\\)\\([a-zA-Z0-9 .,\"-:()]*\\)\\2" token delim))
  (while (re-search-forward rx nil t)
    (replace-match "\\3")))

;; remove commands but leave parameters
(defun remove-markup-in-commands ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\\\.*?\{\\(.*?\\)\}" nil t)
    (replace-match "\\1")))

(defun count-words-in-current-latex-buffer ()
  (interactive)
  (setq current-count (my-latex-word-count)))


(defun my-latex-word-count ()
  (interactive)
  ;; Save the current buffer
  (let ((oldbuf (current-buffer)))
    (save-current-buffer
      (set-buffer (get-buffer-create "simple-wc-mode-temporary-buffer"))
      (erase-buffer)
      (insert-buffer-substring oldbuf)
      (latex-clean-up-file)
      (set-current-word-count)
      )
    )
  )


;; Clean up extra stuff like \item and \maketitle
(defun remove-leftovers ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(\\\\item\\|\\\\maketitle\\)" nil t)
    (replace-match "")))

(defun m-temp-fun ()
  (interactive)
  (mapcar (lambda (args)
	    (funcall #'replace-latex-markup
		     (car args)
		     (cdr args)))
	  '(("verb" . "[!=-]") ;remove \verb!something!, leaving something
	    ("item" . "") ; Remove \item tags
	    )))

(defun latex-clean-up-file ()
;  (interactive)
  (latex-delete-preamble)
  (latex-delete-begin-and-end-lines)
  (remove-markup-in-commands)
  (m-temp-fun)
  (remove-leftovers)
  (goto-char (point-min))
  (flush-lines "^\\s-*$"))

(defun set-current-word-count ()
  (setq current-count
	(format "C:%d W:%d L:%d P:%.3f"
		(abs (- (point-min) (point-max)))
		(count-words-region (point-min) (point-max))
		(line-number-at-pos (point-max))
		(/ (float (abs (- (point-min) (point-max)))) 2400.0))))

  

(define-minor-mode latex-wc-mode
  "Toggle simple-word-count mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the total number of characters, words, lines and pages is
displayed in the mode-line. If buffer is in LaTeX-mode, strip markup before counting."
  :lighter " swc"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c r") 'my-latex-word-count)
	    map)
  (my-latex-word-count))

(add-hook 'latex-mode 'latex-wc-mode)
(provide 'latex-wc-mode)
;;; latex-wc-mode.el ends here
