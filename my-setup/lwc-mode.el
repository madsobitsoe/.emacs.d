;;; lwc-mode.el --- Word count of latex-buffers in modeline

;; Copyright (C) 2017 Mads Obitsø

;; Author: Mads Obitsø <madsobitsoe@gmail.com>
;; URL:  https://github.com/madsobitsoe/lwc-mode
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
;; * Add support for equations and figures


; Save the last calculated
;;; Code:

(make-variable-buffer-local
 (defvar lwc-mode-current-count "L: 0 W: 0 C: 0 P: 0" "The current string to be displayed in modeline."))

(defvar lwc-mode-characters-per-page 2400)

;; add string to mode-line construct
(setq mode-line-position (assq-delete-all 'lwc-mode mode-line-position))
(setq mode-line-position
      (append
       mode-line-position
       '((lwc-mode
	  (:eval
	   (format " %s" lwc-mode-current-count))))
	 ))

;; Functions for stripping off the latex markup
(defun lwc-mode-delete-preamble ()
  "Deletes the preamble of a latex document."
  (goto-char (point-min))
  (re-search-forward "\\\\begin\{document\}" nil t)
  (forward-line)
  (delete-region (point-min) (point)))

(defun lwc-mode-delete-begin-and-end-lines ()
  "Delete \\begin{something} and \\end{something} lines"
  (goto-char (point-min))
  (while (re-search-forward "\\\\\\(begin\\|end\\)\{.*?\}\\([.*?]\\)?" nil t)
    (kill-whole-line)))

;; Used to replace things like \verb!something!
(defun lwc-mode-replace-latex-markup (token delim)
  (goto-char (point-min))
  (setq rx (format "\\(\\\\verb\\)\\([!-=]\\)\\([a-zA-Z0-9 .,\"-:()]*\\)\\2" token delim))
  (while (re-search-forward rx nil t)
    (replace-match "\\3")))

;; remove commands but leave parameters
(defun lwc-mode-remove-markup-in-commands ()
  (goto-char (point-min))
  (while (re-search-forward "\\\\.*?\{\\(.*?\\)\}" nil t)
    (replace-match "\\1")))

;; Set the current count of words and characters
(defun lwc-mode-count-words-in-current-latex-buffer ()
  (interactive)
  (setq lwc-mode-current-count (lwc-mode-word-count)))

;
(defun lwc-mode-word-count ()
  "Copies the entire buffer to a temporary buffer,
strips the latex markup from it and counts the characters,
then updates the character count in the modeline."
  ;; Save the current buffer
  (if (eq major-mode 'latex-mode)
      (let ((oldbuf (current-buffer)))	
	(with-temp-buffer
	  (insert-buffer-substring oldbuf)
	  (lwc-mode-clean-up-file)
	  (lwc-mode-set-current-word-count)
	  ))
    (lwc-mode-set-current-word-count)))

;; Clean up extra stuff like \item and \maketitle
(defun lwc-mode-remove-leftovers ()
  (goto-char (point-min))
  (while (re-search-forward "\\(\\\\item\\|\\\\maketitle\\)" nil t)
    (replace-match "")))

;; Planning for this to be useful at some point
(defun lwc-mode-m-temp-fun ()
  (mapcar (lambda (args)
	    (funcall #'lwc-mode-replace-latex-markup
		     (car args)
		     (cdr args)))
	  '(("verb" . "[!=-]") ;remove \verb!something!, leaving something
	   ; ("item" . "") ; Remove \item tags
	    )))

(defun lwc-mode-clean-up-file ()
  (lwc-mode-delete-preamble)
  (lwc-mode-delete-begin-and-end-lines)
  (lwc-mode-remove-markup-in-commands)
  (lwc-mode-m-temp-fun)
  (lwc-mode-remove-leftovers)
  (goto-char (point-min))
  (flush-lines "^\\s-*$"))

(defun lwc-mode-set-current-word-count ()
  (setq lwc-mode-current-count
	(format "C:%d W:%d L:%d P:%.3f"
		(abs (- (point-min) (point-max)))
		(count-words-region (point-min) (point-max))
		(line-number-at-pos (point-max))
		(/ (float (abs (- (point-min) (point-max)))) lwc-mode-characters-per-page))))

(defun lwc-mode-set-characters-per-page (n)
  "Set the number of characters for a page."
  (interactive)
  (setq lwc-mode-set-characters-per-page n))


(define-minor-mode lwc-mode
  "Count words in latex buffers by stripping the latex.

This command toggles lwc-mode.
It counts the words in a latex buffer by stripping the latex code away.
When enabled, the total number of characters, words, lines and pages is
displayed in the mode-line. A recount can be made with C-c r."
  :lighter " lwc"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c r") 'lwc-mode-count-words-in-current-latex-buffer)
	    map)
  (lwc-mode-count-words-in-current-latex-buffer))

; Add hook if necessary to provide autoloading
;(add-hook 'LaTeX-mode-hook 'lwc-mode t)
;(add-hook 'latex-mode 'lwc-mode t)
(provide 'lwc-mode)
;;; lwc-mode.el ends here
