;; emacs wrapper to a script in python, ruby, etc.

(defun latex-word-count (startPos endPos)
  "Do some text processing on region.
This command calls the external script “removelatexcode.pl, then pipes it to wc”."
  (interactive "r")
  (let (cmdStr)
    (setq cmdStr "perl ~/.emacs.d/my-setup/removelatexcode.pl - | wc") ; full path to your script
    (shell-command-on-region startPos endPos cmdStr nil nil nil t)))
