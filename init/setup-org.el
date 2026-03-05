;; --------------------------------
;; Settings for org-mode
;; --------------------------------

(require 'org)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(setq org-directory "~/org")
(setq org-agenda-files
      (list "~/org/main.org"
	    "~/org/hours.org"
	    ))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELLED")))
