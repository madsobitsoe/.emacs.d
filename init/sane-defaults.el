;; --------------------------------
;; Somewhat sane defaults
;; --------------------------------


;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Always display line and column numbers in modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; Always display line numbers
(global-display-line-numbers-mode 1)

;; Show empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate silly-cased words
(global-subword-mode 1)




(use-package savehist
  :ensure nil
  :init
  (savehist-mode t)
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory)))


(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "tmp" user-emacs-directory))))
  :config
  (global-undo-tree-mode))


;; Write backup files to .emacs.d/backups
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Write auto-save and temporary files to .emacs.d/tmp
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "tmp/" user-emacs-directory) t)))

;; Create directories if they don't exist
(make-directory (expand-file-name "backups" user-emacs-directory) t)
(make-directory (expand-file-name "tmp" user-emacs-directory) t)
