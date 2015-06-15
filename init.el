;; init.el
;; ------------------------------
;; Author: Mads Obitsø
;; contains lot's of copied code

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Send the splash screen crying back to it's mother
(setq inhibit-startup-message t)

;; Save 1 or 2 keystrokes constantly;; Save 1 or 2 keystrokes constantly
(fset 'yes-or-no-p 'y-or-n-p)
;; Ask before killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Waste of bytes
(setq initial-scratch-message 
";; Welcome to your domain of evil
;; The elisp is loose
")

;; Straight to *scratch*
(setq initial-buffer-choice t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq my-setup-lisp
      (expand-file-name "my-setup" user-emacs-directory))

;; Set up load
(add-to-list 'load-path my-setup-lisp)
(add-to-list 'load-path site-lisp-dir)
;; Make site-lisp-dir the package folder
(setq package-user-dir "~/.emacs.d/site-lisp")


;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)



;; Setup appearance early
(require 'appearance)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))



;; Setup my custom functions
(require 'my-functions)

;; setup custom keybindings
(require 'keybindings)

;; Write backup files to own directory
;; Get's rid of ~file~
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Set up saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;; Setup packages
(require 'setup-package)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))
;; If so, set some stuff
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize)


  (require 'mac))


;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(undo-tree
     ace-jump-mode
     htmlize
     smex
     simple-httpd
     highlight-escape-sequences
     flx-ido
     ido-vertical-mode
     ido-at-point
     ido-ubiquitous
     auto-complete
     markdown-mode
     helm
     )))


(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))



;; Start with sane defaults
(require 'sane-defaults)

;; Setup extensions
(eval-after-load 'shell '(require 'setup-shell))

;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))


;; Map files to modes
(require 'mode-mappings)

;; Setup ido
(require 'setup-ido)


;; Setup smex - Smart M-x
(require 'smex)
(smex-initialize)

;; Set up autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete-20140803.2118/dict")
(ac-config-default)


;; Set up org
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; setup some variables for WDIRED
;; invoke with C-x C-q in any dired buffer
;; edit stuff, commit with C-c C-C
(setq wdired-use-interactive-rename t)
(setq wdired-confirm-overwrite t)



;; Setup htmlize
;; ------------------------------
;; Creates an html version with css of the current buffer
;; keeps formatting, syntax-highlighting etc.
(require 'htmlize)


;; Setup Markdown major mode
;; ------------------------------
;; Major mode for editing markdown files
;; 
(require 'markdown-mode)

;; Undo-tree
;; ------------------------------
(require 'undo-tree)
(global-undo-tree-mode)

;; Setup helm-spotify-custom
(require 'helm-spotify-custom)

 
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)





