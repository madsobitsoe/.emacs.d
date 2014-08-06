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

;; Save 1 or 2 keystrokes constantly
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

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "elpa/dash-20140717.547"))
(add-to-list 'load-path (concat user-emacs-directory "elpa/rainbow-delimiters-20140713.1131"))
;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)



;; Setup appearance early
(require 'appearance)

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

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(htmlize
     multiple-cursors
     expand-region
     smex
     simple-httpd
     highlight-escape-sequences
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))


;; Start with sane defaults
(require 'sane-defaults)


;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))
;; If so, set some stuff
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (require 'mac))



;; Setup extensions
(eval-after-load 'shell '(require 'setup-shell))



;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))


;; Load stuff on demand
(autoload 'skewer-start "setup-skewer" nil t)
(autoload 'skewer-demo "setup-skewer" nil t)

;; Map files to modes
(require 'mode-mappings)

;; Setup smex - Smart M-x
(require 'smex)
(smex-initialize)




;; Set up autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140618.2217/dict")
(ac-config-default)


;; setup some variables for WDIRED
;; invoke with C-x C-q in any dired buffer
;; edit stuff, commit with C-c C-C
(setq wdired-use-interactive-rename t)
(setq wdired-confirm-overwrite t)


;; Setup multiple-cursors
;; ------------------------------
;; Useful for editing lots of text as one
;; Aswell as bulk-renaming in dired
;; 
(require 'multiple-cursors)

;; Setup expand region
;; ------------------------------
;; Useful for selecting regions of text
;; based on semantic keywords
;;
(require 'expand-region)

;; Setup htmlize
;; ------------------------------
;; Creates an html version with css of the current buffer
;; keeps formatting, syntax-highlighting etc.
(require 'htmlize)

;; Setup webjump
;; ------------------------------
;; Webjump makes you quickly jump to a website defined in a list
;; Add custom sites here
(require 'webjump)
(setq webjump-sites
      (append '(("Stack Overflow" . "www.stackoverflow.com"))
	      webjump-sample-sites))


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


;; setup eclim

(eval-after-load 'setup-eclim (require 'eclimd))



;; Setup doc-view-mode for pdf
;; ------------------------------
(setq doc-view-ghostscript-program "/usr/local/bin/gs")
(setq doc-view-ghostscript-options (quote ("-dNOPAUSE"
					   "-sDEVICE=png16m" "-dTextAlphaBits=4" "-dBATCH" "-dGraphicsAlphaBits=4"
					   "-dQUIET" "-r120")))


