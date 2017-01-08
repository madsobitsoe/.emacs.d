;; init.el
;; ------------------------------
;; Author: Mads Obitsø
;; contains lot's of copied code

;; Turn off mouse interface early in startup to avoid momentary display



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Send the splash screen crying back to it's mother
(setq inhibit-startup-message t)

;; Save 1 or 2 keystrokes constantly;;
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
;;(dolist (project (directory-files site-lisp-dir t "\\w+"))
;;  (when (file-directory-p project)
;;    (add-to-list 'load-path project)))


;; Setup smooth scrolling
(require 'mouse)


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
     yasnippet
     markdown-mode
     helm
     elpy
     auctex
     )))


(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))



;; Start with sane defaults
(require 'sane-defaults)

;; If so, set some stuff
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (require 'mac))


;; Setup extensions
(eval-after-load 'shell '(require 'setup-shell))

;; Font lock dash.el
(eval-after-load 'dash '(dash-enable-font-lock))

;; Setup yasnippet
(require 'setup-yasnippet)


;; Setup ido
(require 'setup-ido)
;; Map files to modes
(require 'mode-mappings)


;; Setup smex - Smart M-x
(require 'smex)
(smex-initialize)

;; Set up autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete-20140803.2118/dict")
(ac-config-default)
(global-auto-complete-mode t)


;; Set up org
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/Dropbox/org/GP/Work/tasks.org"
                             "~/Dropbox/org/GP/calendar/googlecalForOrgmode.org"
                             "~/Dropbox/org/diku/pop/pop-opgaver.org"))
(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "|" "DONE" "CANCELLED")))
;; setup some variables for WDIRED
;; invoke with C-x C-q in any dired buffer
;; edit stuff, commit with C-c C-C
(setq wdired-use-interactive-rename t)
(setq wdired-confirm-overwrite t)




;; Setup latex
(require 'setup-latex)


;; Setup fsharp
(require 'setup-fsharp)



;; Setup python-mode
;; ------------------------------
;; 
;; 

;; (require 'python-mode)

;; ;; use IPython
;; (setq-default py-shell-name "ipython")
;; (setq-default py-which-bufname "IPython")
;; ; use the wx backend, for both mayavi and matplotlib
;; (setq py-python-command-args
;;   '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
;; (setq py-force-py-shell-name-p t)

;; ; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p t)
;; (setq py-switch-buffers-on-execute-p t)
;; ; don't split windows
;; (setq py-split-windows-on-execute-p t)
;; ; try to automagically figure out indentation
;; (setq py-smart-indentation t)

;; Setup el-py mode
;; ------------------------------
;; Sets up elpy
(require 'setup-elpy)



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
;;(require 'helm-spotify-custom)

 
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

(require 'expand-region)
(require 'multiple-cursors)
(require 'wc-mode)


;; Enable upcase and downcase region (C-x C-u & C-x C-l)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
