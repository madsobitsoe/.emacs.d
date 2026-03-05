;; Turn off mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Don't show the splash screen
(setq inhibit-startup-message t)

;; Ask nicely before exiting
(setq confirm-kill-emacs 'y-or-n-p)

(setq initial-scratch-message
      ";; All your bytes are belong to us
")

;; Straight to *scratch*
(setq initial-buffer-choice t)

;; Is this a mac?
(setq is-mac (equal system-type 'darwin))

;; Turn off the bell
(setq ring-bell-function #'ignore)

;; Run as a daemon so client works
(server-start)

(defvar mads/load-path (file-name-directory (or load-file-name buffer-file-name))
  "Path of emacs config")
(when (not mads/load-path)
  (error "Cannot determine configuration path of emacs config: both load-file-name and buffer-file-name are nil"))


(defun mads/load (paths)
  "Load files relative to this emacs config."
  (when (not (listp paths))
    (setq paths (list paths)))
  (dolist (path paths)
    (condition-case err
	(load (expand-file-name path mads/load-path))
      (error (message "Failed to load config sub-library at %S: %S" path err)))))



;; Load files for initialization
(mads/load
 '("./init/sane-defaults.el"
   "./init/package.el"
   "./init/keybindings.el"
   "./init/appearance.el"
   "./init/setup-org.el"
   "./init/setup-vertico.el"
   "./init/setup-marginalia.el"
   "./init/setup-orderless.el"
   "./init/setup-multiple-cursors.el"
   "./init/setup-magit.el"
   "./init/setup-corfu.el"
   "./langs/setup-yaml.el"
   "./langs/setup-markdown.el"
   "./langs/setup-docker.el"
   "./langs/setup-go.el"
   "./langs/setup-terraform.el"
   ))

;; If this is a mac, do some mac stuff
(when is-mac
  (mads/load
   '("./init/setup-mac.el")
  ))

;; Store Emacs custom settings in a separate file
(setq custom-file (expand-file-name "custom.el" mads/load-path))
(when (file-exists-p custom-file)
  (load custom-file))
