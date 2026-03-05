;; ------------------------------------------------
;; Mac-specific settings and rebinds
;; ------------------------------------------------


(setq mac-emulate-three-button-mouse t
      mac-option-modifier 'none
      mac-command-modifier 'meta
      mac-frame-tabbing nil
      ;; Disable command-h passthrough
      mac-pass-command-to-system nil)

;; Use a hook so the font is set on every new frame (including emacsclient
;; connections in daemon/server mode). The direct call handles the initial
;; frame when Emacs is started normally (non-daemon).
(defun mads/setup-frame-font (&optional frame)
  (when (display-graphic-p frame)
    (set-frame-font "Menlo 16" nil t)))

(add-hook 'after-make-frame-functions #'mads/setup-frame-font)
(mads/setup-frame-font)

(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-variables
   '("PATH"
     "MANPATH"
     "GOPATH"))
  :config
  (when (memq window-system '(ns mac))
    (exec-path-from-shell-initialize)))
