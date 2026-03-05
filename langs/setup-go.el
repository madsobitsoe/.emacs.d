;; Keybindings active in go-mode (via eglot/xref):
;;   C-c C-r  eglot-rename          Rename symbol at point
;;   M-.      xref-find-definitions Go to definition
;;   M-,      xref-go-back          Jump back after M-.
;;   M-?      xref-find-references  Find all references
;;   C-h .    eldoc-doc-buffer      Show documentation

(use-package go-mode
  :defer t
  :bind (:map go-mode-map
              ("C-c C-r" . eglot-rename))
  :hook
  (go-mode . eglot-ensure)
  (go-mode . (lambda ()
               (setq tab-width 2)
               (add-hook 'before-save-hook #'eglot-format-buffer nil t))))

(use-package eglot
  :ensure nil  ;; built-in since Emacs 29
  :commands eglot-ensure
  :config
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls"))))
