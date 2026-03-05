;; Keybindings active in terraform-mode (via eglot/xref):
;;   C-c C-r  eglot-rename          Rename symbol at point
;;   M-.      xref-find-definitions Go to definition
;;   M-,      xref-go-back          Jump back after M-.
;;   M-?      xref-find-references  Find all references
;;   C-h .    eldoc-doc-buffer      Show documentation
;;
;; Requires terraform-ls installed: brew install hashicorp/tap/terraform-ls

(use-package terraform-mode
  :defer t
  :bind (:map terraform-mode-map
              ("C-c C-r" . eglot-rename))
  :hook
  (terraform-mode . eglot-ensure)
  (terraform-mode . (lambda ()
                      (setq tab-width 2))))

(use-package eglot
  :ensure nil
  :commands eglot-ensure
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve"))))
