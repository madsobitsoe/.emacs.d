;; Keybindings active in python-mode (via eglot/xref):
;;   C-c C-r  eglot-rename          Rename symbol at point
;;   M-.      xref-find-definitions Go to definition
;;   M-,      xref-go-back          Jump back after M-.
;;   M-?      xref-find-references  Find all references
;;   C-h .    eldoc-doc-buffer      Show documentation

;; pet detects the project's .venv (uv, poetry, pyenv, etc.) and
;; exposes pet-executable-find so eglot/pyright use the right interpreter.
(use-package pet
  :ensure t)

(defun mads/python-mode-setup ()
  "Configure Python buffer: activate pet, set venv interpreter, start eglot."
  (when (fboundp 'pet-mode)
    (pet-mode +1))
  (let ((python-bin (if (fboundp 'pet-executable-find)
                        (or (pet-executable-find "python") "python3")
                      "python3")))
    (setq-local python-shell-interpreter python-bin)
    (setq-local eglot-workspace-configuration
                `(:python (:pythonPath ,python-bin))))
  (eglot-ensure))

(use-package python
  :ensure nil
  :bind (:map python-mode-map
              ("C-c C-r" . eglot-rename))
  :hook
  (python-base-mode . mads/python-mode-setup))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))
