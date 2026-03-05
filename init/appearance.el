;; ------------------------------------------------
;; Set up appearance with doom-emacs themes
;; ------------------------------------------------

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t) ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled

  :config
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))



(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (inferior-ess-mode . rainbow-delimiters-mode)
  )
