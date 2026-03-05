(use-package multiple-cursors
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  (("C-<" . mc/mark-next-like-this)
   ("C->" . mc/mark-previous-like-this)
   )
  )
