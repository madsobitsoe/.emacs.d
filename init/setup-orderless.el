(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
  (orderless-matching-styles '(orderless-prefixes orderless-initialism orderless-flex)))
