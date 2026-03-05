;; Corfu provides in-buffer completion UI (popup) using completion-at-point.
(use-package corfu
  :custom
  (corfu-auto t)           ; show popup automatically
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)    ; trigger after 2 characters
  (corfu-cycle t)          ; cycle through candidates
  (corfu-quit-no-match t)  ; quit if no match
  :init
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode 1); shows documentation next to completions
  ;; Sort by input history
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  )
