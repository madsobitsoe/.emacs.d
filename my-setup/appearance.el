;; appearance.el
;; ------------------------------
;; Setup visuals


(setq visible-bell t
font-lock-maximum-decoration t 
color-theme-is-global t
truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))


(defun use-default-theme ()
  (interactive)
;;  (disable-theme 'prez)
  (load-theme 'default-black)
  (when (boundp 'cccamera/default-font)
    (set-face-attribute 'default nil :font cccamera/default-font)))


(use-default-theme)


;; Sets the title fram to full file path
(setq frame-title-format '("Buffer: %b %+%+ File: %f"))

;; enable line numbers
(global-linum-mode t)
(setq linum-format "%d")

(setq redisplay-dont-pause t)

;; Highlight matching parentheses when the point is on one of them
(show-paren-mode 1)

;; Setup smart modeline
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'smart-mode-line-powerline)
(sml/setup)


(provide 'appearance)
