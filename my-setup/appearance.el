;; appearance.el
;; ------------------------------
;; Setup visuals


(setq visible-bell t
font-lock-maximum-decoration t 
color-theme-is-global t
truncate-partial-width-windows nil)

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


;; Setup smart modeline
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'respectful)
(sml/setup)



;; Set up various highligting
;; ============================================================================
;; Highlight matching parentheses when the point is on one of them
(show-paren-mode 1)
;; Highlight current line
(global-hl-line-mode 1)
;; Fucking rainbows
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; Hightlight numbers
(highlight-numbers-mode)
;; Highlight indentation
(highlight-indent-guides-mode)
;; Highlight chars above 80
(column-enforce-mode)


;; Set unfocused alpha value
(set-frame-parameter (selected-frame) 'alpha '(100 . 80))



(provide 'appearance)
