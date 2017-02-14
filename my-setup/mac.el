;; mac.el
;; ------------------------------
;; OSX specific settings

(require 'dash)

;; Setup mackeyboard
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)


;; Sets the default directory to home, not /
(setq default-directory "~/")

;; Set locate command to use spotlight
(setq locate-command "mdfind")

;; Setup nice macfont
(when window-system
  (setq cccamera/default-font "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font cccamera/default-font))

;; Delete files to trash -- No more fuck-ups
(setq delete-by-moving-to-trash t trash-directory "~/.Trash/emacs")

;; Ignore crappy .DS_Store files in ido
;;(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Avoid opening too many files in new window - I want one emacs and millions of buffers
(setq ns-pop-up-frames nil)

(provide 'mac)
