;; mac.el
;; ------------------------------
;; OSX specific settings

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


(provide 'mac)
