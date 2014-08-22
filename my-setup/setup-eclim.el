;; Setup eclim
;; ------------------------------
;; Major mode for interacting with eclipse and snatching the nice features
;; 

(require 'eclim)
(require 'eclimd)
(global-eclim-mode)

;; Variables
(setq eclim-auto-save t
;;      eclim-executable "/opt/eclipse/eclim"
;;      eclimd-executable "/opt/eclipse/eclimd"
      eclimd-wait-for-process nil
;;      eclimd-default-workspace "~/Documents/KEA/Programmering/2.semester"
;;      eclim-use-yasnippet nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      )

;; Call the help framework with the settings above & activate
;; eclim-mode
(help-at-pt-set-timer)

;; Hook eclim up with auto complete mode
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)


;; setup company for auto-completion interface
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

(provide 'setup-eclim)



