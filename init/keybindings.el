;; --------------------------------
;; General keybindings not tied to some major mode
;; --------------------------------

;; Use ibuffer instead of temporary buffer popup
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Move text up and down easily
;;(global-set-key (kbd "<M-up>") 'move-text-up


;; Switch to last used buffer
(global-set-key (kbd "C-x p")
		(lambda ()
		  (interactive)
		  (switch-to-buffer (other-buffer))))


;; Open previous buffer below/to the right instead of current buffer
(global-set-key (kbd "C-x 2")
		(lambda ()
		  (interactive)
		  (split-window-vertically)
		  (other-window 1 nil)
		  (switch-to-next-buffer)))
(global-set-key (kbd "C-x 3")
		(lambda ()
		  (interactive)
		  (split-window-horizontally)
		  (other-window 1 nil)
		  (switch-to-next-buffer)))

;; Join lines
(global-set-key (kbd "M-j")
		(lambda ()
		  (interactive)
		  (join-line -1)))


