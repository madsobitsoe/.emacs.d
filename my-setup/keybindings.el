;; keybindings.el
;; ------------------------------
;; File that sets up my custom keybindings for emacs
;;

;; switch to last used buffer
(global-set-key (kbd "C-x p") 'switch-to-last-buffer)

;; Open previous buffer on right or below
(global-set-key (kbd "C-x 2") 'open-prev-buffer-below)
(global-set-key (kbd "C-x 3") 'open-prev-buffer-on-right)

;; Open shell on right or below
(global-set-key (kbd "C-c C-x 2") 'open-shell-below)
(global-set-key (kbd "C-c C-x 3") 'open-shell-on-right)


;; Setup rotate windows
;; Enables to switch buffers around when they open wrong
;; OCD-curing functionality
(global-set-key (kbd "C-c C-r") 'rotate-windows)

;; Create newlines mid-sentence both above and below
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-M-return>") 'open-line-above) 

;; Scroll other buffer upwards
(global-set-key (kbd "C-M-f")  (lambda () (interactive) (scroll-other-window -36)))


;; make ibuffer default for buffer change
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; change search and search regexp
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


;; multiple-cursors
;; ------------------------------
;; add a cursor to each line in region
(global-set-key (kbd "C-c C-a") 'mc/edit-lines)
;; add cursors based on keywords in buffer
(global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
(global-set-key (kbd "C-Æ") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-æ") 'mc/mark-all-like-this)


;; Expand region
;; ------------------------------
(global-set-key (kbd "C-=") 'er/expand-region)


;; Webjump
;; ------------------------------
(global-set-key (kbd "C-c j") 'webjump)

;; Yank-on-right
;; ------------------------------
(global-set-key (kbd "C-c C-.") 'yank-on-right)


;; For each occurence
;; ------------------------------
;; (global-set-key (kbd "C-c o") 'for-each-occurence)

;; Kill other buffers
;; ------------------------------
(global-set-key (kbd "C-x C-l") 'kill-other-buffers)


;; smex - Smart M-x
(global-set-key (kbd "M-x") 'smex)

;; kills current buffer and deletes file
;; ------------------------------
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Sets locate which i'm setting to use spotlight elsewhere
;; ------------------------------
(global-set-key (kbd "M-s M-s") 'locate)

;; Indents whole buffer - same as C-x h TAB
;; ------------------------------
(global-set-key (kbd "C-c C-c i") 'indent-buffer)


;; Ace-jump mode - jump to char by char
(global-set-key (kbd "C-f") 'ace-jump-mode)

;; Joins the current line and the line below at cursor
(global-set-key (kbd "M-j")
		(lambda ()
		  (interactive)
                  (join-line -1)))

;; Setup hotkeys for splitpane docview
;; ------------------------------
(fset 'doc-prev "\C-xo\C-x[\C-xo")
(fset 'doc-next "\C-xo\C-x]\C-xo")
(global-set-key (kbd "M-å") 'doc-prev)
(global-set-key (kbd "M-ø") 'doc-next) 


;; Move lines up and down
(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)


(global-set-key (kbd "C-c C-e") 'replace-last-sexp)


(provide 'keybindings)
