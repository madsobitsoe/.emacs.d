;; latex.el
;; ===========================================================================
;; Latex setup file

(load "auctex.el" nil t t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'lwc-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTex t)
(setq reftex-toc-split-windows-horizontally t
      reftex-toc-split-windows-fraction 0.62)
(setq reftex-cite-format 'natbib
      reftex-sort-bibtex-matches 'reverse-year)

;; using \eqref{} instead of (\ref{})
(setq reftex-label-alist '((nil ?e nil "~\\eqref{%s}" nil nil)))

;; for parentheses
(setq LaTeX-electric-left-right-brace 1)

;; for inserting $|$
(add-hook 'LaTeX-mode-hook
          (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                          (cons "$" "$"))))

;; choose pdflatex
(setq TeX-PDF-mode t)

;; For emacs to know 
;;(setenv "PATH"
;;	(concat
;;	 "/Library/TeX/Distributions/Programs/texbin" ":" (getenv "PATH")))
;; Since El Capitan, the TeX path is changed to the above. 
;; The following is just for backward compatibility
;; (setenv "PATH"
;; 	(concat
;; 	 "/usr/texbin" ":" (getenv "PATH")))

;; Make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -outdir=aux -synctex=1 -pvc -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
;; (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
;; (setq TeX-view-program-list
;;      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))

;; ;; To invoke Skim using shift-command-click
;; (add-hook 'LaTeX-mode-hook
;;           (lambda () (local-set-key (kbd "<S-s-mouse-1>") #'TeX-view)))

;; Error handling
(setq TeX-display-help nil)

;; Clean things up
(eval-after-load 'latex
  '(setq LaTeX-clean-intermediate-suffixes
     (append LaTeX-clean-intermediate-suffixes (list "\\.fdb_latexmk" "\\.rel" "\\.tex~"))))


;; Set RefTeX ToC keybinding
(eval-after-load 'latex '(define-key LaTeX-mode-map (kbd "C-c t") 'reftex-toc))

;; Some ispell setup
;; (require 'ispell)
;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=da" "-W" "3" "--mode=tex"))


(provide 'setup-latex)
