;; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))


(defun replace-plus-in-tables ()
  (save-excursion
  (goto-char (point-min))
       (while (re-search-forward "-\\+-" nil t)
         (replace-match "-|-"))))


(defun fix-org-mode-tables-hook()
  "Replace + with | in tables"
  (when (eq major-mode 'org-mode)
    (replace-plus-in-tables)))


  
(add-hook 'before-save-hook 'fix-org-mode-tables-hook)


(autoload 'fsharp-mode "fsharp-mode" "Major mode for editing F# code." t)
(add-to-list 'auto-mode-alist '("\\.fs[iy]lx]?$" . fsharp-mode))


(provide 'mode-mappings)
