(require 'fsharp-mode)

;; (setq inferior-fsharp-program "/Library/Frameworks/Mono.framework/Versions/Current/Commands/fsharpi --readline-")
;; (setq fsharp-compiler "/Library/Frameworks/Mono.framework/Versions/Current/Commands/fsharpc")
(setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/bin/fsharpc")
(setq fsharp-doc-idle-delay 0)
(setq fsharp-ac-intellisense-enabled t)

(provide 'setup-fsharp)
