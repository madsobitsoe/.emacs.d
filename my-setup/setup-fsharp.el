(require 'fsharp-mode)

(setq inferior-fsharp-program "/Library/Frameworks/Mono.framework/Versions/Current/Commands/fsharpi --readline-")
(setq fsharp-compiler "/Library/Frameworks/Mono.framework/Versions/Current/Commands/fsharpc")
(setq fsharp-doc-idle-delay 0)
(setq fsharp-ac-intellisense-enabled nil)

(provide 'setup-fsharp)
