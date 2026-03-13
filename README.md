# My Emacs Config

Personal Emacs configuration. Feel free to grab pieces you find useful.

## Structure

```
.emacs.d/
├── init.el          # Entry point
├── custom.el        # Emacs-managed custom settings (auto-generated)
├── init/            # Core setup files
└── langs/           # Language-specific setup files
```

## Package Management

Uses the built-in `package.el` with MELPA, and `use-package` (`:ensure t` by default). Local packages can be dropped in a `packages/` directory next to `init.el`.

## Core Setup (`init/`)

| File | What it does |
|------|-------------|
| `sane-defaults.el` | UTF-8, line numbers, delete-selection, subword-mode, undo-tree, backup/tmp dirs |
| `package.el` | MELPA + use-package configuration |
| `keybindings.el` | Global keybindings (see below) |
| `appearance.el` | doom-themes (available), catppuccin-theme (Frappe, default), rainbow-delimiters |
| `setup-org.el` | org-mode with agenda (`~/org/main.org`, `~/org/hours.org`) |
| `setup-vertico.el` | Vertico minibuffer completion + vertico-directory |
| `setup-marginalia.el` | Marginalia annotations in minibuffer |
| `setup-orderless.el` | Orderless completion styles (prefixes, initialism, flex) |
| `setup-multiple-cursors.el` | Multiple cursors |
| `setup-magit.el` | Magit git interface |
| `setup-corfu.el` | Corfu in-buffer completion popup with history and docs |
| `setup-mac.el` | macOS-specific settings (loaded only on macOS) |

## Language Support (`langs/`)

| File | Language | Notes |
|------|----------|-------|
| `setup-go.el` | Go | go-mode + eglot/gopls, format on save |
| `setup-terraform.el` | Terraform | terraform-mode + eglot/terraform-ls (`brew install hashicorp/tap/terraform-ls`) |
| `setup-yaml.el` | YAML | yaml-mode |
| `setup-markdown.el` | Markdown | markdown-mode |
| `setup-docker.el` | Docker | dockerfile-mode |

## Key Bindings

### Global

| Key | Action |
|-----|--------|
| `C-x C-b` | ibuffer |
| `C-x p` | Switch to last used buffer |
| `C-x 2` | Split vertically, show next buffer in new window |
| `C-x 3` | Split horizontally, show next buffer in new window |
| `M-j` | Join next line onto current line |
| `C-c a` | org-agenda |
| `C-x g` / `C-x C-g` | magit-status |

### Multiple Cursors

| Key | Action |
|-----|--------|
| `C-<` | Mark next like this |
| `C->` | Mark previous like this |

### Go / Terraform (via eglot)

| Key | Action |
|-----|--------|
| `C-c C-r` | Rename symbol |
| `M-.` | Go to definition |
| `M-,` | Jump back |
| `M-?` | Find references |
| `C-h .` | Show documentation |

## macOS Notes

- Command key mapped to Meta
- Option key left as-is
- Font: Menlo 16
- `exec-path-from-shell` syncs `PATH`, `MANPATH`, `GOPATH` from shell
- Emacs runs as a server/daemon (`server-start`)
