# Doom Emacs Config

This [Doom Emacs](https://github.com/doomemacs/doomemacs) config focuses on web dev languages like React, Go and TypeScript.
The config provides the standard components you'd expect from an IDE (IntelliSense, file explorer, integrated terminal, etc.)
as well as [GPTel](https://github.com/karthink/gptel) + [MCP.el](https://github.com/lizqwerscott/mcp.el) integration to make
AI-assisted coding even more capable than GitHub Copilot's VSCode extension.

**✨ Highlights:** AI integration, spellcheck, vterm integration, Discord Rich Presence, org-mode, multi-cursor edits,
venv integration, undo tree, treemacs.

## Setup

```sh
git clone https://github.com/ZaneH/doom-emacs-config.git ~/.doom.d
touch ~/.doom.d/secrets.el # optional, see Secrets section below
```

### Copilot Setup

Use <kbd>M-x copilot-login</kbd> to link your Copilot account. This is only needed once.

### Tree Sitter Setup

Use <kbd>M-x tree-sitter-langs-install-latest-grammar</kbd> to setup tree-sitter.

### Secrets

For services like OpenRouter or Anthropic's API to be accessible to GPTel, they require an API key. To prevent these keys from
ending up in git, create a dedicated `~/.doom.d/secrets.el` file. If `secrets.el` exists, the `config.el` will load them.

```elisp
;;; $DOOMDIR/secrets.el -*- lexical-binding: t; -*-

(defun my/gemini-api-key ()
  "Return my Gemini API key"
  "KEY_GOES_HERE"
)

(defun my/anthropic-api-key ()
  "Return my Anthropic API key."
  "KEY_GOES_HERE"
)

(defun my/openrouter-api-key ()
  "Return my Openrouter API key."
  "KEY_GOES_HERE"
)
```
