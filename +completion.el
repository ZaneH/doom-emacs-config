;;; $DOOMDIR/+completion.el -*- lexical-binding: t; -*-

;; Completion (aka 'Intellisense' in Emacs)
(after! lsp-mode
  (setq lsp-completion-provider :capf))
(after! company
  (setq company-idle-delay 5
        company-minimum-prefix-length 1))
