;;; $DOOMDIR/+completion.el -*- lexical-binding: t; -*-

;; Completion (aka 'Intellisense' in Emacs)
(after! lsp-mode
  (setq lsp-completion-provider :capf))
(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1)

  ;; Use higher delay for certain modes
  (add-hook 'markdown-mode-hook
            (lambda () (setq-local company-idle-delay 5)))
  (add-hook 'org-mode-hook
            (lambda () (setq-local company-idle-delay 5)))
  (add-hook 'text-mode-hook
            (lambda () (setq-local company-idle-delay 5)))
  )
