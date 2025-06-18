;;; $DOOMDIR/+term.el -*- lexical-binding: t; -*-

;; Enable vterm with custom settings
(after! vterm
  (setq vterm-max-scrollback 10000
        vterm-shell "/home/me/.nix-profile/bin/zsh"
        vterm-timer-delay 0.01)

  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Set the font for vterm
              (set (make-local-variable 'buffer-face-mode-face) '(:family "JetBrainsMono Nerd Font"))
              (buffer-face-mode t)))
  )
