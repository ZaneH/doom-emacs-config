;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/repos/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq doom-theme 'doom-moonlight)

(setq
 projectile-project-search-path
 '((
    "~/repos/personal/" . 2))
 )

(after! lsp-mode
  (setq lsp-completion-provider :capf))
(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

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

;; Set the default font for unicode characters
(after! unicode-fonts
  (set-fontset-font t 'unicode
                    (font-spec :family "JetBrainsMono Nerd Font" :size 15)
                    nil 'prepend))

;; Enable Elcord for Discord Rich Presence
(require 'elcord)
(elcord-mode)

(setq elcord-idle-timer 600
      elcord-idle-message "Chillin'"
      elcord-editor-icon "doom_cute_icon")

;; Enable iedit mode for editing multiple occurrences of a symbol
(map! :leader
      :prefix "r"
      :desc "iedit-mode" "i" #'iedit-mode)

(map! :v "SPC r i" #'iedit-mode)

;; Typescript and TSX support
(after! treesit
  (setq treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil))))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'lsp!))

;; Enable Copilot for code completion
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  )

;; Enable GPTel for AI conversations
(after! gptel
  (setq gptel-model 'claude-3.7-sonnet
        gptel-backend (gptel-make-gh-copilot "Copilot"))

  ;; Enable MCP servers for AI interactions
  (setq mcp-hub-servers
            '(("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
              ("mcp-server-reddit" . (:command "uvx" :args ("mcp-server-reddit")))
            ))
  (add-hook 'after-init-hook
            #'mcp-hub-start-all-server)

  (require 'gptel-integrations)
)
