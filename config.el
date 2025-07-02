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
;; (setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

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

;; Utility function
(defun load-if-exists (f)
  (let ((file (expand-file-name f)))
    (when (file-exists-p file)
      (load-file file))))

;; Load secrets if they exist
;; You can use this to store sensitive information like API keys or private configurations.
(load-if-exists "~/.doom.d/secrets.el")

(load! "+completion")
(load! "+copilot")
(load! "+git")
(load! "+gptel")
(load! "+keybindings")
(load! "+lsp")
(load! "+org")
(load! "+agenda")
(load! "+python")
(load! "+term")
(load! "+sql")

;; Set theme
(setq doom-theme 'doom-tokyo-night)

;; Project search path
(setq projectile-project-search-path '("~/repos/personal/"
                                       "~/repos/personal/contrib/"
                                       "~/repos/org/roam/"
                                       "~/repos/scrap/"
                                       "~/repos/work/"))


;; Set the default font for unicode characters
(after! unicode-fonts
  (set-fontset-font t 'unicode
                    (font-spec :family "JetBrainsMono Nerd Font" :size 15)
                    nil 'prepend))

;; Set column length and delete trailing lines
(setq-default fill-column 120
              delete-trailing-lines t)

;; Enable auto-fill mode in these modes
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Enable and configure Discord Rich Presence
(require 'elcord)
(elcord-mode)
(setq elcord-idle-timer 600
      elcord-idle-message "Chillin'"
      elcord-editor-icon "doom_cute_icon")

;; Use markdown for .mdc files
(add-to-list 'auto-mode-alist '("\\.mdc\\'" . markdown-mode))

;; Use .dir-locals.el for local variables
(setq-default enable-local-variables t)

