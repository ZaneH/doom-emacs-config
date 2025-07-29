;;; $DOOMDIR/+lsp.el -*- lexical-binding: t; -*-

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

(use-package! lsp-tailwindcss
  :init
  ;; lsp-tailwindcss config
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-skip-config-check t)
  )

(define-derived-mode tiltfile-mode
  python-mode "tiltfile"
  "Major mode for Tilt Dev."
  (setq-local case-fold-search nil))

(add-to-list 'auto-mode-alist '("Tiltfile$" . tiltfile-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(tiltfile-mode . "tiltfile"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection `("tilt" "lsp" "start"))
                    :activation-fn (lsp-activate-on "tiltfile")
                    :server-id 'tilt-lsp)))

;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 1))
