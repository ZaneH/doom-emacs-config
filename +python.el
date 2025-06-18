;;; $DOOMDIR/+python.el -*- lexical-binding: t; -*-

;; Configure pyenv to use the virtual environment in the project root
(add-hook 'python-mode-hook
          (lambda ()
            (when (file-exists-p (expand-file-name "venv" (projectile-project-root)))
              (pyvenv-activate (expand-file-name "venv" (projectile-project-root))))))
