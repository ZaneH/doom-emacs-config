;;; $DOOMDIR/+sql.el -*- lexical-binding: t; -*-

;; Use sqlite-mode for SQLite files
(use-package sqlite-mode
  :config
  (defun my/sqlite-view-file-magically ()
    "Runs `sqlite-mode-open-file' on the file name visited by the current buffer, killing it."
    (require 'sqlite-mode)
    (let ((file-name buffer-file-name))
      (kill-current-buffer)
      (sqlite-mode-open-file file-name)))

  (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . my/sqlite-view-file-magically)))

(after! sqlite-mode
  (require 'sqlite-mode-extras)
  (set-popup-rule! "^\\*SQLite " :ignore t)
  (map! :map sqlite-mode-map
        :nvi "h" #'sqlite-mode-extras-backtab-dwim
        :nvi "j" #'next-line
        :nvi "k" #'previous-line
        :nvi "l" #'sqlite-mode-extras-tab-dwim
        :nvi "a" #'sqlite-mode-extras-add-row
        :nvi "r" #'sqlite-mode-extras-refresh
        :nvi "D" #'sqlite-mode-delete
        :nvi "E" #'sqlite-mode-extras-execute
        :nvi "C" #'sqlite-mode-extras-compose-and-execute
        :nvi "S" #'sqlite-mode-extras-execute-and-display-select-query
        :nvi "<backtab>" #'sqlite-mode-extras-backtab-dwim
        :nvi "<tab>" #'sqlite-mode-extras-tab-dwim
        :nvi "RET" #'sqlite-mode-extras-ret-dwim
        :nvi "DEL" #'sqlite-mode-delete))
