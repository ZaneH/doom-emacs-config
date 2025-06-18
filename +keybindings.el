;;; $DOOMDIR/+keybindings.el -*- lexical-binding: t; -*-

;; SPC r i to edit multiple occurrences of a symbol
(map! :leader
      :prefix "r"
      :desc "iedit-mode" "i" #'iedit-mode)
(map! :v "SPC r i" #'iedit-mode)

;; SPC o j to open the current journal
(map! :leader
      :prefix "o"
      :desc "Open current journal" "j" #'org-journal-open-current-journal-file)
