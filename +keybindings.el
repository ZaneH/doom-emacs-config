;;; $DOOMDIR/+keybindings.el -*- lexical-binding: t; -*-

;; Enable iedit mode for editing multiple occurrences of a symbol
(map! :leader
      :prefix "r"
      :desc "iedit-mode" "i" #'iedit-mode)

(map! :v "SPC r i" #'iedit-mode)
