;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-

;; Org configuration

(after! org
  ;; Set org directory
  (setq org-directory "~/repos/org/")
  (setq-default fill-column 120
                delete-trailing-lines t)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  )

(after! org-journal
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-time-prefix "* "
        org-journal-date-format "%a %m-%d-%Y"
        org-journal-file-format "%Y-%m-%d.org")

  )

(after! org-roam
  ;; Set org roam to use brave browser for graph view
  (setq org-roam-graph-viewer "brave-browser")
  (setq org-roam-directory "~/repos/org/roam/")
  )
