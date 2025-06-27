;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-

;; == Org Mode Configuration ==
(after! org
  ;; Set org directory
  (setq org-directory "~/repos/org/")

  ;; Capture templates
  (setq org-capture-templates '(("j" "Journal entry" plain (function org-journal-find-location)
                                 "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                                 :jump-to-captured t :immediate-finish t)
                                ("p" "Personal todo" entry (file+headline "~/repos/org/todo.org" "Personal")
                                 "* TODO %^{Title}\n%U\n%i%?"
                                 :empty-lines 1)
                                ("w" "Work todo" entry (file+headline "~/repos/org/todo.org" "Work")
                                 "* TODO %^{Title}\n%U\n%i%?"
                                 :empty-lines 1)
                                ("n" "Note" entry (file+headline "~/repos/org/notes.org" "Notes")
                                 "* %^{Title}\n%U\n%i%?"
                                 :empty-lines 1 :immediate-finish t)
                                ))
  )

;; == Org Journal Configuration ==
(after! org-journal
  ;; Documented function: https://github.com/bastibe/org-journal?tab=readme-ov-file#journal-capture-template
  (defun org-journal-find-location ()
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max)))

  ;; Journal config
  (setq org-journal-file-format "%Y-%m-%d.org"
        org-journal-file-type 'weekly
        org-journal-start-on-weekday 7
        org-journal-encrypt-journal t)

  ;; Set GPG recipient
  (setq org-crypt-key "B2BE2AC8A")
  )

;; == Org Roam Configuration ==
(after! org-roam
  ;; Set org roam to use brave browser for graph view
  (setq org-roam-graph-viewer "brave-browser")
  (setq org-roam-directory "~/repos/org/roam/")
  )

;; == Org Archive Configuration ==
(use-package! org-archive
  :after org
  :config
  (setq org-archive-location "archive.org::datetree/"))

;; == Org Export Configuration ==
(with-eval-after-load 'ox
  (require 'ox-hugo))

;; == Org CV Configuration ==
(use-package! ox-moderncv
  :after org)
