;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-

;; Set column length and delete trailing lines
(setq-default fill-column 120
              delete-trailing-lines t)

(after! org
  ;; Set org directory
  (setq org-directory "~/repos/org/")

  ;; Break lines at 120 char
  (add-hook 'org-mode-hook #'auto-fill-mode)
  )

(after! org-journal
  ;; Documented function: https://github.com/bastibe/org-journal?tab=readme-ov-file#journal-capture-template
  (defun org-journal-find-location ()
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max)))

  ;; Capture templates
  (setq org-capture-templates '(("j" "Journal entry" plain (function org-journal-find-location)
                                 "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                                 :jump-to-captured t :immediate-finish t)
                                ))

  ;; Journal config
  (setq org-journal-file-format "%Y-%m-%d.org"
        org-journal-file-type 'weekly
        org-journal-start-on-weekday 7
        org-journal-encrypt-journal t)

  ;; Set GPG recipient
  (setq org-crypt-key "B2BE2AC8A")
  )

(after! org-roam
  ;; Set org roam to use brave browser for graph view
  (setq org-roam-graph-viewer "brave-browser")
  (setq org-roam-directory "~/repos/org/roam/")
  )

;; Add Hugo support for Org files
;; C-c C-e H to start export
(with-eval-after-load 'ox
  (require 'ox-hugo))
