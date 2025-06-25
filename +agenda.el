;;; $DOOMDIR/+agenda.el -*- lexical-binding: t; -*-

;; Inspired by: https://www.youtube.com/watch?v=a_WNtuefREM

;; Org Super Agenda
(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups '((:name "Personal "
                                   :and(:category "Personal")
                                   :order 3)

                                  (:name "Work "
                                   :and(:category "Work")
                                   :order 2)

                                  (:name "Today"
                                   :time-grid t
                                   :scheduled today
                                   :order 1)

                                  (:name "Overdue"
                                   :deadline past
                                   :order 0)

                                  (:name "Due Soon"
                                   :deadline future
                                   :order -1)

                                  (:name "Unscheduled"
                                   :scheduled nil
                                   :order -2)

                                  (:name "Stuck"
                                   :todo "WAITING|HOLD|SOMEDAY"
                                   :order -3)))
  (org-super-agenda-mode)

  (setq org-agenda-files '("~/repos/org/todo.org"))

  (setq org-agenda-span 1
        org-agenda-start-day "+0d"
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-deadline-is-shown t)

  ;; Hide extra information
  (setq org-agenda-current-time-string ""
        org-agenda-time-grid '((daily) () "" "")
        org-agenda-prefix-format '(
                                   (agenda . "  %?-2i %t ")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")
                                   ))

  ;; Hide tags
  (setq org-agenda-hide-tags-regexp ".*")

  ;; Set icons
  (setq org-agenda-category-icon-alist
        `(("Personal" ,(list (nerd-icons-faicon "nf-fa-home" :height 0.9)) nil nil :ascent center)
          ("Work" ,(list (nerd-icons-faicon "nf-fa-briefcase" :height 0.9)) nil nil :ascent center)))

  (custom-set-faces!
    '(org-agenda-date :inherit outline-1 :height 1.15)
    '(org-agenda-date-today :inherit diary :height 1.15)
    '(org-agenda-date-weekend :inherit outline-2 :height 1.15)
    '(org-agenda-date-weekend-today :inherit outline-4 :height 1.15)
    '(org-super-agenda-header :inherit custom-button :weight bold :height 1.05)
    )

  ;; Center agenda buffer
  (add-hook 'org-agenda-mode-hook 'olivetti-mode)
  )

(map! :desc "Next line"
      :map org-super-agenda-header-map
      "j" 'org-agenda-next-line)

(map! :desc "Previous line"
      :map org-super-agenda-header-map
      "k" 'org-agenda-previous-line)
