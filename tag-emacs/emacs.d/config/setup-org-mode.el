(use-package org
  :ensure t
  :diminish org-indent-mode
  :init (setq org-modules '(org-w3m
                            org-bbdb
                            org-bibtex
                            org-docview
                            org-info
                            org-protocol
                            org-habit)))

(use-package org-bullets
  :ensure t
  :init (setq org-bullets-bullet-list '("●" "◉" "○"))
  :config (progn
            (org-bullets-mode)
            (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(use-package org-gcal
  :disabled t)

(setq org-startup-indented t
      org-src-window-setup 'current-window
      org-catch-invisible-edits 'show-and-error
      org-tags-column -100
      org-show-context-detail '((agenda . ancestors)
                                (bookmark-jump . lineage)
                                (isearch . lineage)
                                (default . ancestors))
      org-deadline-warning-days 10
      org-global-properties '(("Effort_ALL" . "0 0:05 0:15 0:30 1:00 2:00 4:00 6:00 8:00")))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))
(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode)))


;;
;; Org Files
;;
(setq my/org-refile (concat org-directory "/refile.org")

      my/org-personal-todo (concat org-directory "/todo/Personal.org")
      my/org-teammobot-todo (concat org-directory "/todo/TeamMobot.org")

      my/org-todo-files (list my/org-personal-todo
                              my/org-teammobot-todo)

      my/org-notes (concat org-directory "/notes.org")
      my/org-errands (concat org-directory "/errands.org")
      my/org-someday (concat org-directory "/someday.org"))


;;
;; Column View
;;
(setq org-columns-default-format "%55ITEM(TASK) %TODO(STATE) %3PRIORITY %Effort(EFFORT){:} %TAGS"
      org-agenda-overriding-columns-format "%CATEGORY %70ITEM(TASK) %TODO(STATE) %3PRIORITY %Effort(EFFORT){:} %TAGS"
      org-agenda-columns-add-appointments-to-effort-sum t
      org-effort-property "Effort"
      org-agenda-default-appointment-duration 60)


;;
;; Tags
;;
(setq org-tag-alist '(("project" . ?p)

                      (:startgroup)
                      ("reading" . ?r) ("coding" . ?c) ("writing" . ?w) ("viewing" . ?v)
                      (:endgroup)

                      (:startgroup)
                      ("@errands" . ?e) ("@home" . ?h) ("@nyc2" . ?n) ("@office" . ?o)
                      (:endgroup)))

(setq org-use-tag-inheritance t
      org-tags-exclude-from-inheritance '("project" "ATTACH"))


;;
;; Capture
;;
(setq my/simple-task-template (list (concat "* TODO %?\n"
                                            ":PROPERTIES:\n:Created: %U\n:END:\n\n"
                                            "%i"))
      my/mail-task-template (list (concat "* TODO %? :email:\n"
                                          ":PROPERTIES:\n:Created: %U\n:END:\n"
                                          "[[%:link][%:description]]\n\n"))
      my/mail-reply-task-template (list (concat "* TODO Reply: [[%:link][%:description]] :email:\n"
                                                "SCHEDULED: %t\n"
                                                ":PROPERTIES:\n:Created: %U\n:END:\n\n")
                                        :immediate-finish t))

(setq org-capture-templates
      (list (append '("p" "Personal Task" entry (file+headline my/org-refile "Personal"))
                    my/simple-task-template)
            (append '("e" "Errands" entry (file my/org-errands))
                    my/simple-task-template)
            (append '("w" "Team Mobot Task" entry (file+headline my/org-refile "TeamMobot"))
                    my/simple-task-template)
            '("m" "Mail Tasks")
            (append '("mp" "Personal Mail Followup Task" entry (file+headline my/org-refile  "Personal"))
                    my/mail-task-template)
            (append '("mw" "Team Mobot Mail Followup Task" entry (file+headline my/org-refile  "TeamMobot"))
                    my/mail-task-template)
            '("r" "Mail Reply Tasks")
            (append '("rp" "Personal Mail Reply Task" entry (file+headline my/org-personal-todo  "Tasks"))
                    my/mail-reply-task-template)
            (append '("rw" "Team Mobot Mail Reply Task" entry (file+headline my/org-teammobot-todo  "Tasks"))
                    my/mail-reply-task-template)))

(setq org-refile-targets '((nil :maxlevel . 5)
                           (my/org-todo-files . (:maxlevel . 5))
                           (my/org-someday . (:maxlevel . 1))
                           (my/org-errands . (:maxlevel . 1)))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)


;;
;; Tasks
;;
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i!/!)" "|" "DONE(d!/!)")
                          (sequence "WAITING(w@/!)" "|" "CANCELED(c@/!)"))
      org-todo-keyword-faces '(("TODO" :foreground "orange red" :weight bold)
                               ("NEXT" :foreground "lawn green" :weight bold)
                               ("IN-PROGRESS" :foreground "navy" :background "sky blue" :weight bold)
                               ("WAITING" :foreground "yellow" :weight bold)
                               ("DONE" :foreground "dim gray")
                               ("CANCELED" :foreground "dim gray" :strike-through t))
      org-log-into-drawer 'LOGBOOK
      org-log-done-with-time t
      org-closed-keep-when-no-todo t
      org-treat-insert-todo-heading-as-state-change nil
      org-treat-S-cursor-todo-selection-as-state-change nil)

;; Habits
(setq org-habit-graph-column 58
      org-habit-preceding-days 21
      org-habit-following-days 7
      org-habit-show-habits-only-for-today t)


;;
;; Agenda
;;
(setq org-agenda-files (append my/org-todo-files
                               (list my/org-refile
                                     my/org-errands)))

(setq org-agenda-sticky nil
      org-agenda-timegrid-use-ampm t
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-todo-list-sublevels nil
      org-agenda-span 1
      org-agenda-compact-blocks nil
      org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t)

(setq org-stuck-projects '("project/-DONE-CANCELED" ("NEXT" "WAITING" "IN-PROGRESS") nil ""))

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down todo-state-down category-keep)
        (todo todo-state-down priority-down category-keep)
        (tags todo-state-down priority-down category-keep)
        (search category-keep)))

(setq org-agenda-custom-commands
      '(("x" "Complete Agenda"
         ((agenda ""
                  ((org-agenda-overriding-header "Complete Agenda")))
          (tags-todo "refile"
                     ((org-agenda-overriding-header "To Refile")))))


        ("p" . "Personal Agendas")

        ("pa" "Personal Agenda"
         ((agenda ""
                  ((org-agenda-overriding-header "Personal Agenda")))
          (tags-todo "refile"
                     ((org-agenda-overriding-header "To Refile")))
          (todo "NEXT|IN-PROGRESS"
                ((org-agenda-overriding-header "Unscheduled Tasks (NEXT, IN-PROGRESS)")
                 (org-agenda-todo-ignore-scheduled 'all))))
         ((org-agenda-tag-filter-preset '("-TMB"))))

        ("pw" "Personal Agenda - Office"
         ((agenda ""
                  ((org-agenda-overriding-header "Personal Agenda - Office")))
          (tags-todo "refile"
                     ((org-agenda-overriding-header "To Refile")))
          (todo "NEXT|IN-PROGRESS"
                ((org-agenda-overriding-header "Unscheduled Tasks (NEXT, IN-PROGRESS)")
                 (org-agenda-todo-ignore-scheduled 'all))))
         ((org-agenda-tag-filter-preset '("-TMB" "-@home"))))

        ("pt" "Personal Upcoming Tasks" tags-todo "PER/!TODO|NEXT"
         ((org-agenda-overriding-header "Personal Upcoming Tasks")))

        ("pn" "Personal Unscheduled NEXT Tasks" tags-todo "PER/!NEXT"
         ((org-agenda-overriding-header "Personal Unscheduled NEXT Tasks")
          (org-agenda-todo-ignore-scheduled 'all)))


        ("w" . "Team Mobot Agendas")

        ("wa" "Team Mobot Agenda"
         ((agenda ""
                  ((org-agenda-overriding-header "Team Mobot Agenda")))
          (tags-todo "refile"
                     ((org-agenda-overriding-header "To Refile")))
          (todo "NEXT|IN-PROGRESS"
                ((org-agenda-overriding-header "Unscheduled Tasks (NEXT, IN-PROGRESS)")
                 (org-agenda-todo-ignore-scheduled 'all))))
         ((org-agenda-tag-filter-preset '("+TMB"))))

        ("wt" "Team Mobot Upcoming Tasks" tags-todo "TMB/!TODO|NEXT"
         ((org-agenda-overriding-header "Team Mobot Upcoming Tasks")))

        ("wn" "Team Mobot Unscheduled NEXT Tasks" tags-todo "TMB/!NEXT"
         ((org-agenda-overriding-header "Team Mobot Unscheduled NEXT Tasks")
          (org-agenda-todo-ignore-scheduled 'all)))


        ("W" "Agenda Planning View"
         ((stuck ""
                 ((org-agenda-overriding-header "Stuck Projects")))
          (todo "WAITING"
                ((org-agenda-overriding-header "WAITING Task To Follow Up")))
          (tags-todo "refile"
                     ((org-agenda-overriding-header "Captured Tasks To Refile")))
          (todo "NEXT|IN-PROGRESS"
                ((org-agenda-overriding-header "Unscheduled Tasks (NEXT, IN-PROGRESS)")
                 (org-agenda-todo-ignore-scheduled 'all)))))

        ("h" "Routines" agenda ""
         ((org-agenda-overriding-header "Routines")
          (org-habit-show-all-today t)
          (org-habit-show-habits-only-for-today t)
          (org-agenda-tag-filter-preset '("+routine"))
          (org-agenda-use-time-grid nil)))

        ("l" . "List Views")

        ("lp" "Projects List" tags "project/-DONE-CANCELED"
         ((org-agenda-overriding-header "Projects")
          (org-agenda-sorting-strategy '(category-keep alpha-up))))))


;;
;; My mail Message-ID link handler
;;
(org-add-link-type "message-id" 'my/org-message-id-open)

(defun my/org-message-id-open (msg-id)
  "Visit mail message with external MUA"
  (let ((cmd "mutt_for_msgid.sh"))
    (async-shell-command (concat cmd " " msg-id))))

(provide 'setup-org-mode)
