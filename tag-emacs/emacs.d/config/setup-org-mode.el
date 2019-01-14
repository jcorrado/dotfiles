(use-package org
  :ensure t
  :diminish org-indent-mode
  :init (setq org-modules '(org-w3m
                            org-bbdb
                            org-bibtex
                            org-docview
                            org-info
                            org-protocol
                            org-habit
                            org-mobile)))

(use-package org-bullets
  :ensure t
  :init (setq org-bullets-bullet-list '("●" "◉" "○"))
  :config (progn
            (org-bullets-mode)
            (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(use-package org-gcal :ensure t)

(setq org-startup-indented t
      org-src-window-setup 'current-window
      org-catch-invisible-edits 'show-and-error
      org-tags-column -100
      org-show-context-detail '((agenda . ancestors)
                                (bookmark-jump . lineage)
                                (isearch . lineage)
                                (default . ancestors))
      org-deadline-warning-days 10)

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
      my/org-birchbox-todo (concat org-directory "/todo/Birchbox.org")
      my/org-empatico-todo (concat org-directory "/todo/Empatico.org")

      my/org-todo-files (list my/org-personal-todo
                              my/org-birchbox-todo
                              my/org-empatico-todo)

      my/org-notes (concat org-directory "/notes.org")
      my/org-errands (concat org-directory "/errands.org")
      my/org-someday (concat org-directory "/someday.org"))

;;
;; Tags
;;
(setq org-tag-alist '(("project" . ?p)

                      (:startgroup)
                      ("reading" . ?r) ("coding" . ?c) ("writing" . ?w)
                      (:endgroup)

                      (:startgroup)
                      ("@errands" . ?e) ("@home" . ?h) ("@nyc2" . ?n)
                      (:endgroup)))

(setq org-use-tag-inheritance t
      org-tags-exclude-from-inheritance '("project"))


;;
;; Capture
;;
(setq my/simple-task-template (list (concat "* TODO %^{Task Summary}\n"
                                            ":PROPERTIES:\n:Created: %U\n:END:\n"
                                            "%?\n\n"
                                            "%i"))
      my/mail-task-template (list (concat "* TODO %^{Task Summary} :email:\n"
                                          ":PROPERTIES:\n:Created: %U\n:END:\n"
                                          "[[%:link][%:description]]\n\n"
                                          "%?\n"))
      my/mail-reply-task-template (list (concat "* TODO Reply: [[%:link][%:description]] :email:\n"
                                                "SCHEDULED: %t\n"
                                                ":PROPERTIES:\n:Created: %U\n:END:\n")
                                        :immediate-finish t))

(setq org-capture-templates
      (list (append '("p" "Personal Task" entry (file+headline my/org-refile "Personal"))
                    my/simple-task-template)
            (append '("e" "Errands" entry (file my/org-errands))
                    my/simple-task-template)
            '("w" "Work Tasks")
            (append '("wb" "Birchbox Task" entry (file+headline my/org-refile "Birchbox"))
                    my/simple-task-template)
            (append '("we" "Empatico Task" entry (file+headline my/org-refile "Empatico"))
                    my/simple-task-template)
            '("m" "Mail Tasks")
            (append '("mp" "Personal Mail Followup Task" entry (file+headline my/org-refile  "Personal"))
                    my/mail-task-template)
            (append '("mb" "Birchbox Mail Followup Task" entry (file+headline my/org-refile  "Birchbox"))
                    my/mail-task-template)
            '("r" "Mail Reply Tasks")
            (append '("rp" "Personal Mail Reply Task" entry (file+headline my/org-personal-todo  "Tasks"))
                    my/mail-reply-task-template)
            (append '("rb" "Birchbox Mail Reply Task" entry (file+headline my/org-birchbox-todo  "Tasks"))
                    my/mail-reply-task-template)))

(setq org-refile-targets '((nil :maxlevel . 5)
                           (my/org-todo-files . (:maxlevel . 5))
                           (my/org-someday . (:maxlevel . 1)))
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
                                     my/org-errands
                                     org-gcal-dir)))

(setq org-agenda-sticky nil
      org-agenda-timegrid-use-ampm t
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-todo-list-sublevels nil
      org-agenda-span 1
      org-agenda-compact-blocks nil)

(setq org-stuck-projects '("project/-DONE-CANCELED" ("NEXT") nil ""))

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up todo-state-down priority-down category-keep)
        (todo todo-state-down priority-down category-keep)
        (tags todo-state-down priority-down category-keep)
        (search category-keep)))

(setq org-agenda-custom-commands
      '(("p" . "Personal Agendas")
        ("pt" "Personal Upcoming TODO, NEXT Tasks" tags-todo "PER/!TODO|NEXT"
         ((org-agenda-overriding-header "Personal Upcoming Tasks")))
        ("pn" "Personal Unscheduled NEXT Tasks" tags-todo "PER/!NEXT"
         ((org-agenda-overriding-header "Personal Unscheduled NEXT Tasks")
          (org-agenda-todo-ignore-scheduled 'all)))

        ("h" "Routines" agenda ""
         ((org-agenda-overriding-header "Routines")
          (org-habit-show-all-today t)
          (org-habit-show-habits-only-for-today t)
          (org-agenda-tag-filter-preset '("+routine"))
          (org-agenda-use-time-grid nil)))

        ("b" . "Birchbox Agendas")
        ("bt" "Birchbox Upcoming TODO, NEXT Tasks" tags-todo "BBX/!TODO|NEXT"
         ((org-agenda-overriding-header "Birchbox Upcoming Tasks")))
        ("bn" "Birchbox Unscheduled NEXT Tasks" tags-todo "BBX/!NEXT"
         ((org-agenda-overriding-header "Birchbox Unscheduled NEXT Tasks")
          (org-agenda-todo-ignore-scheduled 'all)))

        ("e" . "Empatico Agendas")
        ("et" "Empatico Upcoming TODO, NEXT Tasks" tags-todo "EMP/!TODO|NEXT"
         ((org-agenda-overriding-header "Empatico Upcoming Tasks")))
        ("en" "Empatico Unscheduled NEXT Tasks" tags-todo "EMP/!NEXT"
         ((org-agenda-overriding-header "Empatico Unscheduled NEXT Tasks")
          (org-agenda-todo-ignore-scheduled 'all)))

        ("w" "Agenda Planning View"
         ((stuck ""
                 ((org-agenda-overriding-header "Stuck Projects")))
          (todo "WAITING"
                ((org-agenda-overriding-header "WAITING Task To Follow Up")))
          (tags-todo "refile"
                     ((org-agenda-overriding-header "Captured Tasks To Refile")))))

        ("x" "Agenda + Task View"
         ((agenda)
          (tags-todo "refile"
                     ((org-agenda-overriding-header "Captured Tasks To Refile")))
          (todo "NEXT|IN-PROGRESS"
                ((org-agenda-overriding-header "Unscheduled NEXT, IN-PROGRESS Tasks")
                 (org-agenda-todo-ignore-scheduled 'all)))))))


;;
;; My mail Message-ID link handler
;;
(org-add-link-type "message-id" 'my/org-message-id-open)

(defun my/org-message-id-open (msg-id)
  "Visit mail message with external MUA"
  (let ((cmd "mutt_for_msgid.sh"))
    (shell-command (concat cmd " " msg-id))))


;;
;; MobileOrg
;;
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"
      org-mobile-files (list my/org-errands))

;; Reformat items captured by MobileOrg
;;
;; Based on https://www.emacswiki.org/emacs/mobileorg
(defun my/org-convert-incoming-mobile-items (file)
  "Convert incoming MobileOrg items to tasks and move them to file"
  (interactive)
  (with-current-buffer (find-file-noselect org-mobile-inbox-for-pull)
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t)
      (goto-char (match-beginning 0))
      (forward-char 1)
      (insert "* TODO ")
      (forward-line)
      (insert ":PROPERTIES:\n:Created: ")
      (forward-line)
      (insert ":END:\n"))
    (let ((tasks (buffer-string)))
      (erase-buffer)
      (save-buffer)
      (kill-buffer (current-buffer))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-max))
          (forward-line)
          (insert tasks))))))

(add-hook 'org-mobile-post-pull-hook '(lambda () (my/org-convert-incoming-mobile-items my/org-refile)))


;;
;; Org-gcal
;;
(setq my/org-gcal-fetch-timer
      (run-with-timer 1
                      (* 60 3)
                      (lambda ()
                        (when (> (nth 1 (current-idle-time)) 60)
                          (org-gcal-fetch)))))

;; (describe-variable 'timer-list)
;; (cancel-timer my/org-gcal-fetch-timer)


(provide 'setup-org-mode)
