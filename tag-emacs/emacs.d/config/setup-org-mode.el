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
                            org-mobile)
              org-startup-indented t
              org-src-window-setup 'current-window
              org-catch-invisible-edits 'show-and-error
              org-tags-column -100)
  :config (progn
            (global-set-key (kbd "C-c a") 'org-agenda)
            (global-set-key (kbd "C-c c") 'org-capture)
            (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))))

(use-package org-bullets
  :ensure t
  :init (setq org-bullets-bullet-list '("●" "◉" "○"))
  :config (progn
            (org-bullets-mode)
            (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(use-package org-gcal :ensure t)

(setq my/org-personal-todo (concat org-directory "/todo/Personal.org")
      my/org-birchbox-todo (concat org-directory "/todo/Birchbox.org")
      my/org-empatico-todo (concat org-directory "/todo/Empatico.org")

      my/org-todo-files (list my/org-personal-todo
                              my/org-birchbox-todo
                              my/org-empatico-todo)

      my/org-personal-routine (concat org-directory "/routine/Personal.org")

      my/org-routine-files (list my/org-personal-routine)

      my/org-inbox (concat org-directory "/inbox.org")
      my/org-notes (concat org-directory "/notes.org")
      my/org-errands (concat org-directory "/errands.org"))


;;
;; Tags
;;
(setq org-tag-alist '((:startgroup . nil)
                      ("@email" . ?m) ("@errands" . ?e) ("@home" . ?h) ("@nyc2" . ?n)
                      (:endgroup . nil)
                      ("project" . ?p)
                      ("routine" . ?r)
                      ("shared" . ?s)))


;;
;; Capture
;;
(setq org-capture-templates
      '(("p" "Personal Task" entry (file+headline my/org-inbox "Personal")
         "* TODO %?\n")
        ("e" "Errands" entry (file my/org-errands)
         "* TODO %?\n")
        ("w" "Work Tasks")
        ("wb" "Birchbox Task" entry (file+headline my/org-inbox "Birchbox")
         "* TODO %?\n")
        ("we" "Empatico Task" entry (file+headline my/org-inbox "Empatico")
         "* TODO %?\n")
        ("m" "Personal Mail Followup Task" entry (file+headline my/org-personal-todo  "Tasks")
         "* NEXT [[%:link][%:description]] :@email:%i\n%?\n")
        ("n" "Birchbox Mail Followup Task" entry (file+headline my/org-birchbox-todo  "Tasks")
         "* NEXT [[%:link][%:description]] :@email:%i\n%?\n")))

(setq org-refile-targets '((my/org-todo-files . (:maxlevel . 3)))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)


;;
;; Tasks
;;
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i!/!)" "|" "DONE(d!/!)")
                          (sequence "WAITING(w@/!)" "|" "CANCELED(c@/!)")
                          (sequence "HABIT(h)" "|" "DONE(d!)" "CANCELED(c@/!)"))
      org-todo-keyword-faces '(("TODO" :foreground "orange red" :weight bold)
                               ("NEXT" :foreground "lawn green" :weight bold)
                               ("IN-PROGRESS" :foreground "navy" :background "sky blue" :weight bold)
                               ("WAITING" :foreground "yellow" :weight bold)
                               ("DONE" :foreground "dim gray")
                               ("CANCELED" :foreground "dim gray" :strike-through t)
                               ("HABIT" :foreground "light blue" :weight bold))
      org-log-into-drawer 'LOGBOOK
      org-log-done-with-time t
      org-closed-keep-when-no-todo t
      org-treat-insert-todo-heading-as-state-change nil)

;; Habits
(setq org-habit-graph-column 58
      org-habit-preceding-days 21
      org-habit-following-days 7
      org-habit-show-habits-only-for-today t)


;;
;; Agenda
;;
(setq org-agenda-tags-todo-honor-ignore-options t
      org-agenda-todo-list-sublevels nil
      org-agenda-span 1
      org-agenda-files (append my/org-todo-files
                               my/org-routine-files
                               (list my/org-notes
                                     org-gcal-dir
                                     my/org-errands)))

(setq org-agenda-custom-commands
      '(("p" . "Personal Agenda Views")
        ("pt" "Personal Upcoming Tasks" tags-todo "per/TODO|NEXT")
        ("pn" "Personal Unscheduled NEXT Tasks" tags-todo "per/NEXT"
         ((org-agenda-todo-ignore-scheduled 'all)))

        ("h" "Habits Report" agenda ""
         ((org-habit-show-all-today t)
          (org-habit-show-habits-only-for-today t)
          (org-agenda-tag-filter-preset '("+routine"))
          (org-agenda-use-time-grid nil)))

        ("b" . "Birchbox Agenda Views")
        ("bt" "Birchbox Upcoming Tasks" tags-todo "bbx/TODO|NEXT")
        ("bn" "Birchbox Unscheduled NEXT Tasks" tags-todo "bbx/next"
         ((org-agenda-todo-ignore-scheduled 'all)))

        ("e" . "Empatico Agenda Views")
        ("et" "Empatico Upcoming Tasks" tags-todo "emp/TODO|NEXT")
        ("en" "Empatico Unscheduled NEXT Tasks" tags-todo "emp/next"
         ((org-agenda-todo-ignore-scheduled 'all)))

        ("x" "Agenda + task view"
         ((agenda)
          (todo "NEXT|IN-PROGRESS"
                ((org-agenda-overriding-header "Unscheduled NEXT, IN-PROGRESS")
                 (org-agenda-todo-ignore-scheduled 'all))))
         ((org-agenda-tag-filter-preset '("-shared"))))
        ))


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
      org-mobile-files (cons my/org-errands
                             my/org-routine-files))

(provide 'setup-org-mode)
