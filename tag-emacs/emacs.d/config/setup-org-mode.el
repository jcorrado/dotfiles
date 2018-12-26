(use-package org
  :ensure t
  :diminish org-indent-mode
  :init (setq org-modules '(org-w3m
                            org-bbdb
                            org-bibtex
                            org-docview
                            org-info
                            org-protocol
                            org-habit)
              org-startup-indented t
              org-src-window-setup 'current-window
              org-catch-invisible-edits 'show-and-error)
  :config (progn
            (global-set-key (kbd "C-c a") 'org-agenda)
            (global-set-key (kbd "C-c c") 'hydra-org-capture/body)
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
      my/org-notes (concat org-directory "/notes.org"))


;;
;; Agenda
;;
;;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch)))
;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))

(setq org-agenda-span 1
      org-agenda-files (append my/org-todo-files
                               my/org-routine-files
                               (list my/org-notes
                                     org-gcal-dir)))

(setq org-agenda-custom-commands
      '(("p" . "Personal agenda views")
        ("pt" "Personal Upcoming Tasks" tags-todo "personal+TODO=\"TODO\"|personal+TODO=\"NEXT\"")
        ("pn" "Personal NEXT" tags-todo "personal+TODO=\"NEXT\"")

        ("b" . "Birchbox agenda views")
        ("bt" "Birchbox Upcoming Tasks" tags-todo "birchbox+TODO=\"TODO\"|birchbox+TODO=\"NEXT\"")
        ("bn" "Birchbox NEXT" tags-todo "birchbox+TODO=\"NEXT\"")

        ("x" "Agenda + task view"
         ((agenda)
          (todo "NEXT|IN-PROGRESS")))))


;;
;; Capture
;;
(setq org-capture-templates
      '(("p" "Personal Task" entry (file+headline my/org-inbox "Inbox")
         "* TODO %?  :per:\n  %i\n%x\n  %a")
        ("b" "Birchbox Task" entry (file+headline my/org-inbox "Inbox")
         "* TODO %?  :birchbox:\n  %i\n  %a")
        ("e" "Empatico Task" entry (file+headline my/org-inbox "Inbox")
         "* TODO %?  :empatico:\n  %i\n  %a")
        ("m" "Personal Mail Followup Task" entry (file+headline my/org-personal-todo  "Tasks")
         "* NEXT [[%:link][%:description]] :%i:@email:\n%?\n")
        ("n" "Birchbox Mail Followup Task" entry (file+headline my/org-birchbox-todo  "Tasks")
         "* NEXT [[%:link][%:description]] :%i:@email:\n%?\n")))

(setq org-refile-targets '((my/org-todo-files . (:maxlevel . 3)))
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil)

;; Les intrusive than the standard org-capture pop-up, but maybe it doesn't matter
(defhydra hydra-org-capture (:hint nil)
  "
Org Capture:
^^[_p_]ersonal task  ^^[_b_]irchbox task  ^^[_e_]mpatico task
"
  ("p" (org-capture nil "p"))
  ("b" (org-capture nil "b"))
  ("e" (org-capture nil "e"))
  ("q" nil :color blue)
  )


;; 
;; org-refile-allow-creating-parent-nodes


;;
;; Tasks
;;
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i!/!)" "|" "DONE(d!/!)")
                          (sequence "WAITING(w@/!)" "|" "CANCELED(c@/!)")
                          (sequence "HABIT(h)" "|" "DONE(d!)"))
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
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

;; Give some thought to this as my process evolves
;; (setq org-agenda-todo-list-sublevels nil)

;;
;; My mail Message-ID link handler
;;
(org-add-link-type "message-id" 'my/org-message-id-open)

(defun my/org-message-id-open (msg-id)
  "Visit mail message with external MUA"
  (let ((cmd "mutt_for_msgid.sh"))
    (shell-command (concat cmd " " msg-id))))

(provide 'setup-org-mode)
