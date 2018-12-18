(use-package org
  :ensure t
  :diminish org-indent-mode
  :init (setq org-startup-indented t
              org-src-window-setup 'current-window
              org-log-into-drawer 'LOGBOOK
              org-catch-invisible-edits 'show-and-error))

(global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-c b") 'org-switchb)

(use-package org-bullets
  :ensure t
  :init (setq org-bullets-bullet-list '("●" "◉" "○"))
  :config (org-bullets-mode))

(use-package org-gcal :ensure t)

;;
;; Agenda
;;
;;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch)))
;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))

(setq org-agenda-span 1)

;; TODOs
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)")
                          (sequence "WAITING(w)" "|" "CANCELED(c)"))
      org-todo-keyword-faces '(("TODO" :foreground "orange red" :weight bold)
                               ("NEXT" :foreground "lawn green" :weight bold)
                               ("IN-PROGRESS" :foreground "navy" :background "sky blue" :weight bold)
                               ("WAITING" :foreground "yellow" :weight bold)
                               ("DONE" :foreground "dim gray")
                               ("CANCELED" :foreground "dim gray" :strike-through t)))

;; Give some thought to this as my process evolves
;; (setq org-agenda-todo-list-sublevels nil)

(setq org-agenda-custom-commands
      '(("p" . "Personal agenda views")
        ("pt" "Personal Upcoming Tasks" tags-todo "personal+TODO=\"TODO\"|personal+TODO=\"NEXT\"")
        ("pn" "Personal NEXT" tags-todo "personal+TODO=\"NEXT\"")

        ("b" . "Birchbox agenda views")
        ("bt" "Birchbox Upcoming Tasks" tags-todo "birchbox+TODO=\"TODO\"|birchbox+TODO=\"NEXT\"")
        ("bn" "Birchbox NEXT" tags-todo "birchbox+TODO=\"NEXT\"")

        ("x" "Agenda + task view"
         ((agenda)
          (todo "NEXT")))))

(setq org-agenda-files '("~/org/todo"
                         "~/org/gcal/personal"
                         "~/org/gcal/birchbox"
                         "~/org/notes.org"))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))

(provide 'setup-org-mode)
