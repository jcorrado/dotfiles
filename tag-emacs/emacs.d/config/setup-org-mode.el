(use-package org
  :ensure t)

;; (global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-c b") 'org-switchb)

(use-package org-bullets
  :ensure t
  :init (setq org-bullets-bullet-list '("●" "○" "◉" "✚" "✜" "◇" "◆" "✿" "✸" "▸"))
  :config (org-bullets-mode))

(use-package org-gcal
  :ensure t)

;;
;; Agenda
;;
(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch)))
;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))

(setq org-agenda-span 1)

;; General Appearance
(setq org-hide-leading-stars t
      org-catch-invisible-edits 'show-and-error
      org-src-window-setup 'current-window
      org-log-into-drawer 'LOGBOOK)

;; TODOs
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)")
                          (sequence "WAITING(w)" "|" "CANCELED(c)"))
      org-todo-keyword-faces '(("TODO" :foreground "orange red" :weight bold)
                               ("NEXT" :foreground "lawn green" :weight bold)
                               ("IN-PROGRESS" :foreground "navy" :background "sky blue" :weight bold)
                               ("WAITING" :foreground "yellow" :weight bold)
                               ("DONE" :foreground "dim gray")
                               ("CANCELED" :foreground "dim gray" :strike-through t)))

(setq org-agenda-files '("~/org/gcal/personal"
                         "~/org/gcal/birchbox"
                         "~/org/auaap.org"
                         "~/org/notes.org"
                         "~/org/todo.org"))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))

(provide 'setup-org-mode)
