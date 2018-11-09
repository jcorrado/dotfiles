;;
;; org-mode
;;
(use-package org
  :ensure t
  :init
  (add-hook 'org-mode-hook 'flyspell-mode t))

;; (use-package org-journal
;;   :ensure t)

(use-package org-jira
  :ensure t)

;;
;; Source code editing
;;
(setq org-src-window-setup 'current-window)

;;
;; Appearance
;;
(setq org-hide-leading-stars t)
(setq org-catch-invisible-edits 'show-and-error)

;; I'm not sure this is the best way to do this
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :foreground "deep sky blue" :weight bold))))
 '(org-level-2 ((t (:inherit outline-1 :foreground "dark gray"))))
 '(org-level-3 ((t (:inherit outline-1 :foreground "coral"))))
 '(org-level-4 ((t (:inherit outline-1 :foreground "yellow"))))
 '(org-level-5 ((t (:inherit outline-1 :foreground "sandy brown")))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)"))))

(setq org-log-into-drawer 'LOGBOOK)

(setq org-todo-keyword-faces
      (quote (
	      ("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "cyan" :weight bold)
              ("WAITING" :foreground "yellow" :weight bold)
              ("HOLD" :foreground "yellow" :weight bold)
              ("DONE" :foreground "green" :weight bold)
              ("CANCELLED" :foreground "green" :weight bold :strike-through t))))


;;
;; JIRA
;;
(setq jiralib-url "https://birchbox.atlassian.net")
(setq request-message-level 'info)
(setq request-log-level 'info)

(provide 'setup-org-mode)
