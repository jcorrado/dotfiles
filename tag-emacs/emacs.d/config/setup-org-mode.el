;;
;; org-mode
;;
(use-package org
  :ensure t
  :init (setq org-hide-leading-stars t
              org-catch-invisible-edits 'show-and-error
              org-src-window-setup 'current-window
              org-log-into-drawer 'LOGBOOK
              org-todo-keywords
              '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)"))
              org-todo-keyword-faces
              '(("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "cyan" :weight bold)
                ("WAITING" :foreground "yellow" :weight bold)
                ("HOLD" :foreground "yellow" :weight bold)
                ("DONE" :foreground "green" :weight bold)
                ("CANCELLED" :foreground "green" :weight bold :strike-through t)))
  :config
  (add-hook 'org-mode-hook 'flyspell-mode t)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-bullets
  :ensure t
  :init (setq org-bullets-bullet-list '("◉" "●" "○" "✚" "✜" "◇" "◆" "✿" "✸" "▸"))
  :config (org-bullets-mode))

(provide 'setup-org-mode)
