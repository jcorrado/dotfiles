(use-package org
  :ensure t
  :diminish org-indent-mode
  :init (setq org-startup-indented t
              org-src-window-setup 'current-window
              org-catch-invisible-edits 'show-and-error))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c b") 'org-switchb)

(use-package org-bullets
  :ensure t
  :init (setq org-bullets-bullet-list '("●" "◉" "○"))
  :config (org-bullets-mode))

(use-package org-gcal :ensure t)


;;
;; Capture
;;
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo/personal.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("m" "Mail task" entry (file+headline "~/org/todo/personal.org" "Tasks")
         "* TODO %(concat \"[[message-id:\" msg-id \"][\" text \"]]\") %?\n  %i\n  %a")))

(defun my/mail-capture (msg-id text)
  (use-local-map (copy-keymap org-capture-mode-map))
  (local-set-key (kbd "C-c C-c") (lambda () (org-capture-finalize)
                                   (delete-frame)))
  (org-capture nil "m")
  (delete-other-windows))

;;
;; TODOs
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
      org-closed-keep-when-no-todo t
      org-treat-insert-todo-heading-as-state-change nil)

;; Give some thought to this as my process evolves
;; (setq org-agenda-todo-list-sublevels nil)


;;
;; Agenda
;;
;;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch)))
;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))

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

(setq org-agenda-span 1
      org-agenda-files '("~/org/todo/"
                         "~/org/gcal/personal/"
                         "~/org/gcal/birchbox/"
                         "~/org/notes.org"
                         "~/org/from-email/"))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))

;;
;; My mail Message-ID link handler
;;
(org-add-link-type "message-id" 'my/org-message-id-open)

(defun my/org-message-id-open (msg-id)
  "Visit mail message with external MUA"
  (let ((cmd "mutt_for_msgid.sh"))
    (shell-command (concat cmd " " msg-id))))

(provide 'setup-org-mode)
