(setq user-full-name "Jereme Corrado"
      user-mail-address "jereme@zoion.net")

(require 'package)
(setq package-archives nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/config")

(setq inhibit-startup-message t)
(setq vc-follow-symlinks t)
(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)
;;(setq default-major-mode 'text-mode)

(server-start)
(setq  server-temp-file-regexp ".*")

(global-set-key [(control n)] `forward-paragraph)
(global-set-key [(control p)] `backward-paragraph)

(global-set-key (kbd "<f5>") 'revert-buffer)

;; Lots more to learn here
;; http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
;; (global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Alternative to default: other-window
(use-package switch-window
  :ensure t
  :bind
  ("C-x o" . switch-window))

(use-package smartscan
  :ensure t
  :config
  (smartscan-mode 1))

;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

; improved completion
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode))

(use-package swiper
  :ensure t
  :diminish ivy-mode
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper)
  :config
  (setq enable-recursive-minibuffers nil))

(use-package browse-kill-ring
  :ensure t
  :init
  (browse-kill-ring-default-keybindings))

(use-package which-key
  :ensure t)


;;
;; string insert shortcuts
;;
(global-unset-key (kbd "C-x C-e"))

(defun insert-domain ()
  "Birchbox's primary domain name"
  (interactive)
  (insert "birchbox.com"))
(global-set-key (kbd "C-x C-e d") 'insert-domain)

(defun insert-thanks-jereme ()
  "The close I always type"
  (interactive)
  (insert "Thanks,\nJereme"))
(global-set-key (kbd "C-x C-e t") 'insert-thanks-jereme)

(use-package dictionary
  :ensure t
  :init
  (global-set-key "\M-%" 'dictionary-lookup-definition))


;;
;; Appearance
;;
(setq visible-bell t)
(setq blink-cursor-mode t
      blink-cursor-blinks 180
      blink-cursor-delay 0.5
      blink-cursor-interval 0.85)
(setq blink-matching-paren t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq column-number-mode t)
;(global-linum-mode)
(global-font-lock-mode t)
(set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1" t t)


;;
;; Pleasing, darker themes
;;

;; (use-package arjen-grey-theme
;;   :ensure t
;;   :config
;;   (load-theme 'arjen-grey t))

;;(load-theme 'deeper-blue t)
;;(load-theme 'misterioso t)
;;(load-theme 'tango-dark t)
;;(load-theme 'tsdh-dark t)
;;(load-theme 'wheatgrass t)
(load-theme 'wombat t)

(use-package diminish)

(use-package nyan-mode
  :ensure t
  :config
  (setq nyan-wavy-trail nil)
  (nyan-mode))

;; (use-package mode-icons
;;   :ensure t
;;   :config
;;   (mode-icons-mode t)
;; )


;;
;; transparency
;;
;; https://www.emacswiki.org/emacs/TransparentEmacs
;;
(set-frame-parameter (selected-frame) 'alpha '(85 . 30))
;;(add-to-list 'default-frame-alist '(alpha . (85 . 30)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ;; Also handle undocumented (<active> <inactive>) form.
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(85 . 30) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)


;;
;; Editing
;;
(setq-default indent-tabs-mode nil)

(defun my-text-mode-hook ()
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (abbrev-mode 1)
  (setq fill-column 72))

(add-hook 'text-mode-hook 'my-text-mode-hook)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  :config
  (global-undo-tree-mode))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c m") 'magit-status)
  (setq magit-diff-refine-hunk t))

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :ensure t)

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

;; The line numbers were starting to drive me crazy
;;(add-hook 'prog-mode-hook (lambda () (linum-mode)))

(use-package paredit-everywhere
  :ensure t
  :diminish paredit-everywhere-mode
  :config
  (paredit-everywhere-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (global-highlight-parentheses-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode))


;;
;; Clojure
;;
;; This works if your project.clj specifies Figwheel
;; See `lein new figwheel foo'
;; (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
(use-package cider
  :ensure t
  :config
  (setq cider-repl-display-help-banner nil)
  (add-hook 'clojure-mode-hook 'prettify-symbols-mode))

;; JavaScript
(setq js-indent-level 2)


;; org-mode
(require 'setup-org-mode)


;; elfeed - RSS Reader
(require 'setup-elfeed)


;;
;; WWW
;;
(setq browse-url-generic-program "sensible-browser")
(setq browse-url-browser-function 'browse-url-generic)


;;
;; mail
;;

; mutt mail composition
(add-to-list 'auto-mode-alist '("/mutt" . (lambda ()
					    (forward-paragraph)
                                            (message-mode))))

(defun my-message-mode-hook ()
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (abbrev-mode 1)
  (setq fill-column 72)
  (append)
  (bbdb-insinuate-message)
  (local-set-key "\C-c\C-c" 'server-edit))

(add-hook 'message-mode-hook 'my-message-mode-hook)


;;
;; ediff
;;
(setq ediff-split-window-function 'split-window-horizontally)
;; Disable separate frame for control window, instead using a window
;; in the main frame.  I never much cared for the little floater
;; window, but now, with the way my window compositor is setup, it
;; makes ediff unusable.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


(desktop-read)
(desktop-save-mode)
