(require 'package)
(setq package-archives nil
      package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/elisp")
(load "miscellaneous")

(setq custom-file "~/.emacs.d/custom-conf.el")

(setq user-full-name "Jereme Corrado"
      user-mail-address "jereme@zoion.net"
      inhibit-startup-message t)

(setq-default indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

(desktop-save-mode)
;; Unsetting this and forcing a save is a good way to clear out any
;; wonky theme state that's been unintentionally persisted to the
;; desktop file.
(setq desktop-restore-frames t)

(global-set-key (kbd "C-n") 'forward-paragraph)
(global-set-key (kbd "C-p") 'backward-paragraph)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [remap move-beginning-of-line] 'my/smarter-move-beginning-of-line)

;; Alternative to default: other-window
(use-package switch-window
  :ensure t
  :init (setq switch-window-shortcut-style 'qwerty
              switch-window-minibuffer-shortcut ?z)
  :bind ("C-x o" . switch-window))

;; Improved completion
(use-package ivy
  :ensure t
  :diminish
  :init (setq ivy-display-style 'fancy
              ivy-use-virtual-buffers t
              ivy-count-format "(%d/%d) ")
  :config (ivy-mode))

(use-package swiper
  :ensure t
  :init (setq enable-recursive-minibuffers nil)
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package company
  :ensure t
  :diminish
  :config (global-company-mode))

(use-package browse-kill-ring
  :ensure t
  :config (browse-kill-ring-default-keybindings))

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode))

(use-package aggressive-indent
  :ensure t
  :diminish
  :config (global-aggressive-indent-mode))

(use-package smartscan
  :ensure t
  :config (global-smartscan-mode))

(use-package dictionary
  :ensure t
  :config (global-set-key (kbd "M-%") 'dictionary-lookup-definition))


;;
;; Appearance
;;
(setq visible-bell t
      blink-cursor-mode t
      blink-cursor-blinks 180
      blink-cursor-delay 0.5
      blink-cursor-interval 0.85
      blink-matching-paren t
      horizontal-scroll-bar nil
      column-number-mode nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-font-lock-mode t)
(set-frame-font "DejaVu Sans Mono 12" t t)
(set-face-attribute 'fringe nil :background nil)

(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Included themes
;;(load-theme 'deeper-blue t)
;;(load-theme 'misterioso t)
;;(load-theme 'tango-dark t)
;;(load-theme 'tsdh-dark t)
;;(load-theme 'wheatgrass t)
;;(load-theme 'wombat t)

;; Packaged themes - retrieve as needed (but don't use-package)
;;(load-theme 'arjen-grey t)
(load-theme 'zenburn t)
;;(load-theme 'monokai t)

(use-package smart-mode-line
  :ensure t
  :config
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/" ":PRJ:") t)
  (sml/setup))

(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode)
  (diminish 'auto-fill-function)
  (diminish 'flyspell-mode)
  (diminish 'abbrev-mode "Abv")
  (diminish 'mml-mode))

;; This isn't working consistently for me
(use-package dimmer
  :disabled
  :ensure t
  :init (setq dimmer-fraction 0.55
              dimmer-exclusion-regexp "^\*")
  :config (dimmer-mode))

(use-package focus
  :disabled
  :ensure t)

(use-package nyan-mode
  :disabled
  :ensure t
  :init (setq nyan-wavy-trail nil)
  :config (nyan-mode))

(use-package emojify
  :ensure t
  :init (setq emojify-emoji-styles '(unicode)
              emojify-inhibit-major-modes '())
  :config (add-hook 'after-init-hook #'global-emojify-mode))


;;
;; Editing
;;
(setq make-backup-files nil
      vc-follow-symlinks t)

(server-start)
(setq  server-temp-file-regexp ".*")

(defun my-text-mode-hook ()
  (setq fill-column 72)
  (auto-fill-mode t)
  (flyspell-mode t)
  (abbrev-mode t)
  (auto-revert-mode t))

(add-hook 'text-mode-hook 'my-text-mode-hook)

(require 'setup-abbrev-mode)
(require 'setup-org-mode)

(use-package undo-tree
  :ensure t
  :diminish
  :init (setq undo-tree-visualizer-diff t
              undo-tree-visualizer-timestamps t)
  :config (global-undo-tree-mode))

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "Yas")
  :config (yas-global-mode t))

(use-package yasnippet-snippets
  :ensure t)

(use-package ivy-yasnippet
  :ensure t
  :bind ("C-x C-i" . ivy-yasnippet))

(use-package magit
  :ensure t
  :init (setq magit-diff-refine-hunk t)
  :config (global-set-key (kbd "C-c m") 'magit-status))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode t))

(use-package git-timemachine
  :ensure t)

(use-package paredit
  :ensure t
  :diminish
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package paredit-everywhere
  :ensure t
  :diminish
  :config (paredit-everywhere-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish
  :init (setq hl-paren-colors '("red" "orange" "yellow" "green"
                                "cyan" "blue" "dark violet"))
  :config (global-highlight-parentheses-mode))

;; Clojure
(use-package cider
  :ensure t
  :init (setq cider-repl-display-help-banner nil)
  :config (add-hook 'clojure-mode-hook 'prettify-symbols-mode))

(use-package clojure-snippets
  :ensure t)

;; JavaScript
(setq js-indent-level 2)

;; WWW
(setq browse-url-generic-program "sensible-browser"
      browse-url-browser-function 'browse-url-generic)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;;
;; Mail composition, in mutt
;;
(add-to-list 'auto-mode-alist '("/mutt" . (lambda ()
					    (forward-paragraph)
                                            (message-mode))))

(defun my-message-mode-hook ()
  (setq fill-column 72)
  (auto-fill-mode t)
  (flyspell-mode t)
  (abbrev-mode t)
  (append)
  (local-set-key "\C-c\C-c" 'server-edit))

(add-hook 'message-mode-hook 'my-message-mode-hook)
