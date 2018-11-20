(setq user-full-name "Jereme Corrado"
      user-mail-address "jereme@zoion.net")

(require 'package)
(setq package-archives nil
      package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/config")
(load "miscellaneous")

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; For the Customization bits that Emacs wants to manage itself
(setq custom-file "~/.emacs.d/custom-conf.el")

(desktop-save-mode)
;; Unsetting `desktop-restore-frames` and forcing a save is a good way
;; to clear out any wonky theme state that's been unintentionally
;; persisted to the desktop file.
(setq desktop-restore-frames t
      desktop-files-not-to-save nil)

(global-set-key (kbd "C-n") 'forward-paragraph)
(global-set-key (kbd "C-p") 'backward-paragraph)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c t") 'my/toggle-transparency)
(global-set-key [remap move-beginning-of-line] 'my/smarter-move-beginning-of-line)

(global-unset-key (kbd "C-x m"))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package switch-window
  :ensure t
  :init (setq switch-window-shortcut-style 'qwerty
              switch-window-minibuffer-shortcut ?z)
  :bind ("C-x o" . switch-window))

(use-package ivy
  :ensure t
  :diminish
  :init (setq ivy-display-style 'fancy
              ivy-use-virtual-buffers t
              ivy-count-format "(%d/%d) "
              ivy-use-selectable-prompt t)
  :config (ivy-mode t))

(use-package swiper
  :ensure t
  :init (setq enable-recursive-minibuffers nil)
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package ibuffer-vc
  :ensure t
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

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

(use-package smartscan
  :ensure t
  :config (global-smartscan-mode))

(use-package dictionary
  :ensure t
  ;;:config (global-set-key (kbd "M-%") 'dictionary-lookup-definition)
  :bind ("M-%" . dictionary-lookup-definition))


;;
;; Appearance
;;
(setq inhibit-startup-message t
      visible-bell t
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
(add-hook 'after-make-frame-functions (lambda (_) (my/clear-fringe)))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  (my/clear-fringe))

(global-font-lock-mode t)
(set-frame-font "DejaVu Sans Mono 13" t t)

(use-package smart-mode-line
  :ensure t
  :config
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/auaap/empatico/" ":EMP:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/birchbox/" ":BBX:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/yetibot/yetibot.core" ":YETI.CORE:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/yetibot/yetibot" ":YETI:") t)
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
      vc-follow-symlinks t
      server-temp-file-regexp ".*"
      default-major-mode 'text-mode)

(setq-default indent-tabs-mode nil)

(add-hook 'text-mode-hook (lambda ()
                            (setq fill-column 72)
                            (auto-fill-mode t)
                            (flyspell-mode t)
                            (abbrev-mode t)
                            (auto-revert-mode t)))

(add-hook 'prog-mode-hook (lambda ()
                            (enable-paredit-mode)
                            (flyspell-prog-mode)))

(require 'setup-abbrev-mode)
(require 'setup-org-mode)

(server-start)

(use-package undo-tree
  :ensure t
  :diminish
  :init (setq undo-tree-visualizer-diff t
              undo-tree-visualizer-timestamps t)
  :config (global-undo-tree-mode))

(use-package aggressive-indent
  :ensure t
  :diminish
  :config (global-aggressive-indent-mode))

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
  :bind ("C-c m" . magit-status))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode t))

(use-package git-timemachine
  :ensure t)

(use-package paredit
  :ensure t
  :diminish
  :config (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish
  :init (setq hl-paren-colors '("red" "orange" "yellow" "green"
                                "cyan" "blue" "dark violet"))
  :config (global-highlight-parentheses-mode))

(use-package dumb-jump
  :ensure
  :init (setq dumb-jump-selector 'ivy
              dumb-jump-force-searcher 'ag)
  :bind (("M-g j" . dumb-jump-go-prefer-external)
         ("M-g p" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look)))

;; Clojure
(use-package cider
  :ensure t
  :init (setq cider-repl-display-help-banner nil)
  :config (add-hook 'clojure-mode-hook 'prettify-symbols-mode))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(use-package clojure-snippets
  :ensure t)

;; JavaScript
(setq js-indent-level 2)

;; External web browsing
(setq browse-url-generic-program "sensible-browser"
      browse-url-browser-function 'browse-url-generic)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; Mail, via mutt
(add-hook 'mail-mode-hook (lambda () (local-set-key "\C-c\C-c" 'server-edit)))
(add-to-list 'auto-mode-alist '("/mutt" . (lambda ()
                                            (mail-mode)
                                            (forward-paragraph))))
