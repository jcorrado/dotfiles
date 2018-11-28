(require 'package)
(setq package-archives nil
      package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(setq custom-file "~/.emacs.d/custom-conf.el")

(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/config")
(load "miscellaneous")

(setq user-full-name "Jereme Corrado"
      user-mail-address "jereme@zoion.net"

      make-backup-files nil
      vc-follow-symlinks t
      server-temp-file-regexp ".*"
      desktop-restore-frames t
      desktop-files-not-to-save nil
      help-window-select t
      ispell-program-name "aspell"
      ispell-silently-savep t)

(setq-default indent-tabs-mode nil
              major-mode 'text-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-n") 'forward-paragraph)
(global-set-key (kbd "C-p") 'backward-paragraph)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-a") 'my/smarter-move-beginning-of-line)
(global-set-key (kbd "C-c t") (lambda ()
                                (interactive)
                                (my/toggle-transparency my/frame-opacity)))
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-c h d") 'hydra-adjust-display/body)
(global-set-key (kbd "C-c h g") 'hydra-git-gutter/body)

(global-unset-key (kbd "C-x m"))

(server-start)
(winner-mode)

;;
;; Appearance
;;
(setq inhibit-startup-message t
      visible-bell nil
      blink-cursor-blinks 30
      blink-cursor-delay 0.5
      blink-cursor-interval 0.85
      blink-matching-paren t
      cursor-type 'box
      horizontal-scroll-bar nil
      column-number-mode nil
      my/frame-opacity '(85 . 30))

(setq-default cursor-in-non-selected-windows nil)

(blink-cursor-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'initial-frame-alist (cons 'alpha my/frame-opacity))
(add-to-list 'default-frame-alist (cons 'alpha my/frame-opacity))
(add-hook 'after-make-frame-functions (lambda (_) (my/clear-fringe)))

(global-font-lock-mode t)
(set-frame-font "DejaVu Sans Mono 13" t t)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  (my/clear-fringe)
  (custom-set-faces
   '(ivy-current-match
     ((t (:foreground "#000000" :background "#d3d3d3" :underline nil))))))

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

(use-package beacon
  :ensure t
  :diminish
  :init (setq beacon-size 25
              beacon-blink-when-window-scrolls t
              beacon-color "orange red")
  :config (beacon-mode))

(use-package emojify
  :ensure t
  :init (setq emojify-emoji-styles '(unicode)
              emojify-inhibit-major-modes '())
  :config (add-hook 'after-init-hook #'global-emojify-mode))

(use-package nyan-mode
  :disabled
  :ensure t
  :init (setq nyan-wavy-trail nil)
  :config (nyan-mode))

(use-package rainbow-mode
  :ensure t
  :diminish)

(desktop-save-mode)


;;
;; Interface
;;
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-scope 'frame)
  :config (custom-set-faces
           '(aw-leading-char-face
             ((t (:inherit aw-mode-line-face :foreground "orange red" :height 5.0))))))

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

(use-package company
  :ensure t
  :diminish
  :bind (("M-/" . company-complete))
  :config (global-company-mode))

(use-package ibuffer-vc
  :ensure t
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package browse-kill-ring
  :ensure t
  :config (browse-kill-ring-default-keybindings))

(use-package undo-tree
  :ensure t
  :diminish
  :init (setq undo-tree-visualizer-diff t
              undo-tree-visualizer-timestamps t)
  :config (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode))

(use-package smartscan
  :ensure t
  :config (global-smartscan-mode))

(use-package avy
  :ensure t
  :init (setq avy-background t
              avy-all-windows nil)
  :bind (("M-g w" . avy-goto-char-2)))

(use-package hydra
  :ensure t
  :config (require 'setup-hydra))

;;
;; Editing
;;

(defun my/text-mode-hook ()
  (setq fill-column 72)
  (auto-fill-mode t)
  (flyspell-mode t)
  (abbrev-mode t)
  (auto-revert-mode t))
(add-hook 'text-mode-hook 'my/text-mode-hook)

(defun my/prog-mode-hook ()
  (enable-paredit-mode)
  (flyspell-prog-mode))
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

(defun my/conf-unix-mode-hook ()
  (flyspell-prog-mode)
  (auto-revert-mode t))
(add-hook 'conf-unix-mode-hook 'my/conf-unix-mode-hook)

(require 'setup-abbrev-mode)
(require 'setup-org-mode)

(use-package aggressive-indent
  :ensure t
  :diminish
  :config (global-aggressive-indent-mode))

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "Yas")
  :config (yas-global-mode t))

(use-package yasnippet-snippets :ensure t)
(use-package aws-snippets :ensure t)

(use-package ivy-yasnippet
  :ensure t
  :bind ("C-x C-i" . ivy-yasnippet))

(use-package magit
  :ensure t
  :init (setq magit-diff-refine-hunk t
              magit-completing-read-function 'ivy-completing-read)
  :bind ("C-c m" . magit-status))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config
  (set-face-foreground 'git-gutter-fr:modified "deep sky blue")
  (set-face-foreground 'git-gutter-fr:added    "green")
  (set-face-foreground 'git-gutter-fr:deleted  "red")
  (global-git-gutter-mode t))

(use-package git-timemachine
  :ensure t)

(use-package paredit
  :ensure t
  :diminish
  :config (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish
  :init (setq hl-paren-colors '("red" "orange red" "yellow" "green"
                                "cyan" "blue" "magenta" "dark violet"))
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

;; Web browsing
(setq eww-search-prefix "https://www.google.com/search?q=")
(add-hook 'eww-mode-hook (lambda ()
                           (setq-local shr-use-fonts nil)
                           (define-key eww-link-keymap (kbd "I") 'endless/toggle-image-display)))

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

(use-package dictionary
  :ensure t
  :bind ("M-%" . dictionary-lookup-definition))
