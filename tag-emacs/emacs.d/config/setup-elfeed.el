(use-package elfeed
  :disabled
  :ensure t
  :init (setq  shr-inhibit-images nil)
  :config (global-set-key (kbd "C-x w") 'elfeed)
  (eval-after-load 'elfeed-search
    '(define-key elfeed-search-mode-map (kbd "*") 'elfeed-toggle-starred)))

(use-package elfeed-goodies
  :disabled
  :ensure t
  :config (elfeed-goodies/setup))

;; Use an org file to organize feeds
(use-package elfeed-org
  :disabled
  :ensure t
  :init (setq rmh-elfeed-org-files (list "~/.elfeed.org"))
  :config (elfeed-org))

;; Starring articles
;; http://pragmaticemacs.com/emacs/star-and-unstar-articles-in-elfeed/
(defalias 'elfeed-toggle-starred
  (elfeed-expose #'elfeed-search-toggle-all '*))

;; Faces

;; starred
(defface elfeed-search-starred-title-face
  '((t :foreground "white"
       :background "tomato"))
  "Marks a starred Elfeed entry.")

;; security tag
(defface elfeed-search-security-title-face
  '((t :foreground "red"))
  "Marks a security Elfeed entry.")

;; news tag
(defface elfeed-search-news-title-face
  '((t :foreground "cyan"))
  "Marks a news Elfeed entry.")

;; eng tag
(defface elfeed-search-eng-title-face
  '((t :foreground "green"))
  "Marks a eng Elfeed entry.")

;; tech tag
(defface elfeed-search-tech-title-face
  '((t :foreground "orange"))
  "Marks a tech Elfeed entry.")

(push '(security elfeed-search-security-title-face) elfeed-search-face-alist)
(push '(eng elfeed-search-eng-title-face) elfeed-search-face-alist)
(push '(tech elfeed-search-tech-title-face) elfeed-search-face-alist)
(push '(news elfeed-search-news-title-face) elfeed-search-face-alist)
(push '(* elfeed-search-starred-title-face) elfeed-search-face-alist)

(provide 'setup-elfeed)
