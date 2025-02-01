;; Have a look at http://ergoemacs.org/emacs/emacs_abbrev_mode.html

;; Abbrev key follow by C-q, before a trigger (eg: enter) will
;; suppress expansion.

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ("zn" "zoion.net")
    ("gi" "goldandapager.io")

    ("jc" "jcorrado")
    
    ("gm" "Good Morning")
    ("tj" "Thanks,\nJereme")))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

(provide 'setup-abbrev-mode)
