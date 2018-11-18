;; Have a look at http://ergoemacs.org/emacs/emacs_abbrev_mode.html

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ("zn"  "zoion.net")
    ("gi" "goldandapager.io")
    ("bc" "birchbox.com")
    ("eo" "empatico.org")

    ("tj" "Thanks,\nJereme")))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

(provide 'setup-abbrev-mode)