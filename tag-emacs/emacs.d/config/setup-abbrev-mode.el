;; Have a look at http://ergoemacs.org/emacs/emacs_abbrev_mode.html

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ("zn" "zoion.net")
    ("gi" "goldandapager.io")
    ("tm" "Team Mobot")
    ("tmc" "teammobot.com")
    ("tmn" "teammobot.net")
    ("gtmn" "gcp0.teammobot.net")
    ("ntmn" "nyc0.teammobot.net")
    
    ("gm" "Good Morning")
    ("tj" "Thanks,\nJereme")))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

(provide 'setup-abbrev-mode)
