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

(defun my/clear-fringe () (set-face-attribute 'fringe nil :background nil))

;; https://www.emacswiki.org/emacs/TransparentEmacs
(defun my/toggle-transparency (opacity)
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter (selected-frame)
                         'alpha
                         (if (eql alpha 100) opacity 100))))

;; https://emacs.stackexchange.com/questions/561/how-can-i-toggle-displaying-images-in-eww-without-a-page-refresh
(defvar-local endless/display-images t)

(defun endless/toggle-image-display ()
  "Toggle images display on current buffer."
  (interactive)
  (setq endless/display-images
        (null endless/display-images))
  (endless/backup-display-property endless/display-images))

(defun endless/backup-display-property (invert &optional object)
  "Move the 'display property at POS to 'display-backup.
Only applies if display property is an image.
If INVERT is non-nil, move from 'display-backup to 'display
instead.
Optional OBJECT specifies the string or buffer. Nil means current
buffer."
  (let* ((inhibit-read-only t)
         (from (if invert 'display-backup 'display))
         (to (if invert 'display 'display-backup))
         (pos (point-min))
         left prop)
    (while (and pos (/= pos (point-max)))
      (if (get-text-property pos from object)
          (setq left pos)
        (setq left (next-single-property-change pos from object)))
      (if (or (null left) (= left (point-max)))
          (setq pos nil)
        (setq prop (get-text-property left from object))
        (setq pos (or (next-single-property-change left from object)
                      (point-max)))
        (when (eq (car prop) 'image)
          (add-text-properties left pos (list from nil to prop) object))))))

;; Change and freeze time
;; https://www.reddit.com/r/emacs/comments/75nkj6/orgmode_tasks_closed_yesterday/
(defun my/freeze-time ()
  "Freeze `current-time' at the current active or inactive timestamp. If point
is not on a timestamp, the function prompts for one. If time is not specified,
either by the timstamp under point or prompt, the time defaults to the
current HH:MM of today at the selected date."
  (interactive)
  (let ((time
         (cond ((if (org-at-timestamp-p 'lax) t)
                (match-string 0))
               (t
                (org-read-date t nil nil "Input freeze time:")))))
    (eval (macroexpand
           `(defadvice current-time (around freeze activate)
              (setq ad-return-value ',
                    (append (org-read-date nil t time) '(0 0))))))
    (set-face-background 'fringe "firebrick2")))

;; Release changed / frozen time
(defun my/release-time ()
  "Release the time frozen by `freeze-time'."
  (interactive)
  (ad-remove-advice 'current-time 'around 'freeze)
  (ad-activate 'current-time)
  (set-face-background 'fringe nil))
