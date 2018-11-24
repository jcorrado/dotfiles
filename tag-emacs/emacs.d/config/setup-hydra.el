(defhydra hydra-adjust-display (:hint nil)
  "
Adjust Text:
[_i_]ncrease, [_d_]ecrease, [_r_]eset font size
[_c_]olor names %(if rainbow-mode 'off 'on)
[_p_]rettify symbols %(if prettify-symbols-mode 'off 'on)
"
  ("i" text-scale-increase)
  ("d" text-scale-decrease)
  ("r" (progn
         (text-scale-adjust 0)
         (message "Resetting text size")) :color blue)
  ("c" rainbow-mode)
  ("p" prettify-symbols-mode)
  ("q" nil :color blue))

(defhydra hydra-git-gutter (:pre (git-gutter-mode t)
                                 :hint nil)
  "
Git Gutter:
  ^ ^ _k_ ^ ^  [_s_] stage hunk
  _h_ ^+^ _l_  [_r_] revert hunk
  ^ ^ _j_ ^ ^  [_p_] popup hunk    [_R_] set start revision
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" (progn
         (git-gutter:stage-hunk)
         (message nil)))
  ("r" git-gutter:revert-hunk)
  ("p" (let ((buff "*git-gutter:diff*"))
         (if (get-buffer-window buff) (kill-buffer buff) (git-gutter:popup-hunk))))
  ("R" git-gutter:set-start-revision)
  ("q" (kill-buffer "*git-gutter:diff*") :color blue))

(provide 'setup-hydra)
