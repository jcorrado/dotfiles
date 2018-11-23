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
  [_n_]: next hunk        [_s_]: _s_tage hunk
  [_p_]: previous hunk    [_r_]: _r_evert hunk
   ^ ^                    [_o_]: toggle p_o_pup hunk
  [_h_]: first hunk
  [_l_]: last hunk        [_R_]: set start _R_evision
"
  ("n" git-gutter:next-hunk)
  ("p" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" (progn
         (git-gutter:stage-hunk)
         (message nil)))
  ("r" git-gutter:revert-hunk)
  ("o" (let ((buff "*git-gutter:diff*"))
         (if (get-buffer-window buff) (kill-buffer buff) (git-gutter:popup-hunk))))
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue))

(provide 'setup-hydra)
