(defhydra hydra-adjust-display (:hint nil)
  "
Adjust Text: [_i_]ncrease, [_d_]ecrease, [_r_]eset size, [_c_]olorize, [_p_]rettify symbols
"
  ("i" text-scale-increase)
  ("d" text-scale-decrease)
  ("r" (progn
         (text-scale-adjust 0)
         (message "Resetting text size")) :color blue)
  ("c" rainbow-mode)
  ("p" prettify-symbols-mode)
  ("q" nil :color blue))

(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                      :hint nil)
  "
Git Gutter:
  [_n_]: next hunk        [_s_]: _s_tage hunk
  [_p_]: previous hunk    [_r_]: _r_evert hunk
  ^ ^                     [_o_]: p_o_pup hunk
  [_h_]: first hunk
  [_l_]: last hunk        [_R_]: set start _R_evision
"
  ("n" git-gutter:next-hunk)
  ("p" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("o" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue))

(provide 'setup-hydra)
