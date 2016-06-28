 (spacemacs|use-package-add-hook persp-mode
   :pre-init
   (progn
     (message "hello post-init-persp-mode")
     (spacemacs|transient-state-format-hint layouts
       spacemacs--layouts-ts-full-hint
       "\n\n
  Go to^^^^^^                                  Actions^^
  ─────^^^^^^──────────────────────────────    ───────^^──────────────────────────────────────────────────
  [_0_,_9_]^^     nth/new layout               [_a_]^^   add buffer
  [_C-0_,_C-9_]^^ nth/new layout               [_A_]^^   add all from layout
  [_<tab>_]^^^^   last layout                  [_d_]^^   close current layout
  [_b_]^^^^       buffer in layout             [_D_]^^   close other layout
  [_h_]^^^^       default layout               [_r_]^^   remove current buffer
  [_l_]^^^^       layout w/helm/ivy            [_R_]^^   rename current layout
  [_L_]^^^^       layouts in file              [_s_/_S_] save all layouts/save by names
  [_n_/_C-l_]^^   next layout                  [_t_]^^   show a buffer without adding it to current layout
  [_N_/_p_/_C-h_] prev layout                  [_x_]^^   kill current w/buffers
  [_o_]^^^^       custom layout                [_X_]^^   kill other w/buffers
  [_<_]^^^^       move layout backword         [_>_]^^   move layout forward
  [_w_]^^^^       workspaces transient state   [_?_]^^   toggle help\n")

     (spacemacs|define-transient-state layouts
       :title "Layouts Transient State"
       :hint-is-doc t
       :dynamic-hint (spacemacs//layouts-ts-hint)
       :bindings
       ;; need to exit in case number doesn't exist
       ("?" spacemacs//layouts-ts-toggle-hint)
       ("1" spacemacs/persp-switch-to-1)
       ("2" spacemacs/persp-switch-to-2)
       ("3" spacemacs/persp-switch-to-3)
       ("4" spacemacs/persp-switch-to-4)
       ("5" spacemacs/persp-switch-to-5)
       ("6" spacemacs/persp-switch-to-6)
       ("7" spacemacs/persp-switch-to-7)
       ("8" spacemacs/persp-switch-to-8)
       ("9" spacemacs/persp-switch-to-9)
       ("0" spacemacs/persp-switch-to-0)
       ("C-1" spacemacs/persp-switch-to-1 :exit t)
       ("C-2" spacemacs/persp-switch-to-2 :exit t)
       ("C-3" spacemacs/persp-switch-to-3 :exit t)
       ("C-4" spacemacs/persp-switch-to-4 :exit t)
       ("C-5" spacemacs/persp-switch-to-5 :exit t)
       ("C-6" spacemacs/persp-switch-to-6 :exit t)
       ("C-7" spacemacs/persp-switch-to-7 :exit t)
       ("C-8" spacemacs/persp-switch-to-8 :exit t)
       ("C-9" spacemacs/persp-switch-to-9 :exit t)
       ("C-0" spacemacs/persp-switch-to-0 :exit t)
       ("<tab>" spacemacs/jump-to-last-layout)
       ("<return>" nil :exit t)
       ("C-h" persp-prev)
       ("C-l" persp-next)
       ("a" persp-add-buffer :exit t)
       ("A" persp-import-buffers :exit t)
       ("b" spacemacs/persp-helm-mini :exit t)
       ("d" spacemacs/layouts-ts-close)
       ("D" spacemacs/layouts-ts-close-other :exit t)
       ("h" spacemacs/layout-goto-default :exit t)
       ("l" spacemacs/helm-perspectives :exit t)
       ("L" persp-load-state-from-file :exit t)
       ("n" persp-next)
       ("N" persp-prev)
       ("o" spacemacs/select-custom-layout :exit t)
       ("p" persp-prev)
       ("r" persp-remove-buffer :exit t)
       ("R" spacemacs/layouts-ts-rename :exit t)
       ("s" persp-save-state-to-file :exit t)
       ("S" persp-save-to-file-by-names :exit t)
       ("t" persp-temporarily-display-buffer :exit t)
       ("w" spacemacs/workspaces-transient-state/body :exit t)
       ("x" spacemacs/layouts-ts-kill)
       ("X" spacemacs/layouts-ts-kill-other :exit t)
       (">" lx/move-layout-forward)
       ("<" lx/move-layout-backward))))
