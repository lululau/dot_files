(spacemacs|use-package-add-hook persp-mode
  :pre-init
  (progn

    (defun* lx/kill-and-load-from-file
        (&optional (fname persp-auto-save-fname) (phash *persp-hash*)
                   names-regexp set-persp-file)
      (interactive (list (read-file-name "Load perspectives from a file: "
                                         persp-save-dir)))
      (when fname
        (let ((p-save-file (concat (or (file-name-directory fname)
                                       (expand-file-name persp-save-dir))
                                   (file-name-nondirectory fname))))
          (if (not (file-exists-p p-save-file))
              (progn (message "[persp-mode] Error: No such file -- %S." p-save-file)
                     nil)
            (lx/kill-all-non-default-layouts)
            (let ((readed-list
                   (with-temp-buffer
                     (buffer-disable-undo)
                     (insert-file-contents p-save-file nil nil nil t)
                     (goto-char (point-min))
                     (read (current-buffer)))))
              (persps-from-savelist
               readed-list phash p-save-file set-persp-file names-regexp))))))

    ;; layouts transient state
    (spacemacs|transient-state-format-hint layouts
      spacemacs--layouts-ts-full-hint
      "\n
 Go to^^^^^^                        Actions^^^^
 ─────^^^^^^──────────────────────  ───────^^^^───────────────────────────────
 [_0_.._9_]^^     nth/new layout    [_a_]^^   add buffer
 [_C-0_.._C-9_]^^ nth/new layout    [_A_]^^   add all buffers from layout
 [_<tab>_]^^^^    last layout       [_d_]^^   close current layout
 [_n_/_C-l_]^^    next layout       [_D_]^^   close other layout
 [_N_/_p_/_C-h_]  prev layout       [_L_]^^   load layouts from file
 [_b_]^^^^        buffer in layout  [_r_]^^   remove current buffer
 [_h_]^^^^        default layout    [_R_]^^   rename current layout
 [_l_]^^^^        another layout    [_s_/_S_] save all layouts/save by names
 [_o_]^^^^        custom layout     [_t_]^^   show buffer w/o adding to layout
 [_w_]^^^^        workspaces TS     [_x_]^^   kill current w/buffers
 [_e_]^^^^        select layout     [_X_]^^   kill other w/buffers
 ^^^^^^                             [_<_/_>_] move layout left/right
                                    [_k_]^^   kill all non-default layouts
 ^^^^^^                             [_?_]^^   toggle help")

    (spacemacs|define-transient-state layouts
      :title "Layouts Transient State"
      :hint-is-doc t
      :dynamic-hint (spacemacs//layouts-ts-hint)
      :bindings
      ;; need to exit in case number doesn't exist
      ("?" spacemacs//layouts-ts-toggle-hint)
      ("1" spacemacs/persp-switch-to-1 :exit t)
      ("2" spacemacs/persp-switch-to-2 :exit t)
      ("3" spacemacs/persp-switch-to-3 :exit t)
      ("4" spacemacs/persp-switch-to-4 :exit t)
      ("5" spacemacs/persp-switch-to-5 :exit t)
      ("6" spacemacs/persp-switch-to-6 :exit t)
      ("7" spacemacs/persp-switch-to-7 :exit t)
      ("8" spacemacs/persp-switch-to-8 :exit t)
      ("9" spacemacs/persp-switch-to-9 :exit t)
      ("0" spacemacs/persp-switch-to-0 :exit t)
      ("e" spacemacs/layout-switch-to :exit t)
      ("C-1" spacemacs/persp-switch-to-1)
      ("C-2" spacemacs/persp-switch-to-2)
      ("C-3" spacemacs/persp-switch-to-3)
      ("C-4" spacemacs/persp-switch-to-4)
      ("C-5" spacemacs/persp-switch-to-5)
      ("C-6" spacemacs/persp-switch-to-6)
      ("C-7" spacemacs/persp-switch-to-7)
      ("C-8" spacemacs/persp-switch-to-8)
      ("C-9" spacemacs/persp-switch-to-9)
      ("C-0" spacemacs/persp-switch-to-0)
      ("<tab>" spacemacs/jump-to-last-layout :exit t)
      ("<return>" nil :exit t)
      ("TAB" spacemacs/jump-to-last-layout)
      ("RET" nil :exit t)
      ("C-h" persp-prev)
      ("C-l" persp-next)
      ("<" spacemacs/move-current-persp-left)
      (">" spacemacs/move-current-persp-right)
      ("a" persp-add-buffer :exit t)
      ("A" persp-import-buffers :exit t)
      ("b" spacemacs/persp-buffers :exit t)
      ("d" spacemacs/layouts-ts-close)
      ("D" spacemacs/layouts-ts-close-other :exit t)
      ("h" spacemacs/layout-goto-default :exit t)
      ("L" lx/kill-and-load-from-file :exit t)
      ("l" spacemacs/persp-perspectives :exit t)
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
      ("k" lx/kill-all-non-default-layouts))

    ))
