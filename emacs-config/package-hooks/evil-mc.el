(spacemacs|use-package-add-hook  evil-mc
  :post-config
  (global-evil-mc-mode -1))

(with-eval-after-load 'evil-mc

  (evil-define-command evil-mc-undo-cursor-at-posi ()
    "Initialize `evil-mc-pattern' and make cursors for all matches."
    :repeat ignore
    :evil-mc t
    (evil-mc-undo-cursor-at-pos nil))

  (let ((keys '(("C-m" . evil-mc-make-all-cursors)
                ("C-M-u" . evil-mc-undo-all-cursors)
                ("C-u" . evil-mc-undo-cursor-at-posi)
                ("C-s" . evil-mc-pause-cursors)
                ("C-r" . evil-mc-resume-cursors)
                ("C-S-r" . undo-tree-redo)
                ("C-h" . evil-mc-make-cursor-here)
                ("C-j" . evil-mc-make-cursor-move-next-line)
                ("C-k" . evil-mc-make-cursor-move-prev-line)
                ("M-j" . evil-mc-skip-and-goto-next-cursor)
                ("M-k" . evil-mc-skip-and-goto-prev-cursor)
                ("M-n" . evil-mc-make-and-goto-next-cursor)
                ("M-p" . evil-mc-make-and-goto-prev-cursor)
                ("C-n" . evil-mc-make-and-goto-next-match)
                ("C-t" . evil-mc-skip-and-goto-next-match)
                ("C-p" . evil-mc-make-and-goto-prev-match))))
    (dolist (key-data keys)
      (evil-define-key 'normal evil-mc-key-map (kbd (car key-data)) (cdr key-data))
      (evil-define-key 'visual evil-mc-key-map (kbd (car key-data)) (cdr key-data))))

  (setq evil-mc-custom-known-commands '((evil-org-delete-char . ((:default . evil-mc-execute-default-evil-delete)))
                                        (evil-org-delete . ((:default . evil-mc-execute-default-evil-delete))))))
