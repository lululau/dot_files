(spacemacs|use-package-add-hook  evil-mc
  :post-config
  (global-evil-mc-mode -1))

(with-eval-after-load 'evil-mc

  (defun evil-mc-make-cursors-on-paragraph ()
    (interactive)
    (if (evil-mc-has-cursors-p) (user-error "Cursors already exist.")
      (global-evil-mc-mode 1))
    (evil-backward-paragraph 1)
    (if (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (evil-next-line))
    (while (not (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (evil-mc-make-cursor-move-next-line 1))
    (evil-previous-line)
    (evil-mc-undo-cursor-at-pos (point)))

  (defun evil-mc-make-cursors-on-region (skip-empty-lines)
    (interactive "P")
    (if (evil-mc-has-cursors-p) (user-error "Cursors already exist.")
      (global-evil-mc-mode 1))
    (let* ((beg (region-beginning))
           (end (region-end)))
      (evil-exit-visual-state)
      (goto-char beg)
      (while (< (point) end)
        (if (and skip-empty-lines (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (evil-next-line)
            (evil-mc-make-cursor-move-next-line 1)))
      (evil-previous-line)
      (evil-mc-undo-cursor-at-pos (point))))

  (evil-define-command evil-mc-make-cursors-by-regexp ()
    "Initialize `evil-mc-pattern' and make cursors for all matches."
    :repeat ignore
    :evil-mc t
    (if (evil-mc-has-cursors-p) (user-error "Cursors already exist.")
      (global-evil-mc-mode 1)
      (setq evil-mc-pattern (cons (list (read-regexp "Mark regexp: ") t t) (evil-visual-range)))
      (evil-mc-before-undo-all-cursors)
      (if (evil-visual-state-p)
          (narrow-to-region (region-beginning) (region-end)))
      (evil-exit-visual-state)
      (when (evil-mc-has-pattern-p)
        (goto-char (point-min))
        (evil-ex-find-next (evil-mc-get-pattern) 'forward t)
        (goto-char (1- (point)))
        (let ((point (point)))
          (save-excursion
            (while (eq (evil-ex-find-next (evil-mc-get-pattern) 'forward t) t)
              (goto-char (1- (point)))
              (when (/= point (point))
                (evil-mc-run-cursors-before)
                (evil-mc-make-cursor-at-pos (point)))
              (goto-char (1+ (point)))))))
      (widen)
      (evil-mc-print-cursors-info "Created")))

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
