(defun lx/kill-buffers (buffers)
  (interactive)
  (mapc 'kill-buffer buffers))

(defun lx/get-all-buffers-except (keep-buffer-names)
  (interactive)
  (seq-filter (lambda (buf) (not (seq-contains-p keep-buffer-names (buffer-name buf)))) (buffer-list)))

(defun lx/kill-except-spacemacs-and-scratch-buffers ()
  (interactive)
  (lx/kill-buffers (lx/get-all-buffers-except '("*spacemacs*" "*scratch*" "*messages*" " *edit-server*" "*emacsql-sqlite*"))))

(defun lx/kill-all-except-default ()
  (interactive)
  (lx/kill-all-non-default-layouts)
  (lx/kill-except-spacemacs-and-scratch-buffers)
  (lx/kill-except-default-processes))

(defun lx/switch-to-warning-buffer (arg)
  (interactive "P")
  (with-current-buffer (get-buffer "*Warnings*")
    (goto-char (point-max))
    (if arg
        (switch-to-buffer-other-window (current-buffer))
      (switch-to-buffer (current-buffer)))
    (when (evil-evilified-state-p)
      (evil-normal-state)))
  )

(defun lx/delete-window-or-bury-buffer ()
  (interactive)
  (if (window-parent (selected-window))
      (delete-window)
    (bury-buffer)))
