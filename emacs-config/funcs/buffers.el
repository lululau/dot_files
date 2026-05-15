;; -*- lexical-binding: t; -*-

;;;###autoload
(defun lx/kill-buffers (buffers)
  (interactive)
  (mapc 'kill-buffer buffers))

;;;###autoload
(defun lx/get-all-buffers-except (keep-buffer-names)
  (interactive)
  (seq-filter (lambda (buf) (not (seq-contains-p keep-buffer-names (buffer-name buf)))) (buffer-list)))

;;;###autoload
(defun lx/kill-except-spacemacs-and-scratch-buffers ()
  (interactive)
  (lx/kill-buffers (lx/get-all-buffers-except '("*spacemacs*" "*scratch*" "*messages*" " *edit-server*" "*emacsql-sqlite*"))))

;;;###autoload
(defun lx/kill-all-except-default ()
  (interactive)
  (lx/kill-all-non-default-layouts)
  (lx/kill-except-spacemacs-and-scratch-buffers)
  (lx/kill-except-default-processes))

;;;###autoload
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

;;;###autoload
(defun lx/delete-window-or-bury-buffer ()
  (interactive)
  (if (window-parent (selected-window))
      (delete-window)
    (bury-buffer)))

;;;###autoload
(defun lx/new-untitled-buffer ()
  "Create a new untitled buffer in other window"
  (interactive)
  (switch-to-buffer-other-window (generate-new-buffer "*Untitled*")))

;;;###autoload
(defun lx/new-untitled-buffer-same-window ()
  "Create a new untitled buffer in current window"
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Untitled*")))

;;;###autoload
(defun lx/kill-buffer-and-delete-window ()
  "Kill current buffer and delete window"
  (interactive)
  (kill-current-buffer)
  (delete-window))
