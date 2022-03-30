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
  (lx/kill-except-spacemacs-and-scratch-buffers)
  (lx/kill-except-default-processes)
  (lx/kill-all-non-default-layouts))
