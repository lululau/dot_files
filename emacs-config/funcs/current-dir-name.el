(defun lx/current-buffer-dir-name ()
  (let* ((buffer (current-buffer))
         (buffer-file-name (buffer-file-name buffer)))
    (if buffer-file-name
        (setq buffer-file-directory (file-name-directory buffer-file-name))
      (if (eq 'dired-mode (with-current-buffer buffer major-mode))
          (setq buffer-file-directory (with-current-buffer buffer (expand-file-name dired-directory)))
        (setq buffer-file-directory (with-current-buffer buffer (projectile-project-root)))
        ))
    buffer-file-directory))

(defun lx/open-current-bufffer-dir-in-finder ()
  (interactive)
  (shell-command (format "open -a Finder %s" (shell-quote-argument (lx/current-buffer-dir-name)))))
