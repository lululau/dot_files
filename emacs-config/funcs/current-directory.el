(defun lx/current-directory()
  (let* ((buffer (current-buffer))
         (buffer-file-name (buffer-file-name buffer)))
    (if buffer-file-name
        (file-name-directory buffer-file-name)
      (if (eq 'dired-mode major-mode)
          dired-directory
        (if (projectile-project-p)
            (projectile-project-root))))))
