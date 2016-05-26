(defun projectile-project-alternate-buffer ()
  (car
   (--remove
    (or
     (eq it (current-buffer))
     (s-matches? "^[* ]" (buffer-name it)))
    (projectile-project-buffers))))

(defun projectile-project-switch-to-alternate-buffer ()
  (interactive)
  (let ((alternate-buffer (projectile-project-alternate-buffer)))
    (if alternate-buffer (switch-to-buffer alternate-buffer))))
