(defun lx/helm-find-project-files ()
  (interactive)
  (let ((directory
         (projectile-project-root)))
    (helm-find-1 directory)))
