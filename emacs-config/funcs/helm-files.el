(defun lx/helm-find-project-files ()
  (interactive)
  (require 'helm-find)
  (let ((directory
         (projectile-project-root)))
    (helm-find-1 directory)))
