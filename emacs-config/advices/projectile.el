(defun projectile-project-root-remote (origfunc &rest args)
  (let* ((dir (car args))
         (dir (or dir default-directory)))
    (if (not (file-remote-p dir))
        (apply origfunc args))))

(advice-add 'projectile-project-root :around #'projectile-project-root-remote)
