(defun projectile-project-root-remote (origfunc &rest args)
  (let* ((dir (car args))
         (dir (or dir default-directory)))
    (if (not (file-remote-p dir))
        (apply origfunc args))))

(advice-add 'projectile-project-root :around #'projectile-project-root-remote)

(with-eval-after-load 'projectile
  (defun projectile-root-bottom-up (dir &optional list)
    "Identify a project root in DIR by bottom-up search for files in LIST.
If LIST is nil, use `projectile-project-root-files-bottom-up' instead.
Return the first (bottommost) matched directory or nil if not found."
    (cl-some (lambda (name) (projectile-locate-dominating-file dir name))
             (or list projectile-project-root-files-bottom-up))))
