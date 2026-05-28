;; -*- lexical-binding: t; -*-

(defun projectile-project-root-remote (origfunc &rest args)
  (let* ((dir (car args))
         (dir (or dir default-directory)))
    (if (not (file-remote-p dir))
        (apply origfunc args))))

(advice-add 'projectile-project-root :around #'projectile-project-root-remote)

(defvar projectile-enable-caching)

(defun lx/projectile-maybe-invalidate-cache-advice (orig-fun force &rest args)
  "Only run `projectile-maybe-invalidate-cache' when caching is enabled or FORCE is non-nil."
  (when (or force projectile-enable-caching)
    (apply orig-fun force args)))

(advice-add 'projectile-maybe-invalidate-cache :around #'lx/projectile-maybe-invalidate-cache-advice)
