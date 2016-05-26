(defun update-tags()
  (interactive)
  (projectile-with-default-dir (projectile-project-root) (shell-command "ctags -e -R --languages=-javascript --exclude=.git --exclude=log --exclude=target --fields=+iaS --extra=+q .")))
