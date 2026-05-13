(require 'run-in-ghostel)

(defun ghostel-rails/run (command buffer-name-suffix &optional project-name directory exclusive-window)
  (interactive)
  (let* ((project-name (or project-name (projectile-project-name)))
         (buffer-name (format "*%s:%s*" project-name buffer-name-suffix))
         (buffer (get-buffer buffer-name))
         (directory (or directory (projectile-project-root))))
    (if (not (and buffer (buffer-live-p buffer)))
        (rvm-activate-corresponding-ruby))
    (lx/run-in-ghostel command buffer-name directory exclusive-window)))


(defun ghostel-rails/rails-server (port)
  (interactive "P")
  (ghostel-rails/run (format "bundle exec rails server -p %d" (or port 3000)) "rails-server"))


(defun ghostel-rails/rails-dev ()
  (interactive)
  (ghostel-rails/run "./bin/dev" "rails-dev"))


(defun ghostel-rails/sidekiq ()
  (interactive)
  (ghostel-rails/run "sidekiq" "sidekiq"))


(provide 'ghostel-rails)
