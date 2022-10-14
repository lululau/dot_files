(require 'run-in-vterm)

(defun vterm-rails/run (command buffer-name-suffix &optional project-name directory exclusive-window)
  (interactive)
  (let* ((project-name (or project-name (projectile-project-name)))
         (buffer-name (format "*%s:%s*" project-name buffer-name-suffix))
         (buffer (get-buffer buffer-name))
         (directory (or directory (projectile-project-root))))
    (if (not (and buffer (buffer-live-p buffer)))
        (rvm-activate-corresponding-ruby))
    (lx/run-in-vterm command buffer-name directory exclusive-window)))


(defun vterm-rails/rails-server (port)
  (interactive "P")
  (vterm-rails/run (format "bundle exec rails server -p %d" (or port 3000)) "rails-server"))


(defun vterm-rails/rails-dev ()
  (interactive)
  (vterm-rails/run "./bin/dev" "rails-dev"))


(defun vterm-rails/sidekiq ()
  (interactive)
  (vterm-rails/run "sidekiq" "sidekiq"))


(provide 'vterm-rails)
