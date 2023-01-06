(defun lx/helm-find-project-files ()
  (interactive)
  (require 'helm-find)
  (let ((directory
         (projectile-project-root)))
    (helm-find-1 directory)))

(defun lx/helm-dired-histories ()
  (interactive)
  (helm :prompt "Dired Histories: "
        :buffer "*helm-dired-histories*"
        :sources
        (list (helm-build-sync-source (format "%s History" "Dired")
                :header-name (lambda (name)
                               (concat name (substitute-command-keys
                                             helm-find-files-doc-header)))
                :mode-line  helm-read-file-name-mode-line-string
                :candidates helm-dired-history-variable
                :nohighlight t
                :fuzzy-match t
                :action 'find-file))))

(defun lx/projectile-dired-histories ()
  (cond ((with-helm-current-buffer (lx/is-remote-buffer))
         (with-helm-current-buffer (let ((remote-host (lx/get-remote-buffer-host)))
                                     (seq-filter (lambda (dir) (s-matches? (concat remote-host ":") dir)) helm-dired-history-variable))))
        ((projectile-project-p) (let ((project-root (projectile-project-root)))
                                  (seq-filter (lambda (dir) (s-starts-with? project-root dir)) helm-dired-history-variable)))
        (t helm-dired-history-variable)))

(defun lx/helm-projectile-dired-histories ()
  (interactive)
  (helm :prompt "Project Dired Histories: "
        :buffer "*helm-projectile-dired-histories*"
        :sources
        (list (helm-build-sync-source "Projectile Dired History"
                :header-name (lambda (name)
                               (concat name (substitute-command-keys
                                             helm-find-files-doc-header)))
                :mode-line  helm-read-file-name-mode-line-string
                :candidates 'lx/projectile-dired-histories
                :nohighlight t
                :fuzzy-match t
                :action 'find-file))))
