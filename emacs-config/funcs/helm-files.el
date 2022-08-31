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
