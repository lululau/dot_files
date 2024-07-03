(defun get-current-persp-project ()
  (let ((persp (get-current-persp))
        persp-name)
    (when persp
      (setq persp-name (persp-name persp))
      (when (-contains? (projectile-open-projects) (replace-regexp-in-string "^/[^/]*/[^/]*/" "~/" persp-name))
        persp-name))))

(defun projectile-project-alternate-buffer ()
  (car
   (--remove
    (or
     (eq it (current-buffer))
     (s-matches? "^[* ]" (buffer-name it)))
    (projectile-project-buffers))))

(defun projectile-project-switch-to-alternate-buffer ()
  (interactive)
  (let ((project (get-current-persp-project)))
    (if project
        (if (and (projectile-project-p) (string= (expand-file-name project) (projectile-project-root)))
            (let ((alternate-buffer (projectile-project-alternate-buffer)))
              (if alternate-buffer (switch-to-buffer alternate-buffer)))
          (let* ((default-directory project)
                 (buf (cadr (projectile-project-buffers))))
            (switch-to-buffer buf)))
      (call-interactively #'spacemacs/alternate-buffer-in-persp))))

(defun lx/find-or-create-projectile-snippet-file ()
  (interactive)
  ;; (find-file (format "%s/tmp/snippets.rb" (projectile-project-root))))
  (let* ((persp-project (get-current-persp-project))
         (file (format "%s/tmp/snippets.rb"  persp-project)))
    (if (and persp-project (file-exists-p file))
        (find-file file)
      (message "File not exist: %s" file))))

(defun lx/find-or-create-projectile-request-file ()
  (interactive)
  ;; (find-file (format "%s/tmp/requests.http" (projectile-project-root))))
  (let* ((persp-project (get-current-persp-project))
         (file (format "%s/tmp/requests.http"  persp-project)))
    (if (and persp-project (file-exists-p file))
        (find-file file)
      (message "File not exist: %s" file))))

(defun lx/find-or-create-projectile-snippet-org (in-other-window)
  (interactive "P")
  ;; (find-file (format "%s/tmp/snippets.org" (projectile-project-root))))
  (let* ((persp-project (get-current-persp-project))
         (file (format "%s/tmp/snippets.org"  persp-project)))
    (if (and persp-project (file-exists-p file))
        (if in-other-window
            (find-file-other-window file)
          (find-file file))
      (message "File not exist: %s" file))))

(defun lx/find-or-create-projectile-request-org (in-other-window)
  (interactive "P")
  ;; (find-file (format "%s/tmp/requests.org" (projectile-project-root))))
  (let* ((persp-project (get-current-persp-project))
         (file (format "%s/tmp/requests.org"  persp-project)))
    (if (and persp-project (file-exists-p file))
        (if in-other-window
            (find-file-other-window file)
          (find-file file))
      (message "File not exist: %s" file))))

(defun lx/find-or-create-projectile-sql-org (in-other-window)
  (interactive "P")
  ;; (find-file (format "%s/tmp/sql.org" (projectile-project-root))))
  (let* ((persp-project (get-current-persp-project))
         (file (format "%s/tmp/sql.org"  persp-project)))
    (if (and persp-project (file-exists-p file))
        (if in-other-window
            (find-file-other-window file)
          (find-file file))
      (message "File not exist: %s" file))))


(defun lx/find-or-create-projectile-alternate-org (in-other-window)
  (interactive "P")
  (let* ((persp-project (get-current-persp-project))
         (file (format "%s/tmp/traffagent.org"  persp-project)))
    (if (and persp-project (file-exists-p file))
        (if in-other-window
            (find-file-other-window file)
          (find-file file))
      (message "File not exist: %s" file))))

(defun projectile-find-file-in-pwd ()
  (interactive)
  (projectile-find-file-in-directory default-directory))
