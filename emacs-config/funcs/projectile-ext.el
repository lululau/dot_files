;;;###autoload
(defun get-current-persp-project ()
  (let ((persp (get-current-persp))
        persp-name)
    (when persp
      (setq persp-name (persp-name persp))
      (let ((project-dir
             (cond
              ;; 1. Check if persp-name is a directory path
              ((file-directory-p (expand-file-name persp-name))
               (expand-file-name persp-name))
              ;; 2. If it is a project name, find it in known projects
              ((let ((matched-proj (seq-find (lambda (proj)
                                               (string= (projectile-project-name proj) persp-name))
                                             (projectile-known-projects))))
                 (when matched-proj
                   (expand-file-name matched-proj))))
              ;; 3. Fallback to current buffer's projectile project root
              ((and (projectile-project-p)
                    (projectile-project-root))
               (projectile-project-root)))))
        (when project-dir
          (abbreviate-file-name project-dir))))))

;;;###autoload
(defun projectile-project-alternate-buffer ()
  (car
   (--remove
    (or
     (eq it (current-buffer))
     (s-matches? "^[* ]" (buffer-name it)))
    (projectile-project-buffers))))

;;;###autoload
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

;;;###autoload
(defun lx/find-or-create-projectile-snippet-file ()
  (interactive)
  ;; (find-file (format "%s/tmp/snippets.rb" (projectile-project-root))))
  (let* ((persp-project (get-current-persp-project))
         (file (format "%s/tmp/snippets.rb"  persp-project)))
    (if (and persp-project (file-exists-p file))
        (find-file file)
      (message "File not exist: %s" file))))

;;;###autoload
(defun lx/find-or-create-projectile-request-file ()
  (interactive)
  ;; (find-file (format "%s/tmp/requests.http" (projectile-project-root))))
  (let* ((persp-project (get-current-persp-project))
         (file (format "%s/tmp/requests.http"  persp-project)))
    (if (and persp-project (file-exists-p file))
        (find-file file)
      (message "File not exist: %s" file))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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


;;;###autoload
(defun lx/find-or-create-projectile-alternate-org (in-other-window)
  (interactive "P")
  (let* ((persp-project (get-current-persp-project))
         (file (format "%s/tmp/traffagent.org"  persp-project)))
    (if (and persp-project (file-exists-p file))
        (if in-other-window
            (find-file-other-window file)
          (find-file file))
      (message "File not exist: %s" file))))

;;;###autoload
(defun projectile-find-file-in-pwd ()
  (interactive)
  (projectile-find-file-in-directory default-directory))

;;;###autoload
(defun lx/open-persp-project-file (in-other-window)
  "Open current persp project file"
  (interactive "P")
  (let ((current-persp-project (get-current-persp-project)))
    (when current-persp-project
      (if in-other-window
          (find-file-other-window current-persp-project)
        (find-file current-persp-project)))))

;;;###autoload
(defun lx/open-magit-toplevel-file (in-other-window)
  "Open file in magit toplevel directory"
  (interactive "P")
  (if in-other-window
      (find-file-other-window (magit-toplevel))
    (find-file (magit-toplevel))))
