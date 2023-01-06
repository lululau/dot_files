(defun projectile-switch-to-project-last-buffer (project)
  (interactive)
  (let* ((default-directory project)
         (buffers (projectile-project-buffers)))
    (if (cadr buffers)
        (switch-to-buffer (cadr buffers))
      (let ((projectile-completion-system 'helm))
        (projectile-switch-project-by-name project)))))

(with-eval-after-load 'helm-projectile
  (defun remote-recentf-files ()
    (let* ((remote-host (with-helm-current-buffer (lx/get-remote-buffer-host))))
      (mapcar (lambda (file)
                (cons file file))
      (seq-filter (lambda (file)
                    (s-matches? (concat remote-host ":") file))
                  recentf-list))))

  (setq helm-source-projectile-recentf-list
    (helm-build-sync-source "Projectile recent files"
      :candidates (lambda ()
                    (if (with-helm-current-buffer (lx/is-remote-buffer))
                        (remote-recentf-files)
                      (when (projectile-project-p)
                        (with-helm-current-buffer
                          (helm-projectile--files-display-real (projectile-recentf-files)
                                                               (projectile-project-root))))))
      :fuzzy-match helm-projectile-fuzzy-match
      :keymap helm-projectile-find-file-map
      :help-message 'helm-ff-help-message
      :mode-line helm-read-file-name-mode-line-string
      :action helm-projectile-file-actions
      :persistent-action #'helm-projectile-file-persistent-action
      :persistent-help "Preview file"))

  (defun helm-projectile-recentf (&optional arg)
    "Use projectile with Helm for finding files in project
With a prefix ARG invalidates the cache first."
    (interactive "P")
    (when (not (lx/is-remote-buffer))
      (if (projectile-project-p)
          (projectile-maybe-invalidate-cache arg)
        (unless nil (error "You're not in a project"))))
    (let ((helm-ff-transformer-show-only-basename nil)
          (helm-boring-file-regexp-list nil)
          (base-buffer-name (if (lx/is-remote-buffer)
                                (lx/get-remote-buffer-host)
                              (projectile-project-name))))
      (helm :sources 'helm-source-projectile-recentf-list
            :buffer (concat "*helm projectile: " base-buffer-name "*")
            :truncate-lines helm-projectile-truncate-lines
            :prompt "Recently visited file: "))))

;; (spacemacs|use-package-add-hook helm-projectile
;;   :post-config
;;   (setq helm-source-projectile-projects
;;         (helm-build-in-buffer-source "Projectile projects"
;;          :data (lambda ()
;;                   (if (projectile-project-p)
;;                       (cons (abbreviate-file-name (projectile-project-root))
;;                             (projectile-relevant-known-projects))
;;                     projectile-known-projects))
;;           :fuzzy-match helm-projectile-fuzzy-match
;;           :keymap helm-projectile-projects-map
;;           :mode-line helm-read-file-name-mode-line-string
;;           :action '(("Switch to project" .
;;                      (lambda (project)
;;                       (let ((projectile-completion-system 'helm))
;;                          (projectile-switch-project-by-name project))))
;;                     ("Switch to last visited buffer in project `<s-return>'" . projectile-switch-to-project-last-buffer)
;;                    ("Open Dired in project's directory `C-d'" . dired)
;;                      ("Open project root in vc-dir or magit `M-g'" . helm-projectile-vc)
;;                     ("Switch to Eshell `M-e'" . helm-projectile-switch-to-eshell)
;;                     ("Grep in projects `C-s'" . helm-projectile-grep)
;;                     ("Compile project `M-c'. With C-u, new compile command"
;;                      . helm-projectile-compile-project)
;;                     ("Remove project(s) `M-D'" . helm-projectile-remove-known-project)))))
