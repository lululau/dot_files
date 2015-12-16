
;;;;; package hacks;;;;;;

(defun projectile-switch-to-project-last-buffer (project)
  (interactive)
  (let* ((default-directory project)
        (buffers (projectile-project-buffers)))
    (if (cadr buffers)
        (switch-to-buffer (cadr buffers))
      (let ((projectile-completion-system 'helm))
        (projectile-switch-project-by-name project)))))

(with-eval-after-load "helm-projectile"
  (setq helm-source-projectile-projects
    (helm-build-in-buffer-source "Projectile projects"
      :data (lambda ()
              (if (projectile-project-p)
                  (cons (abbreviate-file-name (projectile-project-root))
                        (projectile-relevant-known-projects))
                projectile-known-projects))
      :fuzzy-match helm-projectile-fuzzy-match
      :keymap helm-projectile-projects-map
      :mode-line helm-read-file-name-mode-line-string
      :action '(("Switch to project" .
                (lambda (project)
                  (let ((projectile-completion-system 'helm))
                    (projectile-switch-project-by-name project))))
                ("Switch to last visited buffer in project `<s-return>'" . projectile-switch-to-project-last-buffer)
                ("Open Dired in project's directory `C-d'" . dired)
                ("Open project root in vc-dir or magit `M-g'" . helm-projectile-vc)
                ("Switch to Eshell `M-e'" . helm-projectile-switch-to-eshell)
                ("Grep in projects `C-s'" . helm-projectile-grep)
                ("Compile project `M-c'. With C-u, new compile command"
                . helm-projectile-compile-project)
                ("Remove project(s) `M-D'" . helm-projectile-remove-known-project))))

  (helm-projectile-define-key helm-projectile-projects-map (kbd "<s-return>") #'projectile-switch-to-project-last-buffer)
  )
