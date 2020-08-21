(add-to-list 'load-path (file-name-directory load-file-name))

(autoload 'spacemacs/helm-find-files-recursively "helm-find-files-recursively")
(autoload 'lx/open-file-in-lnav "open-file-in-lnav")
(autoload 'mfd-dired "mfd-dired")
(autoload 'mfd-name-dired "mfd-dired")
(autoload 'mfd-grep-dired "mfd-dired")
(autoload 'lx/helm-projectile-open-projects "helm-open-projects")
(autoload 'lx/helm-projectile-other-open-projects "helm-open-projects")
(autoload 'helm-cwd-buffers "helm-cwd-buffers")

(load-file (format "%s/%s" (file-name-directory load-file-name) "spacemacs-override.el"))
