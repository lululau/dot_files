(require 'helm)
(require 'helm-source)

(defun lx/projectile-open-projects-all-candidates ()
  (mapcar (lambda (dir)  (cons (projectile-project-name dir) dir))
          (projectile-open-projects)))

(defun lx/projectile-open-projects-other-candidates ()
  (mapcar (lambda (dir)  (cons (projectile-project-name dir) dir))
          (-difference (projectile-open-projects) (persp-names-current-frame-fast-ordered))))

(defclass lx/projectile-open-projects-all-source-type (helm-source-sync)
  ((candidates :initform 'lx/projectile-open-projects-all-candidates)
   (action :initform 'find-file)))

(defclass lx/projectile-open-projects-other-source-type (helm-source-sync)
  ((candidates :initform 'lx/projectile-open-projects-other-candidates)
   (action :initform 'find-file)))

(defvar lx/projectile-open-projects-all-source
  (helm-make-source "Find Open Projects" 'lx/projectile-open-projects-all-source-type))

(defvar lx/projectile-open-projects-other-source
  (helm-make-source "Find Open Projects" 'lx/projectile-open-projects-other-source-type))

(defun lx/helm-projectile-open-projects ()
  (interactive)
  (helm :sources 'lx/projectile-open-projects-all-source
        :buffer "*helm all open projects*"
        :ff-transformer-show-only-basename nil))

(defun lx/helm-projectile-other-open-projects ()
  (interactive)
  (helm :sources 'lx/projectile-open-projects-other-source
        :buffer "*helm other open projects*"
        :ff-transformer-show-only-basename nil))

(provide 'helm-open-projects)
