(require 'run-in-vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vterm-maven-dir (file-name-directory (or load-file-name buffer-file-name)))

(defun helm-vterm-maven-deploy-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-maven-deploy-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-maven-deploy-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-maven-deploy-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-maven-deploy-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-maven-deploy-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-maven-deploy-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-maven-deploy-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-maven-option-list ()
  (let* ((project-root-dir (projectile-project-root))
        (current-artifact-dir (helm-vterm-maven-get-current-artifact-dir))
        (parent-artifact-dir (helm-vterm-maven-get-parent-artifact-dir))
        (all-artifact-dirs (mapcar (lambda (host) (cons host host))
                                   (s-split "\n" (shell-command-to-string (format "find %s -maxdepth 4 -name pom.xml | perl -pe 's#%s/##;s#/pom.xml##'" project-root-dir project-root-dir)) t))))
    (if parent-artifact-dir
        (setq all-artifact-dirs (cons parent-artifact-dir all-artifact-dirs)))
    (if current-artifact-dir
        (setq all-artifact-dirs (cons current-artifact-dir all-artifact-dirs)))
    all-artifact-dirs))

(defun helm-vterm-maven-deploy-run (artifact)
  (let* ((vterm-kill-buffer-on-exit nil)
         (project-root-dir (replace-regexp-in-string "/$" "" (projectile-project-root)))
         (default-directory (format "%s/%s" project-root-dir artifact))
         (trigger (if (bound-and-true-p maven-trigger) (format "-Dtrigger=%s" maven-trigger) ""))
         (cmd  (format "mvn -Dmaven.test.skip=true %s -U install deploy" trigger))
         (buffer-name (format "*vterm-maven-deploy-%s/%s*" (file-name-nondirectory project-root-dir) artifact)))
    (lx/run-in-vterm cmd buffer-name default-directory t)))

(defclass helm-vterm-maven-deploy-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-maven-option-list)
   (action :initform 'helm-vterm-maven-deploy-run)))

(setq helm-vterm-maven-deploy-options-list
      (helm-make-source "MAVEN-DEPLOY Artifacts" 'helm-vterm-maven-deploy-options-source))

(setq helm-vterm-maven-deploy-buffers-list
      (helm-make-source "MAVEN-DEPLOY Buffers" 'helm-vterm-maven-deploy-buffers-source))

(defun helm-vterm-maven-deploy ()
  (interactive)
  (helm-other-buffer '(helm-vterm-maven-deploy-buffers-list helm-vterm-maven-deploy-options-list) "*helm-vterm-maven-deploy-buffers*"))

(defun helm-vterm-maven-get-current-artifact-dir ()
  (let* ((dir (expand-file-name default-directory))
         (found nil))
    (while (and (not found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (setq found (s-replace (projectile-project-root) "" dir)))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (if found
        (replace-regexp-in-string "/$" "" found)
      nil)))

(defun helm-vterm-maven-get-parent-artifact-dir ()
  (let* ((dir (expand-file-name default-directory))
         (found nil)
         (parent-found nil))
    (while (and (not parent-found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (if (not found)
            (setq found t)
          (setq parent-found (s-replace (projectile-project-root) "" dir))))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (if parent-found
        (replace-regexp-in-string "/$" "" parent-found))))

(defun helm-vterm-maven-deploy-current-artifact ()
  (interactive)
  (let* ((dir (expand-file-name default-directory))
         (found nil))
    (while (and (not found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (helm-vterm-maven-deploy-run (replace-regexp-in-string "/$/" "" (s-replace (projectile-project-root) "" dir)))
        (setq found t))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "pom not found"))))

(defun helm-vterm-maven-deploy-parent-artifact ()
  (interactive)
  (let* ((dir (expand-file-name default-directory))
         (found nil)
         (parent-found nil))
    (while (and (not parent-found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (if (not found)
            (setq found t)
          (helm-vterm-maven-deploy-run (replace-regexp-in-string "/$/" "" (s-replace (projectile-project-root) "" dir)))
          (setq parent-found t)))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "parent pom not found"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-maven-deps-tree-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-maven-deps-tree-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-maven-deps-tree-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-maven-deps-tree-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-maven-deps-tree-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-maven-deps-tree-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-maven-deps-tree-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-maven-deps-tree-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-maven-deps-tree-run (artifact)
  (let* ((vterm-kill-buffer-on-exit nil)
         (project-root-dir (replace-regexp-in-string "/$" "" (projectile-project-root)))
         (default-directory (format "%s/%s" project-root-dir artifact))
         (trigger (if (bound-and-true-p maven-trigger) (format "-Dtrigger=%s" maven-trigger) ""))
         (cmd  "mvn dependency:tree")
         (buffer-name (format "*vterm-maven-deps-tree-%s/%s*" (file-name-nondirectory project-root-dir) artifact)))
    (lx/run-in-vterm cmd buffer-name default-directory t)))

(defclass helm-vterm-maven-deps-tree-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-maven-option-list)
   (action :initform 'helm-vterm-maven-deps-tree-run)))

(setq helm-vterm-maven-deps-tree-options-list
      (helm-make-source "MAVEN-DEPS-TREE Artifacts" 'helm-vterm-maven-deps-tree-options-source))

(setq helm-vterm-maven-deps-tree-buffers-list
      (helm-make-source "MAVEN-DEPS-TREE Buffers" 'helm-vterm-maven-deps-tree-buffers-source))

(defun helm-vterm-maven-deps-tree ()
  (interactive)
  (helm-other-buffer '(helm-vterm-maven-deps-tree-buffers-list helm-vterm-maven-deps-tree-options-list) "*helm-vterm-maven-deps-tree-buffers*"))

(defun helm-vterm-maven-deps-tree-current-artifact ()
  (interactive)
  (let* ((dir default-directory)
         (found nil))
    (while (and (not found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (helm-vterm-maven-deps-tree-run (s-replace (projectile-project-root) "" dir))
        (setq found t))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "pom not found"))))

(defun helm-vterm-maven-deps-tree-parent-artifact ()
  (interactive)
  (let* ((dir default-directory)
         (found nil)
         (parent-found nil))
    (while (and (not parent-found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (if (not found)
            (setq found t)
          (helm-vterm-maven-deps-tree-run (s-replace (projectile-project-root) "" dir))
          (setq parent-found t)))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "parent pom not found"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-maven-deps-resolve-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-maven-deps-resolve-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-maven-deps-resolve-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-maven-deps-resolve-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-maven-deps-resolve-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-maven-deps-resolve-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-maven-deps-resolve-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-maven-deps-resolve-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-maven-deps-resolve-run (artifact)
  (let* ((vterm-kill-buffer-on-exit nil)
         (project-root-dir (replace-regexp-in-string "/$" "" (projectile-project-root)))
         (default-directory (format "%s/%s" project-root-dir artifact))
         (trigger (if (bound-and-true-p maven-trigger) (format "-Dtrigger=%s" maven-trigger) ""))
         (cmd  "mvn dependency:resolve")
         (buffer-name (format "*vterm-maven-deps-resolve-%s/%s*" (file-name-nondirectory project-root-dir) artifact)))
    (lx/run-in-vterm cmd buffer-name default-directory t)))

(defclass helm-vterm-maven-deps-resolve-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-maven-option-list)
   (action :initform 'helm-vterm-maven-deps-resolve-run)))

(setq helm-vterm-maven-deps-resolve-options-list
      (helm-make-source "MAVEN-DEPS-RESOLVE Artifacts" 'helm-vterm-maven-deps-resolve-options-source))

(setq helm-vterm-maven-deps-resolve-buffers-list
      (helm-make-source "MAVEN-DEPS-RESOLVE Buffers" 'helm-vterm-maven-deps-resolve-buffers-source))

(defun vterm-maven-update-local-artifact ()
  (interactive)
  (let ((vterm-kill-buffer-on-exit nil))
    (lx/run-in-vterm (format "%s/vterm-maven-update-local-artifact.sh" vterm-maven-dir) "*vterm-maven-update-local-artifact*" nil t)))

(defun vterm-maven-kill-local-artifact ()
  (interactive)
  (let ((vterm-kill-buffer-on-exit nil))
    (lx/run-in-vterm (format "%s/vterm-maven-kill-local-artifact.sh" vterm-maven-dir) "*vterm-maven-kill-local-artifact*" nil t)))

(defun helm-vterm-maven-deps-resolve ()
  (interactive)
  (helm-other-buffer '(helm-vterm-maven-deps-resolve-buffers-list helm-vterm-maven-deps-resolve-options-list) "*helm-vterm-maven-deps-resolve-buffers*"))


(provide 'vterm-maven)
