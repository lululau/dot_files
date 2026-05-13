(require 'run-in-ghostel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghostel-maven-dir (file-name-directory (or load-file-name buffer-file-name)))

(defun helm-ghostel-maven-deploy-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-ghostel-maven-deploy-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-ghostel-maven-deploy-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-ghostel-maven-deploy-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'ghostel-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*ghostel-maven-deploy-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-ghostel-maven-deploy-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-ghostel-maven-deploy-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-ghostel-maven-deploy-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-ghostel-maven-option-list ()
  (let* ((project-root-dir (projectile-project-root))
        (current-artifact-dir (helm-ghostel-maven-get-current-artifact-dir))
        (parent-artifact-dir (helm-ghostel-maven-get-parent-artifact-dir))
        (all-artifact-dirs (mapcar (lambda (host) (cons host host))
                                   (s-split "\n" (shell-command-to-string (format "find %s -maxdepth 4 -name pom.xml | perl -pe 's#%s/##;s#/pom.xml##'" project-root-dir project-root-dir)) t))))
    (if parent-artifact-dir
        (setq all-artifact-dirs (cons parent-artifact-dir all-artifact-dirs)))
    (if current-artifact-dir
        (setq all-artifact-dirs (cons current-artifact-dir all-artifact-dirs)))
    all-artifact-dirs))

(defun helm-ghostel-maven-deploy-run (artifact)
  (let* ((ghostel-kill-buffer-on-exit nil)
         (project-root-dir (replace-regexp-in-string "/$" "" (projectile-project-root)))
         (default-directory (format "%s/%s" project-root-dir artifact))
         (trigger (if (bound-and-true-p maven-trigger) (format "-Dtrigger=%s" maven-trigger) ""))
         (cmd  (format "mvn -Dmaven.test.skip=true %s -U install deploy" trigger))
         (buffer-name (format "*ghostel-maven-deploy-%s/%s*" (file-name-nondirectory project-root-dir) artifact)))
    (lx/run-in-ghostel cmd buffer-name default-directory t)))

(defclass helm-ghostel-maven-deploy-options-source (helm-source-sync)
  ((candidates :initform 'helm-ghostel-maven-option-list)
   (action :initform 'helm-ghostel-maven-deploy-run)))

(setq helm-ghostel-maven-deploy-options-list
      (helm-make-source "MAVEN-DEPLOY Artifacts" 'helm-ghostel-maven-deploy-options-source))

(setq helm-ghostel-maven-deploy-buffers-list
      (helm-make-source "MAVEN-DEPLOY Buffers" 'helm-ghostel-maven-deploy-buffers-source))

(defun helm-ghostel-maven-deploy ()
  (interactive)
  (helm-other-buffer '(helm-ghostel-maven-deploy-buffers-list helm-ghostel-maven-deploy-options-list) "*helm-ghostel-maven-deploy-buffers*"))

(defun helm-ghostel-maven-get-current-artifact-dir ()
  (let* ((dir (expand-file-name default-directory))
         (found nil))
    (while (and (not found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (setq found (s-replace (projectile-project-root) "" dir)))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (if found
        (replace-regexp-in-string "/$" "" found)
      nil)))

(defun helm-ghostel-maven-get-parent-artifact-dir ()
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

(defun helm-ghostel-maven-deploy-current-artifact ()
  (interactive)
  (let* ((dir (expand-file-name default-directory))
         (found nil))
    (while (and (not found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (helm-ghostel-maven-deploy-run (replace-regexp-in-string "/$/" "" (s-replace (projectile-project-root) "" dir)))
        (setq found t))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "pom not found"))))

(defun helm-ghostel-maven-deploy-parent-artifact ()
  (interactive)
  (let* ((dir (expand-file-name default-directory))
         (found nil)
         (parent-found nil))
    (while (and (not parent-found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (if (not found)
            (setq found t)
          (helm-ghostel-maven-deploy-run (replace-regexp-in-string "/$/" "" (s-replace (projectile-project-root) "" dir)))
          (setq parent-found t)))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "parent pom not found"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-ghostel-maven-deps-tree-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-ghostel-maven-deps-tree-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-ghostel-maven-deps-tree-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-ghostel-maven-deps-tree-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'ghostel-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*ghostel-maven-deps-tree-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-ghostel-maven-deps-tree-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-ghostel-maven-deps-tree-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-ghostel-maven-deps-tree-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-ghostel-maven-deps-tree-run (artifact)
  (let* ((ghostel-kill-buffer-on-exit nil)
         (project-root-dir (replace-regexp-in-string "/$" "" (projectile-project-root)))
         (default-directory (format "%s/%s" project-root-dir artifact))
         (trigger (if (bound-and-true-p maven-trigger) (format "-Dtrigger=%s" maven-trigger) ""))
         (cmd  "mvn dependency:tree")
         (buffer-name (format "*ghostel-maven-deps-tree-%s/%s*" (file-name-nondirectory project-root-dir) artifact)))
    (lx/run-in-ghostel cmd buffer-name default-directory t)))

(defclass helm-ghostel-maven-deps-tree-options-source (helm-source-sync)
  ((candidates :initform 'helm-ghostel-maven-option-list)
   (action :initform 'helm-ghostel-maven-deps-tree-run)))

(setq helm-ghostel-maven-deps-tree-options-list
      (helm-make-source "MAVEN-DEPS-TREE Artifacts" 'helm-ghostel-maven-deps-tree-options-source))

(setq helm-ghostel-maven-deps-tree-buffers-list
      (helm-make-source "MAVEN-DEPS-TREE Buffers" 'helm-ghostel-maven-deps-tree-buffers-source))

(defun helm-ghostel-maven-deps-tree ()
  (interactive)
  (helm-other-buffer '(helm-ghostel-maven-deps-tree-buffers-list helm-ghostel-maven-deps-tree-options-list) "*helm-ghostel-maven-deps-tree-buffers*"))

(defun helm-ghostel-maven-deps-tree-current-artifact ()
  (interactive)
  (let* ((dir default-directory)
         (found nil))
    (while (and (not found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (helm-ghostel-maven-deps-tree-run (s-replace (projectile-project-root) "" dir))
        (setq found t))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "pom not found"))))

(defun helm-ghostel-maven-deps-tree-parent-artifact ()
  (interactive)
  (let* ((dir default-directory)
         (found nil)
         (parent-found nil))
    (while (and (not parent-found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (if (not found)
            (setq found t)
          (helm-ghostel-maven-deps-tree-run (s-replace (projectile-project-root) "" dir))
          (setq parent-found t)))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "parent pom not found"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-ghostel-maven-deps-resolve-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-ghostel-maven-deps-resolve-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-ghostel-maven-deps-resolve-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-ghostel-maven-deps-resolve-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'ghostel-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*ghostel-maven-deps-resolve-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-ghostel-maven-deps-resolve-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-ghostel-maven-deps-resolve-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-ghostel-maven-deps-resolve-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-ghostel-maven-deps-resolve-run (artifact)
  (let* ((ghostel-kill-buffer-on-exit nil)
         (project-root-dir (replace-regexp-in-string "/$" "" (projectile-project-root)))
         (default-directory (format "%s/%s" project-root-dir artifact))
         (trigger (if (bound-and-true-p maven-trigger) (format "-Dtrigger=%s" maven-trigger) ""))
         (cmd  "mvn dependency:resolve")
         (buffer-name (format "*ghostel-maven-deps-resolve-%s/%s*" (file-name-nondirectory project-root-dir) artifact)))
    (lx/run-in-ghostel cmd buffer-name default-directory t)))

(defclass helm-ghostel-maven-deps-resolve-options-source (helm-source-sync)
  ((candidates :initform 'helm-ghostel-maven-option-list)
   (action :initform 'helm-ghostel-maven-deps-resolve-run)))

(setq helm-ghostel-maven-deps-resolve-options-list
      (helm-make-source "MAVEN-DEPS-RESOLVE Artifacts" 'helm-ghostel-maven-deps-resolve-options-source))

(setq helm-ghostel-maven-deps-resolve-buffers-list
      (helm-make-source "MAVEN-DEPS-RESOLVE Buffers" 'helm-ghostel-maven-deps-resolve-buffers-source))

(defun ghostel-maven-update-local-artifact ()
  (interactive)
  (let ((ghostel-kill-buffer-on-exit nil))
    (lx/run-in-ghostel (format "%s/ghostel-maven-update-local-artifact.sh" ghostel-maven-dir) "*ghostel-maven-update-local-artifact*" nil t)))

(defun ghostel-maven-kill-local-artifact ()
  (interactive)
  (let ((ghostel-kill-buffer-on-exit nil))
    (lx/run-in-ghostel (format "%s/ghostel-maven-kill-local-artifact.sh" ghostel-maven-dir) "*ghostel-maven-kill-local-artifact*" nil t)))

(defun helm-ghostel-maven-deps-resolve ()
  (interactive)
  (helm-other-buffer '(helm-ghostel-maven-deps-resolve-buffers-list helm-ghostel-maven-deps-resolve-options-list) "*helm-ghostel-maven-deps-resolve-buffers*"))


(provide 'ghostel-maven)
