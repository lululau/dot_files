(require 'run-in-ghostel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-ghostel-jenkins-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-ghostel-jenkins-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-ghostel-jenkins-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-ghostel-jenkins-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'ghostel-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*ghostel-jk-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-ghostel-jenkins-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-ghostel-jenkins-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-ghostel-jenkins-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-ghostel-jenkins-run (alias)
  (let* ((ghostel-kill-buffer-on-exit nil)
         (alias-name (replace-regexp-in-string ":.*" "" alias))
         (jenkins-project-name (replace-regexp-in-string ".*build\\s-*\\|:.*" "" alias))
         (cmd  (format "jk %s" alias-name))
         (buffer-name (format "*ghostel-jk-%s-%s*" alias-name jenkins-project-name)))
    (lx/run-in-ghostel cmd buffer-name nil t)))

(defun helm-ghostel-jenkins-option-list ()
  (let ((project-root-dir (projectile-project-root)))
    (mapcar (lambda (alias) (cons alias alias))
            (s-split "\n" (shell-command-to-string "yq e .aliases ~/.jenkins-builder.yaml | grep 'build'") t))))

(defclass helm-ghostel-jenkins-options-source (helm-source-sync)
  ((candidates :initform 'helm-ghostel-jenkins-option-list)
   (action :initform 'helm-ghostel-jenkins-run)))

(setq helm-ghostel-jenkins-options-list
      (helm-make-source "JENKINS Aliases" 'helm-ghostel-jenkins-options-source))

(setq helm-ghostel-jenkins-buffers-list
      (helm-make-source "JENKINS Buffers" 'helm-ghostel-jenkins-buffers-source))

(defun helm-ghostel-jenkins ()
  (interactive)
  (let ((default-directory "~"))
    (helm-other-buffer '(helm-ghostel-jenkins-buffers-list helm-ghostel-jenkins-options-list) "*helm-ghostel-jenkins-buffers*")))


(provide 'ghostel-jenkins)
