(require 'run-in-vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-jenkins-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-jenkins-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-jenkins-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-jenkins-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-jk-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-jenkins-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-jenkins-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-jenkins-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-jenkins-run (alias)
  (let* ((vterm-kill-buffer-on-exit nil)
         (alias-name (replace-regexp-in-string ":.*" "" alias))
         (jenkins-project-name (replace-regexp-in-string ".*build\\s-*\\|:.*" "" alias))
         (cmd  (format "jk %s" alias-name))
         (buffer-name (format "*vterm-jk-%s-%s*" alias-name jenkins-project-name)))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defun helm-vterm-jenkins-option-list ()
  (let ((project-root-dir (projectile-project-root)))
    (mapcar (lambda (alias) (cons alias alias))
            (s-split "\n" (shell-command-to-string "yq e .aliases ~/.jenkins-builder.yaml | grep 'build'") t))))

(defclass helm-vterm-jenkins-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-jenkins-option-list)
   (action :initform 'helm-vterm-jenkins-run)))

(setq helm-vterm-jenkins-options-list
      (helm-make-source "JENKINS Aliases" 'helm-vterm-jenkins-options-source))

(setq helm-vterm-jenkins-buffers-list
      (helm-make-source "JENKINS Buffers" 'helm-vterm-jenkins-buffers-source))

(defun helm-vterm-jenkins ()
  (interactive)
  (helm-other-buffer '(helm-vterm-jenkins-buffers-list helm-vterm-jenkins-options-list) "*helm-vterm-jenkins-buffers*"))


(provide 'vterm-jenkins)
