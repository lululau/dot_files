(require 'pry-ghostel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-pry-ghostel-arql-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-pry-ghostel-arql-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-pry-ghostel-arql-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-pry-ghostel-arql-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'pry-ghostel-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*arql-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-pry-ghostel-arql-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-pry-ghostel-arql-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-pry-ghostel-arql-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-pry-ghostel-arql-option-list ()
  (mapcar (lambda (host) (cons host host))
          (s-split "\n" (shell-command-to-string "perl -ne 'if (/^\\w.*:\\s*$/) {s/:$//; print;}' ~/.arql.d/init.yml") t)))

(defun helm-pry-ghostel-arql-run (env)
  (let ((cmd  (format "~/.rvm/gems/default/bin/arql -e %s" env))
        (buffer-name (format "*arql-%s*" env)))
    (lx/run-in-pry-ghostel cmd buffer-name nil t)))

(defclass helm-pry-ghostel-arql-options-source (helm-source-sync)
  ((candidates :initform 'helm-pry-ghostel-arql-option-list)
   (action :initform 'helm-pry-ghostel-arql-run)))

(setq helm-pry-ghostel-arql-options-list
      (helm-make-source "ARQL Environments" 'helm-pry-ghostel-arql-options-source))

(setq helm-pry-ghostel-arql-buffers-list
      (helm-make-source "ARQL Buffers" 'helm-pry-ghostel-arql-buffers-source))

(defun helm-pry-ghostel-arql ()
  (interactive)
  (let ((default-directory "~"))
    (helm-other-buffer '(helm-pry-ghostel-arql-buffers-list helm-pry-ghostel-arql-options-list) "*helm-pry-ghostel-arql-buffers*")))

(provide 'ghostel-arql)
