(require 'pry-vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-pry-vterm-arql-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-pry-vterm-arql-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-pry-vterm-arql-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-pry-vterm-arql-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'pry-vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*arql-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-pry-vterm-arql-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-pry-vterm-arql-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-pry-vterm-arql-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-pry-vterm-arql-option-list ()
  (mapcar (lambda (host) (cons host host))
          (s-split "\n" (shell-command-to-string "perl -ne 'if (/^\\w.*:\\s*$/) {s/:$//; print;}' ~/.arql.d/init.yml") t)))

(defun helm-pry-vterm-arql-run (env)
  (let ((cmd  (format "~/.rvm/gems/ruby-3.1.0/bin/arql -e %s" env))
        (buffer-name (format "*arql-%s*" env)))
    (lx/run-in-pry-vterm cmd buffer-name nil t)))

(defclass helm-pry-vterm-arql-options-source (helm-source-sync)
  ((candidates :initform 'helm-pry-vterm-arql-option-list)
   (action :initform 'helm-pry-vterm-arql-run)))

(setq helm-pry-vterm-arql-options-list
      (helm-make-source "ARQL Environments" 'helm-pry-vterm-arql-options-source))

(setq helm-pry-vterm-arql-buffers-list
      (helm-make-source "ARQL Buffers" 'helm-pry-vterm-arql-buffers-source))

(defun helm-pry-vterm-arql ()
  (interactive)
  (helm-other-buffer '(helm-pry-vterm-arql-buffers-list helm-pry-vterm-arql-options-list) "*helm-pry-vterm-arql-buffers*"))

(provide 'vterm-arql)
