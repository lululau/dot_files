(defun helm-remote-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-remote-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-remote-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-remote-buffer-list ()
  (let* ((remote-host (lx/get-remote-buffer-host)))
    (if remote-host
      (mapcar 'buffer-name (seq-filter (lambda (buf)
                    (string= remote-host (with-current-buffer buf (lx/get-remote-buffer-host))))
                  (buffer-list))))))

(defclass helm-remote-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-remote-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-remote-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(setq helm-remote-buffers-list
      (helm-make-source "REMOTE Buffers" 'helm-remote-buffers-source))

(defun helm-remote-buffers ()
  (interactive)
  (helm-other-buffer '(helm-remote-buffers-list) "*helm-remote-buffers*"))

(provide 'helm-remote-buffers)
