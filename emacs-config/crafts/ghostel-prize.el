(require 'run-in-ghostel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-ghostel-prize-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-ghostel-prize-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-ghostel-prize-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-ghostel-prize-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'ghostel-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*ghostel-prize-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-ghostel-prize-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-ghostel-prize-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-ghostel-prize-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-ghostel-prize-option-list ()
  (mapcar (lambda (conf) (let ((fields (s-split ":" conf t))) (cons (car fields) conf)))
          (s-split "\n" (f-read-text "~/.prize.yaml" 'utf-8) t)))

(defun helm-ghostel-prize-run-function (conf)
  (let* ((fields (s-split ":" conf t))
         (name (car fields))
         (args (cadr fields))
         (cmd  (format "~/.rvm/gems/default/bin/prize %s" args))
         (buffer-name (format "*ghostel-prize-%s*" name)))
    (lx/run-in-ghostel cmd buffer-name nil t)))

(defun helm-ghostel-prize-run ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-ghostel-prize-run-function)))

(defvar helm-ghostel-prize-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map) map))

(defclass helm-ghostel-prize-options-source (helm-source-sync)
  ((candidates :initform 'helm-ghostel-prize-option-list)
   (action :initform 'helm-ghostel-prize-run-function)
   (keymap :initform 'helm-ghostel-prize-map)))

(setq helm-ghostel-prize-options-list
      (helm-make-source "PRIZE Environments" 'helm-ghostel-prize-options-source))

(setq helm-ghostel-prize-buffers-list
      (helm-make-source "PRIZE Buffers" 'helm-ghostel-prize-buffers-source))

(defun helm-ghostel-prize ()
  (interactive)
  (let ((default-directory "~"))
    (helm-other-buffer '(helm-ghostel-prize-buffers-list helm-ghostel-prize-options-list) "*helm-ghostel-prize-buffers*")))

(provide 'ghostel-prize)
