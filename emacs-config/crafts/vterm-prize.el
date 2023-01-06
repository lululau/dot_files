(require 'run-in-vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-prize-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-prize-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-prize-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-prize-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-prize-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-prize-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-prize-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-prize-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-prize-option-list ()
  (mapcar (lambda (conf) (let ((fields (s-split ":" conf t))) (cons (car fields) conf)))
          (s-split "\n" (f-read-text "~/.prize.yaml" 'utf-8) t)))

(defun helm-vterm-prize-run-function (conf)
  (let* ((fields (s-split ":" conf t))
         (name (car fields))
         (args (cadr fields))
         (cmd  (format "~/.rvm/gems/ruby-3.2.0/bin/prize %s" args))
         (buffer-name (format "*vterm-prize-%s*" name)))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defun helm-vterm-prize-run ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-vterm-prize-run-function)))

(defvar helm-vterm-prize-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map) map))

(defclass helm-vterm-prize-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-prize-option-list)
   (action :initform 'helm-vterm-prize-run-function)
   (keymap :initform 'helm-vterm-prize-map)))

(setq helm-vterm-prize-options-list
      (helm-make-source "PRIZE Environments" 'helm-vterm-prize-options-source))

(setq helm-vterm-prize-buffers-list
      (helm-make-source "PRIZE Buffers" 'helm-vterm-prize-buffers-source))

(defun helm-vterm-prize ()
  (interactive)
  (let ((default-directory "~"))
    (helm-other-buffer '(helm-vterm-prize-buffers-list helm-vterm-prize-options-list) "*helm-vterm-prize-buffers*")))

(provide 'vterm-prize)
