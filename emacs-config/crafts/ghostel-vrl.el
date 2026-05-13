(require 'run-in-ghostel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-ghostel-vrl-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-ghostel-vrl-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-ghostel-vrl-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-ghostel-vrl-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'ghostel-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*ghostel-vrl-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-ghostel-vrl-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-ghostel-vrl-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-ghostel-vrl-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-ghostel-vrl-option-list ()
  (mapcar (lambda (host) (cons host host))
          (s-split "\n" (shell-command-to-string "perl -ne 'unless (/^default/) { if (/^\\S/) {s/:$//; print;}}' ~/.vrl.yml") t)))

(defun helm-ghostel-vrl-run-function (env)
  (let ((cmd  (format "~/bin/vrl %s" env))
        (buffer-name (format "*ghostel-vrl-%s*" env)))
    (lx/run-in-ghostel cmd buffer-name nil t)))

(defun helm-ghostel-vrl-run ()
  (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-ghostel-vrl-run-function)))

(defun helm-ghostel-vrl-run-auto-function (env)
  (let ((cmd  (format "~/bin/vrl %s -a" env))
        (buffer-name (format "*ghostel-vrl-%s*" env)))
    (lx/run-in-ghostel cmd buffer-name nil t)))

(defvar helm-ghostel-vrl-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-RET") 'helm-ghostel-vrl-run)
    map))

(defclass helm-ghostel-vrl-options-source (helm-source-sync)
  ((candidates :initform 'helm-ghostel-vrl-option-list)
   (action :initform (helm-make-actions "vrl auto" 'helm-ghostel-vrl-run-auto-function
                                        "vrl" 'helm-ghostel-vrl-run-function))
   (keymap :initform 'helm-ghostel-vrl-map)))

(setq helm-ghostel-vrl-options-list
      (helm-make-source "VRL Environments" 'helm-ghostel-vrl-options-source))

(setq helm-ghostel-vrl-buffers-list
      (helm-make-source "VRL Buffers" 'helm-ghostel-vrl-buffers-source))

(defun helm-ghostel-vrl ()
  (interactive)
  (let ((default-directory "~"))
    (helm-other-buffer '(helm-ghostel-vrl-buffers-list helm-ghostel-vrl-options-list) "*helm-ghostel-vrl-buffers*")))

(provide 'ghostel-vrl)
