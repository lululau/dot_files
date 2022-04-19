(require 'run-in-vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-vrl-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-vrl-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-vrl-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-vrl-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-vrl-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-vrl-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-vrl-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-vrl-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-vrl-option-list ()
  (mapcar (lambda (host) (cons host host))
          (s-split "\n" (shell-command-to-string "perl -ne 'unless (/^default/) { if (/^\\S/) {s/:$//; print;}}' ~/.vrl.yml") t)))

(defun helm-vterm-vrl-run-function (env)
  (let ((cmd  (format "~/bin/vrl %s" env))
        (buffer-name (format "*vterm-vrl-%s*" env)))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defun helm-vterm-vrl-run ()
  (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-vterm-vrl-run-function)))

(defun helm-vterm-vrl-run-auto-function (env)
  (let ((cmd  (format "~/bin/vrl %s -a" env))
        (buffer-name (format "*vterm-vrl-%s*" env)))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defvar helm-vterm-vrl-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-RET") 'helm-vterm-vrl-run)
    map))

(defclass helm-vterm-vrl-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-vrl-option-list)
   (action :initform (helm-make-actions "vrl auto" 'helm-vterm-vrl-run-auto-function
                                        "vrl" 'helm-vterm-vrl-run-function))
   (keymap :initform 'helm-vterm-vrl-map)))

(setq helm-vterm-vrl-options-list
      (helm-make-source "VRL Environments" 'helm-vterm-vrl-options-source))

(setq helm-vterm-vrl-buffers-list
      (helm-make-source "VRL Buffers" 'helm-vterm-vrl-buffers-source))

(defun helm-vterm-vrl ()
  (interactive)
  (let ((default-directory "~"))
    (helm-other-buffer '(helm-vterm-vrl-buffers-list helm-vterm-vrl-options-list) "*helm-vterm-vrl-buffers*")))

(provide 'vterm-vrl)
