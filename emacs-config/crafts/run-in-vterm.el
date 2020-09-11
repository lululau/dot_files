(defun lx/run-in-vterm (command buffer-name &optional directory exclusive-window)
  (interactive)
  (let* ((buffer (get-buffer buffer-name)))
    (if buffer
        (if (equal buffer (current-buffer))
            (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
                (bury-buffer)
              (delete-window))
          (if exclusive-window
              (switch-to-buffer buffer)
            (pop-to-buffer buffer 'display-buffer-pop-up-window)))
      (let* ((default-directory (or directory user-home-directory))
             (vterm-shell command))
        (unless exclusive-window (split-window-right-and-focus))
        (vterm buffer-name)))))

(defun lx/run-in-vterm/set-green-box-cursor ()
  (interactive)
  (setq cursor-type 'box)
  (set-cursor-color "#00ff00"))

(defun lx/run-in-vterm/set-blue-bar-cursor ()
  (interactive)
  (setq cursor-type 'bar)
  (set-cursor-color "#6db2e9"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (eq 'vterm-mode (with-current-buffer b major-mode)))
                        (buffer-list)))))

(defclass helm-vterm-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(setq helm-vterm-buffers-list
      (helm-make-source "VTerm Buffers" 'helm-vterm-buffers-source))

(defun helm-vterm-buffers ()
  (interactive)
  (helm-other-buffer '(helm-vterm-buffers-list) "*helm-vterm-buffers*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-ssh-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-ssh-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-ssh-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-ssh-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-ssh-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-ssh-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-ssh-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-ssh-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-ssh-option-list ()
  (mapcar (lambda (host) (cons host host))
          (s-split "\n" (shell-command-to-string "perl -ne 'if (/^Host [^*]/) {s/^Host *//; print;}' ~/.ssh/config") t)))

(defun helm-vterm-ssh-run (host)
  (let ((process-environment '("SSH_INTERACTIVE=1"))
        (cmd (format "ssh %s" host))
        (buffer-name (format "*vterm-ssh-%s*" host)))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defclass helm-vterm-ssh-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-ssh-option-list)
   (action :initform 'helm-vterm-ssh-run)))

(setq helm-vterm-ssh-buffers-list
      (helm-make-source "SSH Buffers" 'helm-vterm-ssh-buffers-source))

(setq helm-vterm-ssh-options-list
      (helm-make-source "SSH Hosts" 'helm-vterm-ssh-options-source))

(defun helm-vterm-ssh ()
  (interactive)
  (helm-other-buffer '(helm-vterm-ssh-buffers-list helm-vterm-ssh-options-list) "*helm-vterm-ssh-buffers*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-arql-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-arql-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-arql-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-arql-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*arql-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-arql-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-arql-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-arql-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-arql-option-list ()
  (mapcar (lambda (host) (cons host host))
          (s-split "\n" (shell-command-to-string "perl -ne 'unless (/^default/) { if (/^\\S/) {s/:$//; print;}}' ~/.arql.d/init.yml") t)))

(defun helm-vterm-arql-run (env)
  (let ((cmd  (format "~/.rvm/gems/ruby-2.7.0/bin/arql -e %s" env))
        (buffer-name (format "*arql-%s*" env)))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defclass helm-vterm-arql-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-arql-option-list)
   (action :initform 'helm-vterm-arql-run)))

(setq helm-vterm-arql-options-list
      (helm-make-source "ARQL Environments" 'helm-vterm-arql-options-source))

(setq helm-vterm-arql-buffers-list
      (helm-make-source "ARQL Buffers" 'helm-vterm-arql-buffers-source))

(defun helm-vterm-arql ()
  (interactive)
  (helm-other-buffer '(helm-vterm-arql-buffers-list helm-vterm-arql-options-list) "*helm-vterm-arql-buffers*"))

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

(defun helm-vterm-vrl-run (env)
  (let ((cmd  (format "~/bin/vrl %s" env))
        (buffer-name (format "*vterm-vrl-%s*" env)))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defclass helm-vterm-vrl-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-vrl-option-list)
   (action :initform 'helm-vterm-vrl-run)))

(setq helm-vterm-vrl-options-list
      (helm-make-source "VRL Environments" 'helm-vterm-vrl-options-source))

(setq helm-vterm-vrl-buffers-list
      (helm-make-source "VRL Buffers" 'helm-vterm-vrl-buffers-source))

(defun helm-vterm-vrl ()
  (interactive)
  (helm-other-buffer '(helm-vterm-vrl-buffers-list helm-vterm-vrl-options-list) "*helm-vterm-vrl-buffers*"))


(provide 'run-in-vterm)
