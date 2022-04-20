(require 'vterm)

(defun lx/run-in-vterm (command buffer-name &optional directory exclusive-window)
  (interactive)
  (let* ((buffer (get-buffer buffer-name)))
    (set (intern (format "%s-command" buffer-name)) (list command buffer-name directory exclusive-window))
    (set (intern (format "%s-process-environment" buffer-name)) process-environment)
    (set (intern (format "%s-kill-buffer-on-exit" buffer-name)) (bound-and-true-p vterm-kill-buffer-on-exit))
    (set (intern (format "%s-kill-buffer-on-normal-exit" buffer-name)) (bound-and-true-p vterm-kill-buffer-on-normal-exit))
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

(defun lx/run-in-vterm/rerun ()
  (interactive)
  (let* ((buffer-name (buffer-name))
         (process (get-buffer-process buffer-name))
         (args (eval (read (format "%s-command" buffer-name))))
         (penv (eval (read (format "%s-process-environment" buffer-name))))
         (kill-on-exit (eval (read (format "%s-kill-buffer-on-exit" buffer-name))))
         (kill-on-normal-exit (eval (read (format "%s-kill-buffer-on-normal-exit" buffer-name)))))
    (if process
        (message "Buffer process still running")
      (progn
        (kill-buffer)
        (let ((process-environment penv)
              (vterm-kill-buffer-on-exit kill-on-exit)
              (vterm-kill-buffer-on-normal-exit kill-on-normal-exit))
          (apply 'lx/run-in-vterm args))))))

(defun lx/run-in-vterm/set-green-box-cursor ()
  (interactive)
  (setq cursor-type 'box)
  (set-cursor-color "#00ff00"))

(defun lx/run-in-vterm/set-blue-bar-cursor ()
  (interactive)
  (setq cursor-type 'bar)
  (set-cursor-color "#6db2e9"))

(defun lx/run-in-vterm/set-default-directory (dir)
  (interactive)
  (setq default-directory dir))

(defun lx/run-in-vterm/find-remote-file (file &optional host)
  (interactive)
  (let* ((pwd default-directory)
         (remote-host (if (eq major-mode 'ssh-zsh-vterm-mode)
                          (plist-get ssh-zsh-vterm-ssh-options :host)
                        nil))
         (file-prefix (if remote-host (format "/scp:%s:" remote-host) ""))
         (file (concat file-prefix file)))
    (find-file-other-window file)))

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
                          (seq-contains-p '(vterm-mode zsh-vterm-mode ssh-zsh-vterm-mode pry-vterm-mode) (with-current-buffer b major-mode)))
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

(provide 'run-in-vterm)
