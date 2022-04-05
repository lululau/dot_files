(require 'vterm)

(defun lx/run-in-pry-vterm (command buffer-name &optional directory exclusive-window)
  (interactive)
  (let* ((buffer (get-buffer buffer-name)))
    (set (intern (format "%s-command" buffer-name)) (list command buffer-name directory exclusive-window))
    (set (intern (format "%s-process-environment" buffer-name)) process-environment)
    (set (intern (format "%s-kill-buffer-on-exit" buffer-name)) (bound-and-true-p vterm-kill-buffer-on-exit))
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
        (pry-vterm buffer-name)))))

(defun pry-vterm (&optional arg)
  "Create an interactive Vterm buffer.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name'."
  (interactive "P")
  (pry-vterm--internal #'pop-to-buffer-same-window arg))

(defun pry-vterm--internal (pop-to-buf-fun &optional arg)
  (cl-assert vterm-buffer-name)
  (let ((buf (cond ((numberp arg)
                    (get-buffer-create (format "%s<%d>"
                                               vterm-buffer-name
                                               arg)))
                   ((stringp arg) (generate-new-buffer arg))
                   (arg (generate-new-buffer vterm-buffer-name))
                   (t
                    (get-buffer-create vterm-buffer-name)))))
    (cl-assert (and buf (buffer-live-p buf)))
    (funcall pop-to-buf-fun buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'pry-vterm-mode)
        (pry-vterm-mode)))
    buf))

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

(defun pry-vterm-get-current-line ()
  (let* ((start (line-beginning-position))
        (end (point))
        (str (buffer-substring-no-properties start end)))
    (replace-regexp-in-string "^.*[❯>] ?" "" str)))

(defun pry-vterm-accept-copilot-or-send-tab-to-term ()
  (interactive)
  (if copilot--overlay
      (copilot-accept-completion)
    (vterm-send-tab)))

(defvar pry-vterm-mode-map
  (let ((map (copy-keymap vterm-mode-map)))
    (define-key map (kbd "<backtab>") #'copilot-accept-completion)
    (define-key map (kbd "<tab>") #'pry-vterm-accept-copilot-or-send-tab-to-term)
    map))

(define-derived-mode pry-vterm-mode vterm-mode "Pry"
  "Major mode for pry vterm buffer.")

(provide 'pry-vterm)
