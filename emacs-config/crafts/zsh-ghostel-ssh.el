(require 'zsh-ghostel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ssh-zsh-ghostel (&optional arg ssh-options)
  (interactive "P")
  (ssh-zsh-ghostel--internal #'pop-to-buffer-same-window arg ssh-options))

(defun ssh-zsh-ghostel--internal (pop-to-buf-fun &optional arg ssh-options)
  (cl-assert ghostel-buffer-name)
  (let ((buf (cond ((numberp arg)
                    (get-buffer-create (format "%s<%d>"
                                               ghostel-buffer-name
                                               arg)))
                   ((stringp arg) (generate-new-buffer arg))
                   (arg (generate-new-buffer ghostel-buffer-name))
                   (t
                    (get-buffer-create ghostel-buffer-name)))))
    (cl-assert (and buf (buffer-live-p buf)))
    (funcall pop-to-buf-fun buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'ssh-zsh-ghostel-mode)
        (ssh-zsh-ghostel-mode)
        (setq-local ssh-zsh-ghostel-ssh-options ssh-options)))
    buf))

(defun  lx/run-ssh-in-zsh-ghostel (command buffer-name &optional ssh-options directory window-type)
  (interactive)
  (let* ((buffer-name (or buffer-name (message "zsh %d" (cl-incf zsh-ghostel-zsh-buffer-index))))
         (buffer (get-buffer buffer-name))
         (window-type (or window-type 'default)))
    (set (intern (format "%s-command" buffer-name)) (list command buffer-name directory window-type))
    (set (intern (format "%s-process-environment" buffer-name)) process-environment)
    (set (intern (format "%s-kill-buffer-on-exit" buffer-name)) (bound-and-true-p ghostel-kill-buffer-on-exit))
    (if buffer
        (if (equal buffer (current-buffer))
            (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
                (bury-buffer)
              (delete-window))
          (setq zsh-ghostel-last-buffer (current-buffer))
          (pcase window-type
            ('split (if (get-buffer-window buffer)
                        (select-window (get-buffer-window buffer))
             (pop-to-buffer buffer 'display-buffer-pop-up-window)))
            ('popup (if (get-buffer-window buffer)
                        (select-window (get-buffer-window buffer))
             (select-window (shell-pop-split-window)) (switch-to-buffer buffer)))
            (_ (switch-to-buffer buffer))))

      (let* ((default-directory (or directory user-home-directory))
             (ghostel-shell command))
        (pcase window-type
          ('split (split-window-right-and-focus))
          ('popup (select-window (shell-pop-split-window))))

        (setq zsh-ghostel-last-buffer (current-buffer))
        (ssh-zsh-ghostel buffer-name ssh-options)))))


(defun helm-zsh-ghostel-ssh-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-zsh-ghostel-ssh-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-zsh-ghostel-ssh-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-zsh-ghostel-ssh-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'ssh-zsh-ghostel-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*zsh-ghostel-ssh-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-zsh-ghostel-ssh-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-zsh-ghostel-ssh-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-zsh-ghostel-ssh-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-zsh-ghostel-ssh-option-list ()
  (mapcar (lambda (candidate)
            (let ((host (car (s-split " " candidate))))
            (cons candidate host)))
          (s-split "\n" (shell-command-to-string "ruby -e 'h=nil;ARGF.readlines.each {|l| l.chomp!; if l=~/^Host\\s+\\w/; puts h unless h.nil?; h=l.gsub(/^Host\\s+/, \"\"); end; if l=~/^\\s+Host[Nn]ame\\s+\\S/; puts \"%-32s [ #{l.gsub(/^\\s+Host.ame\\s+/,\"\")} ]\" % h; h=nil; end;}' ~/.ssh/config") t)))

(defun helm-zsh-ghostel-ssh-run (host)
  (let ((process-environment '("SSH_INTERACTIVE=1"))
        (cmd (format "ssh %s" host))
        (buffer-name (format "*zsh-ghostel-ssh-%s*" host)))
    (lx/run-ssh-in-zsh-ghostel cmd buffer-name (plist-put nil :host host))))

(defun helm-zsh-ghostel-ssh-run-without-interactive-environ ()
  (interactive)
  (with-helm-alive-p (helm-exit-and-execute-action #'helm-zsh-ghostel-ssh-run-without-interactive-environ-action)))

(defun helm-zsh-ghostel-ssh-run-without-interactive-environ-action (host)
  (let* ((cmd (format "ssh %s" host))
        (buffer-name (format "*zsh-ghostel-ssh-%s*" host)))
    (lx/run-ssh-in-zsh-ghostel cmd buffer-name (plist-put nil :host host))))

(defvar helm-zsh-ghostel-ssh-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<s-return>") #'helm-zsh-ghostel-ssh-run-without-interactive-environ)
    map))

(defclass helm-zsh-ghostel-ssh-options-source (helm-source-sync)
  ((candidates :initform 'helm-zsh-ghostel-ssh-option-list)
   (action :initform '(("SSH (with SSH_INTERACTIVE environ set)" . helm-zsh-ghostel-ssh-run) ("SSH (without SSH_INTERACTIVE environ set)" . helm-zsh-ghostel-ssh-run-without-interactive-environ-action)))
   (keymap :initform helm-zsh-ghostel-ssh-map)))

(setq helm-zsh-ghostel-ssh-buffers-list
      (helm-make-source "SSH Buffers" 'helm-zsh-ghostel-ssh-buffers-source))

(setq helm-zsh-ghostel-ssh-options-list
      (helm-make-source "SSH Hosts" 'helm-zsh-ghostel-ssh-options-source))

(defun helm-zsh-ghostel-ssh ()
  (interactive)
  (let ((default-directory "~"))
    (helm-other-buffer '(helm-zsh-ghostel-ssh-buffers-list helm-zsh-ghostel-ssh-options-list) "*helm-zsh-ghostel-ssh-buffers*")))

(defvar ssh-zsh-ghostel-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map zsh-ghostel-mode-map)
    map))

(define-derived-mode ssh-zsh-ghostel-mode zsh-ghostel-mode "ssh"
  "Major mode for ssh zsh ghostel buffer.")

(setplist 'ssh-zsh-ghostel-mode (plist-put (symbol-plist 'ssh-zsh-ghostel-mode) 'insert-function 'ghostel-send-string))

(provide 'zsh-ghostel-ssh)
