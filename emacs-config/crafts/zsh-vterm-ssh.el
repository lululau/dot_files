(require 'zsh-vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ssh-zsh-vterm (&optional arg ssh-options)
  (interactive "P")
  (ssh-zsh-vterm--internal #'pop-to-buffer-same-window arg ssh-options))

(defun ssh-zsh-vterm--internal (pop-to-buf-fun &optional arg ssh-options)
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
      (unless (derived-mode-p 'ssh-zsh-vterm-mode)
        (ssh-zsh-vterm-mode)
        (setq-local ssh-zsh-vterm-ssh-options ssh-options)))
    buf))

(defun  lx/run-ssh-in-zsh-vterm (command buffer-name &optional ssh-options directory window-type)
  (interactive)
  (let* ((buffer-name (or buffer-name (message "zsh %d" (cl-incf zsh-vterm-zsh-buffer-index))))
         (buffer (get-buffer buffer-name))
         (window-type (or window-type 'default)))
    (set (intern (format "%s-command" buffer-name)) (list command buffer-name directory window-type))
    (set (intern (format "%s-process-environment" buffer-name)) process-environment)
    (set (intern (format "%s-kill-buffer-on-exit" buffer-name)) (bound-and-true-p vterm-kill-buffer-on-exit))
    (if buffer
        (if (equal buffer (current-buffer))
            (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
                (bury-buffer)
              (delete-window))
          (setq zsh-vterm-last-buffer (current-buffer))
          (pcase window-type
            ('split (if (get-buffer-window buffer)
                        (select-window (get-buffer-window buffer))
             (pop-to-buffer buffer 'display-buffer-pop-up-window)))
            ('popup (if (get-buffer-window buffer)
                        (select-window (get-buffer-window buffer))
             (select-window (shell-pop-split-window)) (switch-to-buffer buffer)))
            (_ (switch-to-buffer buffer))))

      (let* ((default-directory (or directory user-home-directory))
             (vterm-shell command))
        (pcase window-type
          ('split (split-window-right-and-focus))
          ('popup (select-window (shell-pop-split-window))))

        (setq zsh-vterm-last-buffer (current-buffer))
        (ssh-zsh-vterm buffer-name ssh-options)))))


(defun helm-zsh-vterm-ssh-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-zsh-vterm-ssh-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-zsh-vterm-ssh-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-zsh-vterm-ssh-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'ssh-zsh-vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*zsh-vterm-ssh-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-zsh-vterm-ssh-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-zsh-vterm-ssh-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-zsh-vterm-ssh-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-zsh-vterm-ssh-option-list ()
  (mapcar (lambda (candidate)
            (let ((host (car (s-split " " candidate))))
            (cons candidate host)))
          (s-split "\n" (shell-command-to-string "ruby -e 'h=nil;ARGF.readlines.each {|l| l.chomp!; if l=~/^Host\\s+\\w/; puts h unless h.nil?; h=l.gsub(/^Host\\s+/, \"\"); end; if l=~/^\\s+Host[Nn]ame\\s+\\S/; puts \"%-32s [ #{l.gsub(/^\\s+Host.ame\\s+/,\"\")} ]\" % h; h=nil; end;}' ~/.ssh/config") t)))

(defun helm-zsh-vterm-ssh-run (host)
  (let ((process-environment '("SSH_INTERACTIVE=1"))
        (cmd (format "ssh %s" host))
        (buffer-name (format "*zsh-vterm-ssh-%s*" host)))
    (lx/run-ssh-in-zsh-vterm cmd buffer-name (plist-put nil :host host))))

(defun helm-zsh-vterm-ssh-run-without-interactive-environ ()
  (interactive)
  (with-helm-alive-p (helm-exit-and-execute-action #'helm-zsh-vterm-ssh-run-without-interactive-environ-action)))

(defun helm-zsh-vterm-ssh-run-without-interactive-environ-action (host)
  (let* ((cmd (format "ssh %s" host))
        (buffer-name (format "*zsh-vterm-ssh-%s*" host)))
    (lx/run-ssh-in-zsh-vterm cmd buffer-name (plist-put nil :host host))))

(defvar helm-zsh-vterm-ssh-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<s-return>") #'helm-zsh-vterm-ssh-run-without-interactive-environ)
    map))

(defclass helm-zsh-vterm-ssh-options-source (helm-source-sync)
  ((candidates :initform 'helm-zsh-vterm-ssh-option-list)
   (action :initform '(("SSH (with SSH_INTERACTIVE environ set)" . helm-zsh-vterm-ssh-run) ("SSH (without SSH_INTERACTIVE environ set)" . helm-zsh-vterm-ssh-run-without-interactive-environ-action)))
   (keymap :initform helm-zsh-vterm-ssh-map)))

(setq helm-zsh-vterm-ssh-buffers-list
      (helm-make-source "SSH Buffers" 'helm-zsh-vterm-ssh-buffers-source))

(setq helm-zsh-vterm-ssh-options-list
      (helm-make-source "SSH Hosts" 'helm-zsh-vterm-ssh-options-source))

(defun helm-zsh-vterm-ssh ()
  (interactive)
  (helm-other-buffer '(helm-zsh-vterm-ssh-buffers-list helm-zsh-vterm-ssh-options-list) "*helm-zsh-vterm-ssh-buffers*"))

(defvar ssh-zsh-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map zsh-vterm-mode-map)
    map))

(define-derived-mode ssh-zsh-vterm-mode zsh-vterm-mode "ssh"
  "Major mode for ssh zsh vterm buffer.")

(provide 'zsh-vterm-ssh)
