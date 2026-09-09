(require 'zsh-ghostel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ssh-zsh-ghostel-tramp-method "rpc"
  "TRAMP method for all `ssh-zsh-ghostel-mode' buffers (OSC 7 and initial seed).")

(defun ssh-zsh-ghostel--tramp-method ()
  "TRAMP method name for `ssh-zsh-ghostel' remote paths."
  ssh-zsh-ghostel-tramp-method)

(defun ssh-zsh-ghostel--remote-host (ssh-options &optional command)
  "Return remote host from SSH-OPTIONS plist or parse COMMAND (e.g. \"ssh host\")."
  (or (plist-get ssh-options :host)
      (when (and (stringp command)
                 (string-match "ssh\\(?:\\s-+[^\\s-]+\\)*\\s-+\\([^\\s-]+\\)" command))
        (match-string 1 command))))

(defun ssh-zsh-ghostel--tramp-default-directory (host &optional dir)
  "Build TRAMP `default-directory' for HOST; remote DIR defaults to \"/\"."
  (file-name-as-directory
   (format "/%s:%s:%s"
           ssh-zsh-ghostel-tramp-method
           host
           (or dir "/"))))

(defun ssh-zsh-ghostel--rpc-prefix-p (path)
  "Non-nil when PATH is a remote file name using `ssh-zsh-ghostel-tramp-method'."
  (and (stringp path)
       (string-match (format "\\`/%s:" (regexp-quote ssh-zsh-ghostel-tramp-method))
                     path)))

(defun ssh-zsh-ghostel--prepare-for-directory-update (&rest _)
  "Before `ghostel--update-directory': force rpc and a reusable TRAMP prefix.

`ghostel--update-directory' reuses `(file-remote-p default-directory)' when
set; otherwise it builds a path from `ghostel-tramp-default-method' or
`tramp-default-method' (often \"scp\").  OSC 7 can arrive before our seed
runs, or while `default-directory' is still local — then the path flips to
/scp:.  This hook keeps ssh ghostel buffers on /rpc:."
  (when (derived-mode-p 'ssh-zsh-ghostel-mode)
    (setq-local ghostel-tramp-default-method (intern ssh-zsh-ghostel-tramp-method))
    (when-let* ((host (ssh-zsh-ghostel--remote-host ssh-zsh-ghostel-ssh-options)))
      (let ((dd default-directory))
        (cond
         ((and (file-remote-p dd) (ssh-zsh-ghostel--rpc-prefix-p dd))
          nil)
         ((file-remote-p dd)
          ;; e.g. /scp:lx:/foo from a prior OSC 7 — rewrite method, keep path.
          (with-parsed-tramp-file-name dd nil
            (setq default-directory
                  (ssh-zsh-ghostel--tramp-default-directory host localname))))
         (t
          (setq default-directory
                (ssh-zsh-ghostel--tramp-default-directory host "/"))))))))

(defun ssh-zsh-ghostel--apply-default-directory (ssh-options &optional remote-dir command)
  "Set buffer `default-directory' to a TRAMP path for the SSH session host.
Uses the same \"/METHOD:HOST:DIR\" shape as `ghostel--update-directory' (OSC 7).
Does nothing when no host can be determined.  REMOTE-DIR defaults to \"/\"."
  (when-let* ((host (ssh-zsh-ghostel--remote-host ssh-options command)))
    (let ((tramp-dir (ssh-zsh-ghostel--tramp-default-directory host remote-dir)))
      (setq default-directory tramp-dir
            list-buffers-directory tramp-dir))))

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
    ;; ghostel--init-buffer takes optional ROWS/COLS, not an identity string.
    (ghostel--init-buffer buf)
    (with-current-buffer buf
      (setq ghostel-identity `((kind . ssh-zsh-ghostel)
                               (name . ,(buffer-name buf))
                               (instance . 1))
            ghostel--managed-buffer-name (buffer-name)
            ghostel--initial-name (buffer-name))
      (ghostel--start-process)
      (ghostel--apply-initial-input-mode)
      ;; OSC 7 may never arrive; seed TRAMP cwd for docker, find-file, etc.
      (ssh-zsh-ghostel--apply-default-directory ssh-options))
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

      (let* ((remote-dir directory)
             (command-parts (split-string-and-unquote command))
             (buffer (generate-new-buffer buffer-name)))
        (pcase window-type
          ('split (split-window-right-and-focus))
          ('popup (select-window (shell-pop-split-window))))

        (setq zsh-ghostel-last-buffer (current-buffer))
        (with-current-buffer buffer
          (ssh-zsh-ghostel-mode)
          (setq-local ssh-zsh-ghostel-ssh-options ssh-options)
          (setq ghostel--managed-buffer-name ""))
        (pop-to-buffer buffer (append display-buffer--same-window-action
                                      '((category . comint))))
        ;; Keep local `default-directory' during spawn: remote TRAMP path would
        ;; make `ghostel-exec' run PROGRAM on the remote host instead of `ssh'.
        (let ((default-directory user-home-directory))
          (ghostel-exec buffer (car command-parts) (cdr command-parts)))
        (with-current-buffer buffer
          (ssh-zsh-ghostel--apply-default-directory ssh-options remote-dir command))))))


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
  ;; Must cons onto existing process-environment; replacing it entirely
  ;; drops SSH_AUTH_SOCK and breaks agent forwarding (ssh -A / ForwardAgent).
  (let ((process-environment (cons "SSH_INTERACTIVE=1" process-environment))
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
  "Major mode for ssh zsh ghostel buffer."
  (setq-local ghostel-tramp-default-method (intern ssh-zsh-ghostel-tramp-method)))

(with-eval-after-load 'ghostel
  (advice-add #'ghostel--update-directory :before #'ssh-zsh-ghostel--prepare-for-directory-update))

(provide 'zsh-ghostel-ssh)
