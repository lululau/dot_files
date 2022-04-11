(require 'vterm)

(defvar zsh-vterm-last-buffer nil)

(defvar zsh-vterm-prompt-regexp "^.*\\(❯\\|\\]#\\|\\]\\$\\|➜\\) ")

(defvar zsh-vterm-prompt-has-previous-regexp "^.*\\(❯\\|➜\\) ")

(defun lx/run-in-zsh-vterm (command buffer-name &optional directory exclusive-window)
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
          (setq zsh-vterm-last-buffer (current-buffer))
          (if exclusive-window
              (switch-to-buffer buffer)
            (pop-to-buffer buffer 'display-buffer-pop-up-window)))
      (let* ((default-directory (or directory user-home-directory))
             (vterm-shell command))
        (unless exclusive-window (split-window-right-and-focus))
        (setq zsh-vterm-last-buffer (current-buffer))
        (zsh-vterm buffer-name)))))

(defun zsh-vterm (&optional arg)
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
  (zsh-vterm--internal #'pop-to-buffer-same-window arg))

(defun zsh-vterm--internal (pop-to-buf-fun &optional arg)
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
      (unless (derived-mode-p 'zsh-vterm-mode)
        (zsh-vterm-mode)))
    buf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lx/run-shell-in-zsh-vterm (command buffer-name &optional directory exclusive-window)
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
        (zsh-vterm buffer-name)))))


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
                          (and (eq 'zsh-vterm-mode (with-current-buffer b major-mode))
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
    (lx/run-shell-in-zsh-vterm cmd buffer-name nil t)))

(defun helm-zsh-vterm-ssh-run-without-interactive-environ ()
  (interactive)
  (with-helm-alive-p (helm-exit-and-execute-action #'helm-zsh-vterm-ssh-run-without-interactive-environ-action)))

(defun helm-zsh-vterm-ssh-run-without-interactive-environ-action (host)
  (let* ((cmd (format "ssh %s" host))
        (buffer-name (format "*zsh-vterm-ssh-%s*" host)))
    (lx/run-shell-in-zsh-vterm cmd buffer-name nil t)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zsh-vterm-previous-cli-output ()
  (if (string-match-p zsh-vterm-prompt-has-previous-regexp (thing-at-point 'line))
      (evil-previous-line))
  (evil-previous-line)
  (evil-visual-line)
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute (concat "?" zsh-vterm-prompt-regexp)))
  (evil-next-line)
  (beginning-of-line))

(defun zsh-vterm-previous-cli-command ()
  (evil-previous-line)
  (beginning-of-line)
  (string-match zsh-vterm-prompt-regexp (thing-at-point 'line))
  (forward-char (match-end 0))
  (evil-visual-char)
  (evil-end-of-line)
  (evil-backward-WORD-end))

(defun zsh-vterm-previous-cli ()
  (interactive)
  (if (eq evil-state 'visual) (evil-exit-visual-state))
  (evil-normal-state)
  (if (string-match-p zsh-vterm-prompt-regexp (thing-at-point 'line))
      (zsh-vterm-previous-cli-output)
    (zsh-vterm-previous-cli-command)))

(defun zsh-vterm-next-cli-output ()
  (evil-next-line)
  (evil-visual-line)
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute (concat "/" zsh-vterm-prompt-regexp)))
  (if (string-match-p zsh-vterm-prompt-has-previous-regexp (thing-at-point 'line))
      (evil-previous-line))
  (evil-previous-line)
  (beginning-of-line))

(defun zsh-vterm-next-cli-command ()
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute (concat "/" zsh-vterm-prompt-regexp)))
  (beginning-of-line)
  (string-match zsh-vterm-prompt-regexp (thing-at-point 'line))
  (forward-char (match-end 0))
  (evil-visual-char)
  (evil-end-of-line)
  (evil-backward-WORD-end))

(defun zsh-vterm-next-cli ()
  (interactive)
  (if (eq evil-state 'visual) (evil-exit-visual-state))
  (evil-normal-state)
  (if (string-match-p zsh-vterm-prompt-regexp (thing-at-point 'line))
      (zsh-vterm-next-cli-output)
    (zsh-vterm-next-cli-command)))

(evil-define-operator evil-yank-for-zsh-vterm (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((evil-was-yanked-without-register
         (and evil-was-yanked-without-register (not register))))
    (cond
     ((and (fboundp 'cua--global-mark-active)
           (fboundp 'cua-copy-region-to-global-mark)
           (cua--global-mark-active))
      (cua-copy-region-to-global-mark beg end))
     ((eq type 'block)
      (evil-yank-rectangle beg end register yank-handler))
     ((memq type '(line screen-line))
      (evil-yank-lines beg end register yank-handler))
     (t
      (evil-yank-characters beg end register yank-handler)))
    (evil-hybrid-state)
    (vterm-send-string "a")
    (vterm-send-backspace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zsh-vterm-get-current-line ()
  (let* ((start (line-beginning-position))
        (end (point))
        (str (buffer-substring-no-properties start end)))
    (replace-regexp-in-string zsh-vterm-prompt-regexp "" str)))

(defun zsh-vterm-accept-copilot-or-send-tab-to-term ()
  (interactive)
  (if copilot--overlay
      (copilot-accept-completion)
    (vterm-send-tab)))

(defun zsh-vterm-accept-copilot-or-send-shift-tab-to-term ()
  (interactive)
  (if copilot--overlay
      (copilot-accept-completion)
    (vterm--self-insert)))

(defvar zsh-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vterm-mode-map)
    (define-key map (kbd "<backtab>") #'zsh-vterm-accept-copilot-or-send-shift-tab-to-term)
    (define-key map (kbd "<tab>") #'zsh-vterm-accept-copilot-or-send-tab-to-term)
    (define-key map (kbd "s-C") #'zsh-vterm-previous-cli)
    (define-key map (kbd "s-V") #'zsh-vterm-next-cli)

    (evil-define-key 'visual map (kbd "<return>") #'evil-yank-for-zsh-vterm)

    (evil-define-key 'hybrid map (kbd "M-1") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "7")))
    (evil-define-key 'hybrid map (kbd "M-2") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "8")))
    (evil-define-key 'hybrid map (kbd "M-3") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "9")))
    (evil-define-key 'hybrid map (kbd "M-4") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "0")))
    (evil-define-key 'hybrid map (kbd "s-z") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "z")))

    (evil-define-key 'hybrid map (kbd "C-M-s-!") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "1")))
    (evil-define-key 'hybrid map (kbd "C-M-s-@") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "2")))
    (evil-define-key 'hybrid map (kbd "C-M-s-#") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "3")))
    (evil-define-key 'hybrid map (kbd "C-M-s-$") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "4")))
    (evil-define-key 'hybrid map (kbd "C-M-s-%") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "5")))
    (evil-define-key 'hybrid map (kbd "C-M-s-^") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "6")))
    (evil-define-key 'hybrid map (kbd "C-M-s-|") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-C-j)))

    (evil-define-key 'hybrid map (kbd "s-]") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string ">")))
    (evil-define-key 'hybrid map (kbd "s-[") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "<")))

    (define-key map (kbd "<s-S-return>") #'(lambda () (interactive) (if (window-parent) (spacemacs/toggle-maximize-buffer) (vterm-send-C-j) (vterm-send-string "z"))))

    map))

(define-derived-mode zsh-vterm-mode vterm-mode "zsh"
  "Major mode for zsh vterm buffer.")

(provide 'zsh-vterm)
