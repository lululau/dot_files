(require 'vterm)

(defvar zsh-vterm-last-buffer nil)

(defvar zsh-vterm-prompt-regexp "^.*\\(❯\\|\\]#\\|\\]\\$\\|➜\\) ")

(defvar zsh-vterm-prompt-has-previous-regexp "^.*\\(❯\\|➜\\) ")

(defun lx/run-in-zsh-vterm (command buffer-name &optional directory window-type)
  (interactive)
  (let* ((buffer (get-buffer buffer-name))
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
            ('split (pop-to-buffer buffer 'display-buffer-pop-up-window))
            ('popup (select-window (shell-pop-split-window)) (switch-to-buffer buffer))
            (_ (switch-to-buffer buffer))))

      (let* ((default-directory (or directory user-home-directory))
             (vterm-shell command))
        (pcase window-type
          ('split (split-window-right-and-focus))
          ('popup (select-window (shell-pop-split-window))))

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

(defun zsh-vterm-goto-tmp-dir ()
  (interactive)
  (let* ((project-root (projectile-project-root))
         (tmp-dir (concat project-root "tmp")))
    (if project-root
        (if (file-exists-p tmp-dir)
              (vterm-send-string (concat "cd " tmp-dir "\n"))
          (message "No tmp directory found"))
      (vterm-send-string "cd ~/tmp\n"))))

(defvar zsh-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vterm-mode-map)
    (define-key map (kbd "<backtab>") #'zsh-vterm-accept-copilot-or-send-shift-tab-to-term)
    (define-key map (kbd "<tab>") #'zsh-vterm-accept-copilot-or-send-tab-to-term)
    (define-key map (kbd "s-C") #'zsh-vterm-previous-cli)
    (define-key map (kbd "s-V") #'zsh-vterm-next-cli)
    (define-key map (kbd "s-i s-o") #'zsh-vterm-goto-tmp-dir)

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
