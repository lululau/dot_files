(with-eval-after-load 'agent-shell

  (defvar lx/agent-shell--display-in-other-window nil
    "When non-nil, the next `agent-shell--display-buffer' uses another window.")

  (defun lx/agent-shell--display-buffer-in-other-window (shell-buffer)
    "Display SHELL-BUFFER in another window.

When the current frame has only one window, split it once.
When multiple windows exist, reuse another window instead of splitting again."
    (setq lx/agent-shell--display-in-other-window nil)
    (if-let ((window (get-buffer-window shell-buffer (selected-frame))))
        (select-window window)
      (let* ((frame (selected-frame))
             (windows (window-list frame))
             (target (if (= 1 (length windows))
                         (split-window (selected-window) nil 'right)
                       (or (cl-find (lambda (w) (not (eq w (selected-window))))
                                    windows)
                           (split-window (selected-window) nil 'right)))))
        (select-window target)
        (switch-to-buffer shell-buffer))))

  (defun lx/agent-shell--display-buffer-advice (orig shell-buffer)
    (if lx/agent-shell--display-in-other-window
        (lx/agent-shell--display-buffer-in-other-window shell-buffer)
      (funcall orig shell-buffer)))

  (defun lx/agent-shell--emit-event-advice (orig &rest args)
    (when (and lx/agent-shell--display-in-other-window
               (eq (plist-get args :event) 'session-selection-cancelled))
      (setq lx/agent-shell--display-in-other-window nil))
    (apply orig args))

  (unless (advice-member-p 'lx/agent-shell--display-buffer-advice
                           'agent-shell--display-buffer)
    (advice-add 'agent-shell--display-buffer :around #'lx/agent-shell--display-buffer-advice))
  (unless (advice-member-p 'lx/agent-shell--emit-event-advice
                           'agent-shell--emit-event)
    (advice-add 'agent-shell--emit-event :around #'lx/agent-shell--emit-event-advice))

  (defun lx/agent-shell-in-other-window (&optional arg)
    "Start or reuse an agent shell in another window.

Same as `agent-shell', but always displays the shell in another window.
When the current frame has only one window, split it first.

With \\[universal-argument] prefix ARG, force start a new shell.

With \\[universal-argument] \\[universal-argument] prefix ARG, prompt to pick an existing shell."
    (interactive "P")
    (setq lx/agent-shell--display-in-other-window t)
    (condition-case-unless-debug err
        (cond
         ((equal arg '(16))
          (agent-shell--dwim :switch-to-shell t))
         ((equal arg '(4))
          (agent-shell--dwim :new-shell t))
         (t
          (agent-shell--dwim)))
      (quit (setq lx/agent-shell--display-in-other-window nil))))

  (define-key agent-shell-mode-map (kbd "C-v") 'agent-shell-send-clipboard-image)

  (cl-defun lx/agent-shell--buffer-files ()
    "Return buffer file(s) or `dired' selected file(s)."
    (if (buffer-file-name)
        (list (buffer-file-name))
      (or
      (agent-shell--dired-paths-in-region)
      (dired-get-marked-files))))

  (defun agent-shell-send-current-file (&optional prompt-for-file)
    "Insert a file into `agent-shell'.

If visiting a file, send this file.

If invoked from shell, select a project file.

If invoked from `dired', use selection or region files.

With prefix argument PROMPT-FOR-FILE, always prompt for file selection."
    (interactive "P")
    (if (and (region-active-p)
             (buffer-file-name))
        (agent-shell-send-region)
      (let* ((in-shell (derived-mode-p 'agent-shell-mode))
             (files (if (or in-shell prompt-for-file)
                        (list (completing-read "Send file: " (agent-shell--project-files)))
                      (or (lx/agent-shell--buffer-files)
                          (list (completing-read "Send file: " (agent-shell--project-files)))
                          (user-error "No file to send")))))
        (agent-shell--insert-to-shell-buffer
         :text (agent-shell--processed-files :files files)))))
  )
