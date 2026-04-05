(with-eval-after-load 'agent-shell

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
