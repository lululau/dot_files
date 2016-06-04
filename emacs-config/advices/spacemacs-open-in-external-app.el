(advice-add
 'spacemacs/open-in-external-app :override
 #'(lambda ()
     (interactive)
     (let ((file-path (if (eq major-mode 'dired-mode)
                          (dired-get-file-for-visit)
                        (buffer-file-name))))
       (if file-path
           (cond
            ((spacemacs/system-is-mswindows) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
            ((spacemacs/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
            ((spacemacs/system-is-linux) (let ((process-connection-type nil))
                                           (start-process "" nil "xdg-open" file-path))))
         (if (or (eq 'mu4e-headers-mode major-mode) (eq 'mu4e-view-mode major-mode) (eq 'mu4e-org-mode major-mode))
             (open-message-with-mail-app)
           (message "No file associated to this buffer."))))))
