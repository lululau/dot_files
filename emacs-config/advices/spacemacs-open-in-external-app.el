(advice-add 'spacemacs/open-file-or-directory-in-external-app :override #'(lambda (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (spacemacs//open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (spacemacs//open-in-external-app file-path)
        (if (or (eq 'mu4e-headers-mode major-mode) (eq 'mu4e-view-mode major-mode) (eq 'mu4e-org-mode major-mode))
            (open-message-with-mail-app)
          (message "No file associated to this buffer.")))))))
