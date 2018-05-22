;;;###autoload
(defun lx/browse-in-alfred (path)
  (shell-command-to-string (format "osascript -e 'tell app \"Alfred 3\" to browse \"%s\"'" path)))

;;;###autoload
(defun lx/browse-file-or-directory-in-alfred (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (lx/browse-in-alfred (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (lx/browse-in-alfred file-path)
        (message "No file associated to this buffer.")))))
