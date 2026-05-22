(require 'proced)

;;;###autoload
(defun lx/proced-same-window (&optional arg)
  "Show proced in the current window, ignoring window-purpose."
  (interactive "P")
  (unless proced-available
    (error "Proced is not available on this system"))
  (without-purpose
    (let ((buffer (get-buffer-create "*Proced*")))
      (with-current-buffer buffer
        (when (and (file-remote-p default-directory)
                   (not (or proced-show-remote-processes (eq arg '-))))
          (setq default-directory temporary-file-directory))
        (when (zerop (buffer-size))
          (proced-mode))
        (if arg
            (progn
              (display-buffer buffer)
              (proced-update t))
          (pop-to-buffer-same-window buffer)
          (proced-update t)
          (message
           (substitute-command-keys
            "Type \\<proced-mode-map>\\[quit-window] to quit, \\[proced-help] for help")))))))
