;;;###autoload
(defun lx/autojump-add ()
  (if (eq 'dired-mode major-mode)
      (shell-command-to-string (concat "autojump --add " (shell-quote-argument (dired-current-directory))))
    (if (buffer-file-name)
        (shell-command-to-string (concat "autojump --add " (shell-quote-argument (file-name-directory (buffer-file-name))))))))
