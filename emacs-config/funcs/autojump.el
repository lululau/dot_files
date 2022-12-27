(defun lx/autojump-add ()
  (if (eq 'dired-mode major-mode)
      (shell-command-to-string (concat "autojump --add " (shell-quote-argument (dired-current-directory))))
    (if (buffer-file-name)
        (shell-command-to-string (concat "autojump --add " (shell-quote-argument (file-name-directory (buffer-file-name))))))))

(add-hook 'find-file-hook 'lx/autojump-add)
(add-hook 'dired-after-readin-hook 'lx/autojump-add)
