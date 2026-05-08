(with-eval-after-load 'shell-pop
  (defun shell-pop--cd-to-cwd (cwd)
    "Change the current working directory of the shell buffer to CWD."
    (let ((abspath (expand-file-name cwd)))
      (cond ((string= shell-pop-internal-mode "eshell")
             (shell-pop--cd-to-cwd-eshell abspath))
            ((string= shell-pop-internal-mode "shell")
             (shell-pop--cd-to-cwd-shell abspath))
            ((string-match-p "vterm" shell-pop-internal-mode)
             (shell-pop--cd-to-cwd-vterm abspath))
            ((string= shell-pop-internal-mode "eat")
             (shell-pop--cd-to-cwd-eat abspath))
            (t
             (shell-pop--cd-to-cwd-term abspath))))))
