(defun lx/switch-to-layout-of-project (project)
  (spacemacs/layout-switch-by-pos (or (--find-index (string= project (expand-file-name it)) (persp-names-current-frame-fast-ordered)) 0)))

(defun lx/open-with-idea()
  (interactive)
  (shell-command-to-string (format "/usr/local/bin/idea \"%s\":%s" (buffer-file-name) (line-number-at-pos))))
