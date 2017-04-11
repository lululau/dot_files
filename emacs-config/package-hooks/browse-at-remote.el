(spacemacs|use-package-add-hook browse-at-remote
  :post-config
  (defun browse-at-remote/parse-git-prefixed (remote-url)
    "Extract domain and slug from REMOTE-URL like git@..."
    (cdr (s-match "git@\\([a-z.-]+\\):\\([a-z0-9_.-]+/[a-z0-9_.-]+?\\)\\(?:\.git\\)?$" remote-url))))
