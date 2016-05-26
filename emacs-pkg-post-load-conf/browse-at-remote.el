(spacemacs|use-package-add-hook browse-at-remote
  :post-config
  (defun browse-at-remote/parse-git-prefixed (remote-url)
    "Extract domain and slug from REMOTE-URL like git@..."
    (cdr (s-match "git@\\([a-z.-]+\\):\\([a-z0-9_.-]+/[a-z0-9_.-]+?\\)\\(?:\.git\\)?$" remote-url)))

  (defun browse-at-remote/get-url-from-remote (remote-url)
    "Return (DOMAIN . URL) from REMOTE-URL."
    (let* ((parsed
            (cond
             ((s-starts-with? "git" remote-url) (browse-at-remote/parse-git-prefixed remote-url))
             ((s-starts-with? "http" remote-url) (browse-at-remote/parse-https-prefixed remote-url))))
           (proto
            (if (s-starts-with? "https:" remote-url) "https" "http"))
           (domain (car parsed))
           (slug (nth 1 parsed)))
      (cons domain (format "%s://%s/%s" proto domain slug)))))
