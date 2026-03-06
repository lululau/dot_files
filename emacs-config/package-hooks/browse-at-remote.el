;; -*- lexical-binding: t; -*-

(spacemacs|use-package-add-hook browse-at-remote
  :post-config
  (defun browse-at-remote/parse-git-prefixed (remote-url)
    "Extract domain and slug from REMOTE-URL like git@..."
    (cdr (s-match "git@\\([a-z.-]+\\):\\([a-z0-9_.-]+/[a-z0-9_.-]+?\\)\\(?:\.git\\)?$" remote-url)))

  (defun browse-at-remote--get-local-branch ()
    "Return the name of the current local branch name.
  If HEAD is detached, return nil."
    ;; Based on http://stackoverflow.com/a/1593487/509706
    (s-chop-prefix "refs/heads/" (s-trim (vc-git--run-command-string nil "symbolic-ref" "HEAD"))))


  (defun browse-at-remote--get-remote-url (remote)
    "Get URL of REMOTE from current repo."
      (s-replace "\n" "" (vc-git--run-command-string nil "ls-remote" "--get-url" remote)))


  (defun browse-at-remote--get-from-config (key)
    (let* ((value (vc-git--run-command-string nil "config" "--get" key)))
      (when value (s-trim value)))))
