(with-eval-after-load 'magit-gh-pulls
  (defun magit-gh-pulls-build-req (api user proj callback)
    "Builds a request entity for a new pull request. Under
   synchronous flow (editor disabled), fires CALLBACK with
   API, USER, PROJ and the new REQUEST as args. Under
   asynchronous flow, passes all ARGS through to the PR
   editor which is responsible for continuing the flow."
    (let* ((current (magit-get-current-branch))
           (current-default (magit-gh-pulls-get-remote-default))
           (base-branch (magit-read-branch-or-commit "Base" current-default))
           (head-branch (magit-read-branch-or-commit "Head" current))
           (head-user (replace-regexp-in-string "^.*[:/]\\([^/]*\\)/[^/]*\\.git" "\\1" (magit-get "remote" (magit-get-remote head-branch) "url"))))
      (let* ((head-remote (concat (magit-get-remote head-branch) "/" head-branch))
             (pushed-p (and (magit-branch-p head-remote)
                            (null (magit-git-lines "diff" (concat head-remote ".." head-branch))))))
        (when (and (not pushed-p)
                   (yes-or-no-p "PR branch doesn't appear to be pushed. Push it?"))
          (magit-push current (magit-get-remote base-branch))))
      (let* ((base
              (make-instance 'gh-repos-ref :user (make-instance 'gh-users-user :name user)
                             :repo (make-instance 'gh-repos-repo :name proj)
                             :ref base-branch))
             (head
              (make-instance 'gh-repos-ref :user (make-instance 'gh-users-user :name head-user)
                             :repo (make-instance 'gh-repos-repo :name proj)
                             :ref (format "%s%s" (if (string= user head-user) "" (format "%s:" head-user)) head-branch)))
             (default-title (magit-git-string "log"
                                              (format "%s..%s" base-branch head-branch)
                                              "--format=%s" "--reverse"))
             (default-body (mapconcat 'identity (magit-git-items "log"
                                                                 (format "%s..%s" base-branch head-branch)
                                                                 "--reverse" "--format=**%s**%n%b") "\n")))

        (if (member "--use-pr-editor" (magit-gh-pulls-arguments))
            (magit-gh-pulls-init-pull-editor api user proj default-title default-body base head callback)
          (let* ((title (read-string "Title: " default-title))
                 (body (read-string "Description: " default-body))
                 (req (make-instance 'gh-pulls-request :head head :base base :body body :title title)))
            (funcall callback api user proj req))))))
  (defun magit-gh-pulls-url-for-pull (info)
    "Return github url for a pull request using INFO."
    (let* ((domain
            (cdr (assoc
                  (car (--find (string-match-p (plist-get (cdr it) :remote-regexp) (magit-get "remote" (magit-get-remote) "url")) gh-profile-alist))
                  '(("github" . "https://www.github.com") ("kaitong" . "https://github.ktjr.com")))))
           (url (concat domain "/%s/%s/pull/%s")))
      (apply 'format url info)))
  (add-hook 'magit-gh-pulls-mode-hook 'lx/set-gh-profile-current-profile))
