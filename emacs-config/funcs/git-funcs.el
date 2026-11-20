;;; -*- lexical-binding: t -*-

(defvar magit--refresh-cache)

;;;###autoload
(defun lx/magit-status-smart (arg)
  "Call magit-status. With prefix arg, switch to existing magit buffer if any"
  (interactive "P")
  (if arg
      (progn
        (require 'magit-mode)
        (let ((magit-buf (--find (s-starts-with? "magit:" (buffer-name it)) (magit-mode-get-buffers))))
          (if magit-buf (switch-to-buffer magit-buf) (magit-status))))
    (let ((magit--refresh-cache (list (cons 0 0))))
      (cond
       ;; Matches `magit-status' interactive logic: `(not (magit-toplevel))' means out of repo.
       ((and (require 'magit nil t) (magit-toplevel))
        (magit-status nil magit--refresh-cache))
       ((bound-and-true-p main-git-directory)
        (magit-status main-git-directory magit--refresh-cache))
       (t
        (magit-status nil magit--refresh-cache))))))

;;;###autoload
(defun lx/magit-smart-checkout (arg)
  "Checkout branch. With prefix arg, create and checkout"
  (interactive "P")
  (call-interactively (if arg 'magit-branch-and-checkout 'magit-checkout)))

;;;###autoload
(defun lx/magit-status-async-and-show ()
  "Run git status async and show process buffer"
  (interactive)
  (magit-run-git-async "status")
  (magit-process-buffer))

;;;###autoload
(defun lx/magit-file-undo-checkout ()
  "Checkout current file from current branch"
  (interactive)
  (magit-file-checkout (magit-get-current-branch) (buffer-file-name)))

;;;###autoload
(defun lx/magit-pull-and-show ()
  "Pull from upstream and show process buffer"
  (interactive)
  (call-interactively 'magit-pull-from-upstream)
  (magit-process-buffer))

;;;###autoload
(defun lx/magit-push-and-show ()
  "Push to upstream and show process buffer"
  (interactive)
  (call-interactively 'magit-push-current-to-upstream)
  (magit-process-buffer))

;;;###autoload
(defun lx/git-default-commit-message ()
  "Return default commit message from git config `lx.defaultCommitMessage'.
Fall back to \"A commit\" when unset or empty."
  (let ((msg (magit-get "lx.defaultCommitMessage")))
    (if (and msg (not (string-empty-p msg)))
        msg
      "A commit")))

;;;###autoload
(defun lx/git-stage-commit-push ()
  "Stage all changes, commit with a default message, then push asynchronously.
The commit message comes from git config `lx.defaultCommitMessage'
(per-repo or global); if unset, uses \"A commit\".
Show the magit process buffer in another window so the push
output and result can be watched."
  (interactive)
  (require 'magit)
  (magit-run-git "add" "-A")
  (magit-run-git "commit" "-m" (lx/git-default-commit-message))
  (magit-run-git-async "push")
  (switch-to-buffer-other-window (magit-process-buffer t)))

;;;###autoload
(defun lx/magit-merge-interactive ()
  "Call magit merge interactively"
  (interactive)
  (call-interactively 'magit-merge))

(defun lx/magit--read-merge-into-args ()
  "Prompt for target branch to merge current HEAD into, plus `magit-merge' args."
  (let ((branch (magit-get-current-branch)))
    (list (magit-read-other-local-branch
           (format "Merge `%s' into" (or branch (magit-rev-parse "HEAD")))
           nil
           (and branch (magit-get-local-upstream-branch branch)))
          (magit-merge-arguments))))

;;;###autoload
(defun lx/magit-merge-into (branch &optional args)
  "Merge current branch/HEAD into BRANCH without switching away or deleting it."
  (interactive (lx/magit--read-merge-into-args))
  (let ((source (or (magit-get-current-branch)
                    (magit-rev-parse "HEAD")))
        (repo (magit-toplevel)))
    (if (and (not (member "--no-ff" args))
             (magit-rev-ancestor-p branch source))
        ;; Fast-forward in place without touching the working tree.
        (magit-run-git "fetch" "." (format "HEAD:%s" branch))
      (magit-merge-assert)
      (when (zerop (magit-call-git "checkout" branch))
        (magit-run-git-async "merge" args "--no-edit" source)
        (set-process-sentinel
         magit-this-process
         (lambda (process event)
           (when (memq (process-status process) '(exit signal))
             (if (> (process-exit-status process) 0)
                 (progn
                   (magit-process-sentinel process event)
                   (message "Merge into `%s' did not complete cleanly; staying on `%s' (source `%s' preserved)."
                            branch branch source))
               (process-put process 'inhibit-refresh t)
               (magit-process-sentinel process event)
               (let ((default-directory repo))
                 (magit-run-git "checkout" source))))))))))

;;;###autoload
(defun lx/magit-merge-into-and-checkout (branch &optional args)
  "Merge current branch/HEAD into BRANCH and switch to BRANCH, keeping original."
  (interactive (lx/magit--read-merge-into-args))
  (let ((source (or (magit-get-current-branch)
                    (magit-rev-parse "HEAD"))))
    (magit-merge-assert)
    (when (zerop (magit-call-git "checkout" branch))
      (magit-run-git-async "merge" args "--no-edit" source))))
