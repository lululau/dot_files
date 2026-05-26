;;;###autoload
(defun lx/magit-status-smart (arg)
  "Call magit-status. With prefix arg, switch to existing magit buffer if any"
  (interactive "P")
  (if arg
      (progn
        (require 'magit-mode)
        (let ((magit-buf (--find (s-starts-with? "magit:" (buffer-name it)) (magit-mode-get-buffers))))
          (if magit-buf (switch-to-buffer magit-buf) (magit-status))))
    (cond
     ;; Matches `magit-status' interactive logic: `(not (magit-toplevel))' means out of repo.
     ((and (require 'magit nil t) (magit-toplevel))
      (magit-status))
     ((bound-and-true-p main-git-directory)
      (magit-status main-git-directory))
     (t
      (magit-status)))))

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
(defun lx/magit-merge-interactive ()
  "Call magit merge interactively"
  (interactive)
  (call-interactively 'magit-merge))
