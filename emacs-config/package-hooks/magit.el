(spacemacs|use-package-add-hook magit-status
  :post-config
  (defun magit-refresh-vc-mode-line (rev)
    "Update the information displayed by `vc-mode' in the mode-line.
Like `vc-mode-line' but simpler, more efficient, and less buggy."
    (setq vc-mode
          (if vc-display-status
              (magit-with-toplevel
               (let* ((msg (cl-letf (((symbol-function #'vc-working-revision)
                                      (lambda (&rest _) rev)))
                             (vc-default-mode-line-string
                              'Git buffer-file-name))))
                 (propertize
                  (concat " " msg)
                  'mouse-face 'mode-line-highlight
                  'help-echo (concat (get-text-property 0 'help-echo msg)
                                     "\nCurrent revision: " rev
                                     "\nmouse-1: Version Control menu")
                  'local-map vc-mode-line-map)))
            " Git"))
    (force-mode-line-update))

  (defun magit-refresh-projectile-buffers-vc-mode-line ()
    (interactive)
    (let ((rev (or (magit-get-current-branch)
                   (magit-rev-parse "--short" "HEAD"))))
      (dolist (buffer (projectile-project-buffers))
        (when (buffer-file-name buffer)
          (with-current-buffer buffer
            (magit-refresh-vc-mode-line rev))
          ))))
  (global-set-key (kbd "s-G") 'magit-refresh-projectile-buffers-vc-mode-line))

(spacemacs|use-package-add-hook magit-branch
  (advice-add 'magit-checkout :after #'(lambda (&rest args) (run-hooks 'magit-post-checkout-hooks)))
  (add-hook 'magit-post-checkout-hooks 'magit-refresh-projectile-buffers-vc-mode-line))

(with-eval-after-load 'magit-mode
  (define-key magit-mode-map (kbd "S-SPC") nil)
  (define-key magit-mode-map [S-tab] 'magit-section-cycle-global)
  (define-key magit-mode-map [remap org-store-link] 'orgit-store-link))

(with-eval-after-load 'magit-commit
  (defun lx/git-commit-get-message ()
    (interactive)
    (replace-regexp-in-string "^#[^\n]*\n" ""
                              (or (buffer-substring-no-properties (point-min) (point-max)) "")))

  (defun lx/git-commit-get-diff ()
    (interactive)
    (let* ((diff-buffer (magit-get-mode-buffer 'magit-diff-mode))

           (diff (if diff-buffer
                     (with-current-buffer diff-buffer
                       (buffer-substring-no-properties (point-min) (point-max)))
                   "")))
      (format "%s\n\nGit commit message:\n\n" diff)))

  (defun lx/git-commit-get-diff-lines ()
    (interactive)
    (s-lines (s-chomp (lx/git-commit-get-diff))))


  (defun lx/git-commit-get-diff-line-nums ()
    (interactive)
    (s-count-matches "\n" (lx/git-commit-get-diff)))

  (defun lx/git-commit-get-doc ()
    (interactive)
    (s-lines (concat (lx/git-commit-get-diff)
            (lx/git-commit-get-message)))))
