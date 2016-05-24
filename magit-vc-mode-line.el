(with-eval-after-load 'magit

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

  (advice-add 'magit-checkout :after #'(lambda (&rest args) (run-hooks 'magit-post-checkout-hooks)))
  (add-hook 'magit-post-checkout-hooks 'magit-refresh-projectile-buffers-vc-mode-line)
  (global-set-key (kbd "s-G") 'magit-refresh-projectile-buffers-vc-mode-line))
