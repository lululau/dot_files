(with-eval-after-load 'term
  (define-key term-raw-map
    (kbd "<S-return>") #'(lambda ()
                           (interactive)
                           (shell-pop--cd-to-cwd
                            (with-current-buffer (get-buffer shell-pop-last-buffer) (projectile-project-root)))))

  (define-key term-raw-map
    (kbd "<s-return>") #'(lambda ()
                           (interactive)
                           (shell-pop--cd-to-cwd
                            (file-name-directory
                             (buffer-file-name (get-buffer shell-pop-last-buffer)))))))
