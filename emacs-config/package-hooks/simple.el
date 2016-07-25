(with-eval-after-load 'simple
  (define-key
    process-menu-mode-map
    (kbd "D")
    #'(lambda ()
        (interactive)
        (let ((process (get-text-property (point) 'tabulated-list-id)))
          (cond ((and process
                      (processp process))
                 (delete-process process)
                 (revert-buffer))
                (t
                 (error "no process at point!")))))))
