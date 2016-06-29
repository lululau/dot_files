(advice-add 'neotree-find-project-root :override (lambda ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let (has-neo-window)
      (--each (window-list) (when (string= " *NeoTree*" (buffer-name (window-buffer it))) (delete-window it) (setq has-neo-window t)))
      (unless has-neo-window
        (let ((origin-buffer-file-name (buffer-file-name)))
          (neotree-find (projectile-project-root))
          (neotree-find origin-buffer-file-name)))))))

