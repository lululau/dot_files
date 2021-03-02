(with-eval-after-load 'evil-collection-dired
  (with-eval-after-load 'dired-subtree
    (evil-collection-define-key 'normal 'dired-mode-map
      (kbd "TAB") 'dired-subtree-toggle
      (kbd "<backtab>") 'dired-subtree-cycle)))
