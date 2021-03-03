(with-eval-after-load 'evil-collection-dired
  (evil-collection-define-key 'motion 'dired-mode-map
    (kbd "TAB") 'dired-subtree-toggle
    (kbd "<backtab>") 'dired-subtree-cycle)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "TAB") 'dired-subtree-toggle
    (kbd "<backtab>") 'dired-subtree-cycle))
