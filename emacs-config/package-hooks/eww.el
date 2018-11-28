(with-eval-after-load 'eww
  (define-key eww-link-keymap (kbd "C-i") 'eww-forward-url)
  (evil-define-key 'evilified eww-mode-map (kbd "C-o") 'eww-back-url)
  (evil-define-key 'evilified eww-mode-map (kbd "C-i") 'eww-forward-url))
