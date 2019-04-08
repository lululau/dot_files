(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "C-n") #'evil-search-next)
  (define-key Info-mode-map (kbd "g") nil)
  (define-key Info-mode-map (kbd "gg") #'evil-goto-first-line)
  (define-key Info-mode-map (kbd "G") #'evil-goto-line))
