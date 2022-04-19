(with-eval-after-load 'evil-core
  (with-eval-after-load 'info
    (evil-define-key 'motion Info-mode-map
      (kbd "n") #'Info-next
      (kbd "C-n") #'evil-search-next
      (kbd "O") #'Info-toc
      (kbd "<return>") #'Info-follow-nearest-node
      (kbd "RET") #'Info-follow-nearest-node
      (kbd "g") nil
      (kbd "gg") #'evil-goto-first-line
      (kbd "G") #'evil-goto-line)))
