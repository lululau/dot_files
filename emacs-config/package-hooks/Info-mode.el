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
      (kbd "G") #'evil-goto-line
      "1" #'Info-nth-menu-item
      "2" #'Info-nth-menu-item
      "3" #'Info-nth-menu-item
      "4" #'Info-nth-menu-item
      "5" #'Info-nth-menu-item
      "6" #'Info-nth-menu-item
      "7" #'Info-nth-menu-item
      "8" #'Info-nth-menu-item
      "9" #'Info-nth-menu-item)))
