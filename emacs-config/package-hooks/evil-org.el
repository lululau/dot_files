(spacemacs|use-package-add-hook evil-org
  :post-config
  (progn
    (mapc (lambda (state)
            (evil-define-key state evil-org-mode-map
              (kbd "M-l") nil
              (kbd "M-h") nil
              (kbd "M-k") nil
              (kbd "M-j") nil
              (kbd "M-L") nil
              (kbd "M-H") nil
              (kbd "M-K") nil
              (kbd "M-J") nil
              ))
          '(normal insert))
    (define-key org-mode-map (kbd "M-h") nil)
    (unless (display-graphic-p)
      (evil-define-key 'normal evil-org-mode-map
        (kbd "C-RET") (evil-org-define-eol-command
                       org-insert-heading-respect-content)
        (kbd "M-S-RET") (evil-org-define-eol-command
                       org-insert-todo-heading)))
    ))
