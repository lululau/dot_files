(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map (kbd "RET") #'(lambda () (interactive) (condition-case nil (call-interactively 'org-open-at-point) (error (evil-insert-newline-below))))))
