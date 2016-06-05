(advice-add 'spacemacs//helm-hjkl-navigation :after (lambda (style) (define-key helm-map (kbd "C-k") nil)))
