(spacemacs|use-package-add-hook helm
  :post-config
  (define-key helm-map (kbd "s-m") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "s-l") 'ace-jump-helm-line)
  (define-key helm-map (kbd "s-j") #'(lambda () (interactive) (helm-next-line 5)))
  (define-key helm-map (kbd "s-k") #'(lambda () (interactive) (helm-previous-line 5))))
