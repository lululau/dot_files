(spacemacs|use-package-add-hook company
  :post-config
  (define-key company-active-map (kbd "C-r") 'helm-company)
  (push 'company-robe company-backends)
  (spacemacs|add-company-backends
    :backends company-indirect-sql-backend
    :modes sql-mode))
