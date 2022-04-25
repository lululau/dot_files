(spacemacs|use-package-add-hook company
  :post-config
  (define-key company-active-map (kbd "C-r") 'helm-company)
  (push 'company-robe company-backends)
  ;; (add-to-list 'company-backends #'company-tabnine)
  (spacemacs|add-company-backends
    :backends company-indirect-sql-backend
    :modes sql-mode)
  (define-key company-mode-map (kbd "<backtab>") 'copilot-accept-completion)
  (define-key company-mode-map (kbd "M-f") 'lx/copilot-accept-or-forward-word)
  (define-key company-mode-map (kbd "C-n") 'lx/copilot-accept-or-next-line)
  (define-key company-active-map (kbd "C-l") 'yas/expand)
  (define-key company-active-map (kbd "C-y") '(lambda () (interactive) (company-abort) (call-interactively 'company-yasnippet)))
  (add-hook 'company-after-completion-hook '(lambda (&optional args) (copilot-clear-overlay)))

  )
