(with-eval-after-load 'magit-status
  (require 'gpt-commit)
  (define-key git-commit-mode-map (kbd "C-c C-m") 'gpt-commit-message)
  (define-key git-commit-mode-map (kbd "C-c C-g") 'gpt-commit-message))
