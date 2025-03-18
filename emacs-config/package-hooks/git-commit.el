(with-eval-after-load 'git-commit
  (require 'gpt-commit)
  (define-key git-commit-mode-map (kbd "C-c m") 'gpt-commit-message)
  (define-key git-commit-mode-map (kbd "C-c C-g") 'gpt-commit-message))
