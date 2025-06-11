;; -*- lexical-binding: t; -*-

(with-eval-after-load 'magit-status
  (require 'gpt-commit)
  (define-key magit-status-mode-map (kbd "C-c C-j") 'lx/git-commit-to-org-journal)
  (define-key git-commit-mode-map (kbd "C-c C-m") 'gpt-commit-message)
  (define-key git-commit-mode-map (kbd "C-c C-g") 'gpt-commit-message))

(with-eval-after-load 'magit-log
  (define-key magit-log-mode-map (kbd "C-c C-j") 'lx/git-commit-to-org-journal))
