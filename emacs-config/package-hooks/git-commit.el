;; -*- lexical-binding: t; -*-

;; `gpt-commit' is vendored (vendor/gpt-commit.el) and its commands are
;; autoloaded from vendor/init.el. Do NOT `require' it from this hook: this file
;; is loaded in `dotspacemacs/user-init' (before packages are installed), so on
;; a magit reinstall the hook fires during the package phase. Eagerly loading
;; gpt-commit there also pulls in its deps (magit, request) before they are on
;; `load-path', aborting magit activation with "Cannot open load file ...".
;; Binding the autoloaded command symbols is enough; gpt-commit (and request)
;; load lazily on first use, after `dotspacemacs/user-config' has run.
(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "C-c C-j") 'lx/git-commit-to-org-journal))

(with-eval-after-load 'git-commit
  (define-key git-commit-mode-map (kbd "C-c C-m") 'gpt-commit-message)
  (define-key git-commit-mode-map (kbd "C-c m") 'gpt-commit-message-in-zh)
  (define-key git-commit-mode-map (kbd "C-c C-g") 'gpt-commit-message))

(with-eval-after-load 'magit-log
  (define-key magit-log-mode-map (kbd "C-c C-j") 'lx/git-commit-to-org-journal))
