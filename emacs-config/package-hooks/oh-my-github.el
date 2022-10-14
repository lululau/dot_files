(with-eval-after-load 'oh-my-github

  (oh-my-github-setup)

  (define-key oh-my-github-repos-mode-map (kbd "b") 'oh-my-github-browse-repo)
  (define-key oh-my-github-repos-mode-map (kbd "RET") 'oh-my-github-browse-repo)
  (define-key oh-my-github-repos-mode-map (kbd "w") 'oh-my-github-copy-repo-url)
  (define-key oh-my-github-repos-mode-map (kbd "s") 'oh-my-github-query-repos)
  (define-key oh-my-github-repos-mode-map (kbd "c") 'oh-my-github-query-commits)
  (define-key oh-my-github-repos-mode-map (kbd "r") 'oh-my-github-query-releases)
  (define-key oh-my-github-repos-mode-map (kbd "gr") 'tabulated-list-revert)

  (evilified-state-evilify-map oh-my-github-repos-mode-map :mode oh-my-github-repos-mode :bindings
    "u" 'oh-my-github-sync
    "RET" 'oh-my-github-browse-repo
    "w" 'oh-my-github-copy-repo-url
    "s" 'oh-my-github-query-repos
    "c" 'oh-my-github-query-commits
    "r" 'oh-my-github-query-releases
    "gr" 'tabulated-list-revert)

  (evilified-state-evilify-map oh-my-github-gists-mode-map :mode oh-my-github-gists-mode :bindings
    "u" 'oh-my-github-sync
    "b" 'oh-my-github-browse-gist
    "x" 'oh-my-github-delete-gist
    "d" 'oh-my-github-download-gist-file
    "w" 'oh-my-github-copy-gist-file-url
    "RET" 'oh-my-github-browse-gist-file
    "gr" 'tabulated-list-revert)

  (evilified-state-evilify-map oh-my-github-assets-mode-map :mode oh-my-github-assets-mode :bindings
    "w" 'oh-my-github-copy-asset-url
    "RET" 'oh-my-github-download-asset)

  (evilified-state-evilify-map oh-my-github-commits-mode-map :mode oh-my-github-commits-mode :bindings
    "RET" 'oh-my-github-browse-commit
    "w" 'oh-my-github-copy-commit-url)

  (evilified-state-evilify-map oh-my-github-releases-mode-map :mode oh-my-github-releases-mode :bindings
    "b" 'oh-my-github-browse-release
    "w" 'oh-my-github-copy-release-url
    "RET" 'oh-my-github-query-assets)

  (evilified-state-evilify-map oh-my-github-trendings-mode-map :mode-mode oh-my-github-trending-repos-mode :bindings
    "RET" 'oh-my-github-browse-repo
    "s" 'oh-my-github-trending-repos-query
    "i" 'oh-my-github-trending-repos-info)

  (advice-add 'oh-my-github-sync :after #'(lambda (&rest args)  (popwin:popup-buffer-tail oh-my-github--log-buf-name))))
