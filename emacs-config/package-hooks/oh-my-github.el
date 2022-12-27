(with-eval-after-load 'omg

  (omg-setup)

  (define-key omg-repo-mode-map (kbd "b") 'omg-repo-browse)
  (define-key omg-repo-mode-map (kbd "RET") 'omg-repo-browse)
  (define-key omg-repo-mode-map (kbd "w") 'omg-repo-copy-url)
  (define-key omg-repo-mode-map (kbd "s") 'omg-repo-query-repos)
  (define-key omg-repo-mode-map (kbd "c") 'omg-repo-query-commits)
  (define-key omg-repo-mode-map (kbd "r") 'omg-repo-query-releases)
  (define-key omg-repo-mode-map (kbd "gr") 'tabulated-list-revert)

  (evilified-state-evilify-map omg-repo-mode-map :mode omg-repo-mode :bindings
    "u" 'omg-sync
    "RET" 'omg-repo-browse
    "w" 'omg-repo-copy-url
    "s" 'omg-repo-query-repos
    "c" 'omg-repo-query-commits
    "r" 'omg-repo-query-releases
    "gr" 'tabulated-list-revert)

  (evilified-state-evilify-map omg-gist-mode-map :mode omg-gist-mode :bindings
    "u" 'omg-sync
    "b" 'omg-gist-browse
    "x" 'omg-gist-delete
    "d" 'omg-gist-download
    "w" 'omg-gist-copy-gist-url
    "RET" 'omg-gist-browse-file
    "gr" 'tabulated-list-revert)

  (evilified-state-evilify-map omg-release-asset-mode-map :mode omg-release-asset-mode :bindings
    "w" 'omg-release-copy-asset-url
    "RET" 'omg-release-download-asset)

  (evilified-state-evilify-map omg-commit-mode-map :mode omg-commit-mode :bindings
    "RET" 'omg-commit-browse
    "w" 'omg-commit-copy-url)

  (evilified-state-evilify-map omg-release-mode-map :mode omg-release-mode :bindings
    "b" 'omg-release-browse
    "w" 'omg-release-copy-url
    "RET" 'omg-query-assets)

  (evilified-state-evilify-map omg-trending-mode-map :mode-mode omg-trending-repos-mode :bindings
    "RET" 'omg-repo-browse
    "s" 'omg-trending-query)

  (advice-add 'omg-sync :after #'(lambda (&rest args)  (popwin:popup-buffer-tail omg--log-buf-name))))
