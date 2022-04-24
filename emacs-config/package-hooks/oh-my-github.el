(with-eval-after-load 'oh-my-github

  (oh-my-github-setup)

  (evilified-state-evilify oh-my-github-stars-mode oh-my-github-stars-mode-map
    "u" 'oh-my-github-sync
    "RET" 'oh-my-github-browse-repo
    "w" 'oh-my-github-copy-repo-url
    "s" 'oh-my-github-query-repos
    "c" 'oh-my-github-query-commits
    "r" 'oh-my-github-query-releases
    "gr" 'tabulated-list-revert)

  (evilified-state-evilify oh-my-github-repos-mode oh-my-github-repos-mode-map
    "u" 'oh-my-github-sync
    "RET" 'oh-my-github-browse-repo
    "w" 'oh-my-github-copy-repo-url
    "s" 'oh-my-github-query-repos
    "c" 'oh-my-github-query-commits
    "r" 'oh-my-github-query-releases
    "gr" 'tabulated-list-revert)

  (evilified-state-evilify oh-my-github-assets-mode oh-my-github-assets-mode-map
    "w" 'oh-my-github-copy-asset-url
    "RET" 'oh-my-github-download-asset)

  (evilified-state-evilify oh-my-github-commits-mode oh-my-github-commits-mode-map
    "RET" 'oh-my-github-browse-commit
    "w" 'oh-my-github-copy-commit-url)

  (evilified-state-evilify oh-my-github-release-mode oh-my-github-release-mode-map
    "b" 'oh-my-github-browse-release
    "w" 'oh-my-github-copy-release-url
    "RET" 'oh-my-github-query-assets))
