(with-eval-after-load 'helm-ls-git
  (evilified-state-evilify-map helm-ls-git-rebase-todo-mode-map
    :mode helm-ls-git-rebase-todo-mode
    :bindings
    (kbd "p")    'git-rebase-pick
    (kbd "r")    'git-rebase-reword
    (kbd "e")    'git-rebase-edit
    (kbd "s")    'git-rebase-squash
    (kbd "f")    'git-rebase-fixup
    (kbd "x")    'git-rebase-exec
    (kbd "d")    'git-rebase-kill-line
    (kbd "u")    'git-rebase-undo
    (kbd "k")    'evil-previous-line
    (kbd "j")    'evil-next-line
    (kbd "M-k")  'git-rebase-move-line-up
    (kbd "M-j")  'git-rebase-move-line-down))
