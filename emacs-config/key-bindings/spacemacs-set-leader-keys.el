(spacemacs|create-align-repeat-x "hash" "#")

(spacemacs/set-leader-keys
  "bk" #'(lambda () (interactive) (call-interactively 'kill-buffer))
  "gC" 'magit-commit
  "gc" 'magit-checkout
  "gS" 'magit-status
  "gs" #'(lambda () (interactive) (magit-run-git-async "status") (magit-process-buffer))
  "gu" #'(lambda () (interactive) (magit-checkout-file (magit-get-current-branch) (buffer-file-name)))
  "gp" #'(lambda () (interactive) (call-interactively 'magit-pull-from-upstream) (magit-process-buffer))
  "gP" #'(lambda () (interactive) (call-interactively 'magit-push-current-to-upstream) (magit-process-buffer))
  "gM" #'(lambda () (interactive) (call-interactively 'magit-merge))
  "aoA" #'(lambda () (interactive) (require 'calfw-org) (cfw:open-org-calendar))
  "aoR" #'(lambda () (interactive) (org-refile '(4)))
  "aob" 'org-iswitchb
  "gho" #'browse-at-remote
  "to" #'org-toggle-link-display
  "s-f" #'(lambda () (interactive) (lx/set-monospaced-font "Monaco" "STHeiti" 12 14 13 16))
  "ps" #'lx/find-or-create-projectile-snippet-file
  "ad" #'(lambda () (interactive) (call-interactively 'dired))
  "tl" #'copy-org-links-at-point
  "xa#" #'spacemacs/align-repeat-hash
)
