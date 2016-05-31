(spacemacs/set-leader-keys
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
  "gho" #'browse-at-remote/browse
  "to" #'org-toggle-link-display
)
(spacemacs/set-leader-keys
  "0" 'spacemacs/persp-switch-to-0
  "1" 'spacemacs/persp-switch-to-1
  "2" 'spacemacs/persp-switch-to-2
  "3" 'spacemacs/persp-switch-to-3
  "4" 'spacemacs/persp-switch-to-4
  "5" 'spacemacs/persp-switch-to-5
  "6" 'spacemacs/persp-switch-to-6
  "7" 'spacemacs/persp-switch-to-7
  "8" 'spacemacs/persp-switch-to-8
  "9" 'spacemacs/persp-switch-to-9)
