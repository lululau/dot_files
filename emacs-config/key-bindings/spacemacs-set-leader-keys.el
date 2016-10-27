(spacemacs|create-align-repeat-x "hash" "#")

(spacemacs/set-leader-keys
  "bk" #'(lambda () (interactive) (call-interactively 'kill-buffer))
  "gC" 'magit-commit
  "gc" 'magit-checkout
  "gc" #'(lambda (arg) (interactive "P") (call-interactively (if arg 'magit-branch-and-checkout 'magit-checkout)))
  "gS" 'magit-status
  "gs" #'(lambda () (interactive) (magit-run-git-async "status") (magit-process-buffer))
  "gu" #'(lambda () (interactive) (magit-checkout-file (magit-get-current-branch) (buffer-file-name)))
  "gr" 'diff-hl-revert-hunk
  "gd" 'magit-diff-buffer-file
  "gp" #'(lambda () (interactive) (call-interactively 'magit-pull-from-upstream) (magit-process-buffer))
  "gP" #'(lambda () (interactive) (call-interactively 'magit-push-current-to-upstream) (magit-process-buffer))
  "gM" #'(lambda () (interactive) (call-interactively 'magit-merge))
  "aoA" #'(lambda () (interactive) (require 'calfw-org) (cfw:open-org-calendar))
  "aoR" #'(lambda () (interactive) (org-refile '(4)))
  "aob" 'org-iswitchb
  "gho" #'browse-at-remote
  "to" #'org-toggle-link-display
  "s-f" #'(lambda () (interactive) (lx/set-monospaced-font "Source Code Pro" "STHeiti" 12 14 13 16))
  "ps" #'lx/find-or-create-projectile-snippet-file
  "ad" #'(lambda () (interactive) (call-interactively 'dired))
  "tl" #'copy-org-links-at-point
  "xa#" #'spacemacs/align-repeat-hash
  "wxj" #'(lambda () (interactive) (let ((wind (windmove-find-other-window 'down))) (when (and wind (not (minibufferp (window-buffer wind)))) (delete-window wind))))
  "wxk" #'(lambda () (interactive) (let ((wind (windmove-find-other-window 'up))) (when (and wind (not (minibufferp (window-buffer wind)))) (delete-window wind))))
  "wxh" #'(lambda () (interactive) (let ((wind (windmove-find-other-window 'left))) (when (and wind (not (minibufferp (window-buffer wind)))) (delete-window wind))))
  "wxl" #'(lambda () (interactive) (let ((wind (windmove-find-other-window 'right))) (when (and wind (not (minibufferp (window-buffer wind)))) (delete-window wind))))
  ;; "wpl" #'(lambda () (interactive) (call-interactively 'popwin:popup-last-buffer) (call-interactively 'popwin:select-popup-window))
  "wpl" #'(lambda () (interactive) (popwin:pop-to-buffer (get-buffer "*rspec-compilation*")) (delete-window (get-buffer-window " *popwin-dummy*")) (select-window (get-buffer-window "*rspec-compilation*")))
)
