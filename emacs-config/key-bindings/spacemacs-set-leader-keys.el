(spacemacs|create-align-repeat-x "hash" "#")

(spacemacs/set-leader-keys
  "bk" #'(lambda () (interactive) (call-interactively 'kill-buffer))
  "gC" 'magit-commit
  "gc" #'(lambda (arg) (interactive "P") (call-interactively (if arg 'magit-branch-and-checkout 'magit-checkout)))
  "gS" #'(lambda () (interactive) (magit-run-git-async "status") (magit-process-buffer))
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
  "s-f" #'(lambda () (interactive) (lx/set-monospaced-font "Source Code Pro" "黑体-简" 12 14 13 14))
  "ps" #'lx/find-or-create-projectile-snippet-file
  "aC" #'calendar
  "col" #'copy-org-links-at-point
  "xa#" #'spacemacs/align-repeat-hash
  "w|"  #'split-window-right-and-focus
  "wxj" #'(lambda () (interactive) (let ((wind (windmove-find-other-window 'down))) (when (and wind (not (minibufferp (window-buffer wind)))) (delete-window wind))))
  "wxk" #'(lambda () (interactive) (let ((wind (windmove-find-other-window 'up))) (when (and wind (not (minibufferp (window-buffer wind)))) (delete-window wind))))
  "wxh" #'(lambda () (interactive) (let ((wind (windmove-find-other-window 'left))) (when (and wind (not (minibufferp (window-buffer wind)))) (delete-window wind))))
  "wxl" #'(lambda () (interactive) (let ((wind (windmove-find-other-window 'right))) (when (and wind (not (minibufferp (window-buffer wind)))) (delete-window wind))))
  ;; "wpl" #'(lambda () (interactive) (call-interactively 'popwin:popup-last-buffer) (call-interactively 'popwin:select-popup-window))
  "wpl" #'(lambda () (interactive) (popwin:pop-to-buffer (get-buffer "*rspec-compilation*")) (delete-window (get-buffer-window " *popwin-dummy*")) (select-window (get-buffer-window "*rspec-compilation*")))
  "ael" #'geeknote-notebook-list
  "tt" #'lx/toggle-title-format
  "DD" #'find-dired
  "DF" #'find-name-dired
  "DG" #'find-grep-dired
  "d TAB" #'(lambda () (interactive) (switch-to-buffer (--find (eq 'dired-mode (with-current-buffer it major-mode)) (buffer-list))))
  "pL" #'lx/helm-persp-replace-project
  "bL" #'lx/persp-swith-to-buffer-project
  "jw" #'evil-avy-goto-word-0
  "ja" #'evil-avy-goto-char-in-line
)
