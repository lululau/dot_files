(spacemacs|create-align-repeat-x "hash" "#")

(spacemacs/set-leader-keys
  "bk" #'(lambda () (interactive) (call-interactively 'kill-buffer))
  "gC" 'magit-commit
  "gc" #'(lambda (arg) (interactive "P") (call-interactively (if arg 'magit-branch-and-checkout 'magit-checkout)))
  "gS" #'(lambda () (interactive) (magit-run-git-async "status") (magit-process-buffer))
  "gu" #'(lambda () (interactive) (magit-file-checkout (magit-get-current-branch) (buffer-file-name)))
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
  "df" #'find-dired
  "dn" #'find-name-dired
  "dg" #'find-grep-dired
  "dF" #'mfd-dired
  "dN" #'mfd-name-dired
  "dG" #'mfd-grep-dired
  "d TAB" #'(lambda () (interactive) (switch-to-buffer (--find (eq 'dired-mode (with-current-buffer it major-mode)) (buffer-list))))
  "pL" #'lx/helm-persp-replace-project
  "bL" #'lx/persp-swith-to-buffer-project
  "jw" #'evil-avy-goto-word-0
  "ja" #'evil-avy-goto-char-in-line
  "aj" #'jq-interactively
  "xx" #'lx/set-last-dir-and-quit
  "it" #'lx/insert-timestamp
  "fi" 'lx/open-with-idea
  "fa" 'lx/browse-file-or-directory-in-alfred
  "fei" #'(lambda () (interactive) (find-file (format "%sinit.el" user-emacs-directory)))
  "te" #'toggle-company-english-helper
  "t C-s" #'lx/toggle-global-syntax-check
  "tP" #'proxy-mode
  "aE" #'es-command-center
  "f." #'lx/open-current-bufffer-dir-in-finder
  "xle" #'lx/remove-empty-lines
  "xlbe" #'base64-encode-utf8-region
  "xlbd" #'base64-decode-utf8-region
  "aojo" #'org-journal-find-today-entry
  "at" #'helm-tramp
  "s*" #'spacemacs/helm-swoop-region-or-symbol
  "amm" #'lx/load-or-switch-to-emms
  "asn" #'ssh-tunnels
  "cv" #'customize-variable
  "aas" #'code-archive-save-code
  "aai" #'code-archive-insert-org-block
)

(spacemacs/set-leader-keys-for-major-mode 'dired-mode
  "h" 'dired-dotfiles-toggle
  "ar" 'tda/rsync
  "aR" 'tda/rsync-delete
  "az" 'tda/zip
  "au" 'tda/unzip
  "aa" 'tda/rsync-multiple-mark-file
  "ae" 'tda/rsync-multiple-empty-list
  "ad" 'tda/rsync-multiple-remove-item
  "av" 'tda/rsync-multiple
  "as" 'tmtxt/dired-async-get-files-size
  "aq" 'tda/download-to-current-dir
  "al" 'tda/download-clipboard-link-to-current-dir
  "ax" 'tda/execute-command-in-current-dir
  "f" dired-filter-map
  "ra" 'dired-ranger-copy
  "rv" 'dired-ranger-paste
  "re" 'dired-ranger-move
  "lf" 'dired-list-find-file
  "ln" 'dired-list-find-name
  "lg" 'dired-list-git-ls-files
  "lG" 'dired-list-grep
  "lk" 'dired-list-kill-process
  "ll" 'dired-list-locate
  "lv" 'lx/open-file-in-lnav
  "nn" 'dired-narrow
  "nr" 'dired-narrow-regexp
  "nf" 'dired-narrow-fuzzy)

(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
   "cb" #'byte-compile-current-buffer-file)
