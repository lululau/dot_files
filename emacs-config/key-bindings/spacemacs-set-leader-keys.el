;; -*- lexical-binding: t; -*-

(spacemacs|create-align-repeat-x "hash" "#")

(spacemacs/set-leader-keys
  "bk" #'lx/kill-buffer-interactively
  "gC" 'magit-commit
  "gc" #'lx/magit-smart-checkout
  "gS" #'lx/magit-status-async-and-show
  "gu" #'lx/magit-file-undo-checkout
  "gr" 'diff-hl-revert-hunk
  "gd" 'magit-diff-buffer-file
  "gp" #'lx/magit-pull-and-show
  "gP" #'lx/magit-push-and-show
  "gM" #'lx/magit-merge-interactive
  "aoA" #'lx/open-org-calendar
  "aoR" #'lx/org-refile
  "aob" 'org-iswitchb
  "gho" #'browse-at-remote
  ;; "Ct" #'copilot-toggle-auto-copilot
  "Cd" #'copilot-diagnose
  "tol" #'org-toggle-link-display
  "tom" #'org-toggle-org-modern-mode
  "toM" #'org-toggle-global-org-modern-mode
  "tA" #'lx/toggle-annotate-mode
  "s-f" #'lx/set-default-font
  "s-0" #'lx/set-default-font
  "ps" #'lx/find-or-create-projectile-snippet-file
  "aC" #'calendar
  "col" #'copy-org-links-at-point
  "xa#" #'spacemacs/align-repeat-hash
  "w|"  #'split-window-right-and-focus
  "wxj" #'lx/delete-window-below
  "wxk" #'lx/delete-window-above
  "wxh" #'lx/delete-window-left
  "wxl" #'lx/delete-window-right
  ;; "wpl" #'(lambda () (interactive) (call-interactively 'popwin:popup-last-buffer) (call-interactively 'popwin:select-popup-window))
  "wpl" #'lx/popwin-rspec-buffer
  "ael" #'geeknote-notebook-list
  "tt" #'lx/toggle-title-format
  "df" #'find-dired
  "dn" #'find-name-dired
  "dg" #'find-grep-dired
  "dF" #'fd-dired
  "dN" #'fd-name-dired
  "d s-f" #'mfd-dired
  "d s-n" #'mfd-name-dired
  "d s-g" #'mfd-grep-dired
  "d TAB" #'lx/switch-to-dired-buffer
  "pL" #'lx/helm-persp-replace-project
  "bL" #'lx/persp-swith-to-buffer-project
  "jw" #'evil-avy-goto-word-0
  "ja" #'evil-avy-goto-char-in-line
  "aj" #'jq-interactively
  "xx" #'lx/set-last-dir-and-quit
  "it" #'lx/insert-timestamp
  "fi" 'lx/open-with-idea
  "fa" 'lx/browse-file-or-directory-in-alfred
  "fei" #'lx/open-emacs-init-file
  "te" #'toggle-company-english-helper
  "t C-s" #'lx/toggle-global-syntax-check
  "tP" #'proxy-mode
  "aE" #'es-command-center
  "f." #'lx/open-current-bufffer-dir-in-finder
  "xle" #'lx/remove-empty-lines
  "xlbe" #'base64-encode-utf8-region
  "xlbd" #'base64-decode-utf8-region
  "aojo" #'org-journal-find-today-entry
  "aojj" #'org-journal-new-todo-entry
  "aojJ" #'org-journal-new-entry
  "aht" #'helm-tramp
  "s*" #'spacemacs/helm-swoop-region-or-symbol
  "amm" #'lx/load-or-switch-to-emms
  "asn" #'ssh-tunnels
  "cv" #'customize-variable
  "aas" #'code-archive-save-code
  "aai" #'code-archive-insert-org-block
  "qw" #'spacemacs/frame-killer
  "\\" #'lx/switch-to-previous-perp
  "p." #'projectile-find-file-in-pwd
  "bc" 'helm-cwd-buffers
  "Ka" #'lx/kubectl/apply-region-or-buffer
  "Kd" #'lx/kubectl/delete-region-or-buffer
  "KA" #'lx/kubectl/istio-inject-apply-region-or-buffer
  "KD" #'lx/kubectl/istio-inject-delete-region-or-buffer
  "dk" #'docker
  "qk" #'lx/kill-all-except-default
  "bM" #'lx/switch-to-warning-buffer
  "ag" #'remoto-browse
  ;; "agu" #'omg-sync
  ;; "ags" #'omg-repo-list-starred
  ;; "agr" #'omg-repo-list-created
  ;; "agt" #'omg-trending-list
  ;; "agg" #'omg-gist-list-created
  ;; "agG" #'omg-gist-list-starred
  ;; "agi" #'omg-whoami
  "man" #'helm-man-woman
  "mai" #'helm-info
  "xit" #'lx/insert-timestamp
  "xpt" #'lx/parse-timestamp
  "xC" #'lx/cleanup-text-properties

  "hhl" #'highlight-lines-matching-regexp
  "hhr" #'highlight-regexp
  "hhp" #'highlight-phrase
  "hhw" #'highlight-symbol-at-point
  "hhu" #'lx/unhighlight-all

  "hdd" #'dash-at-point

  "cg" #'chatgpt-query
  "ct" #'gptel
  "cs" #'chatgpt-shell
  "CS" #'lx/run-chatsh

  "cxg" #'mind-wave-generate-code
  "cxr" #'mind-wave-refactory-code
  "cxR" #'mind-wave-refactory-code-with-input
  "cxc" #'mind-wave-comment-code
  "cxe" #'mind-wave-explain-code
  "cxG" #'mind-wave-generate-commit-name

  "axx" #'xwidget-webkit-browse-url
  "axh" #'xwwp-history-show
  "axjc" #'lx/xwidget-open-local-clash
  "axjC" #'lx/xwidget-open-remote-clash
  "axjr" #'lx/xwidget-open-openclash
  "axjg" #'lx/xwidget-open-github
  "axjl" #'lx/xwidget-open-gitlab
  "axjj" #'lx/xwidget-open-jenkins
  "axjJ" #'lx/xwidget-open-jira
  "axjv" #'lx/xwidget-open-v2ex
  "axje" #'lx/xwidget-open-emacs-china
  "axjt" #'lx/xwidget-open-twitter

  "$m" #'mcp-hub-start
  "$gr" #'gptel-mcp-register-tool

  ;; "acc" #'claude-code-transient
  "ac" #'claude-code-ide-menu

  "aY" #'yas-reload-all
  "ay" #'agent-shell
  "aq" #'clutch-query-console

  "aP"  'list-processes
  "ap"  'lx/proced-same-window
  )

(spacemacs/set-leader-keys-for-major-mode 'dired-mode
  "h" 'dired-dotfiles-toggle
  "aC" 'dired-async-do-copy
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
  "nf" 'dired-narrow-fuzzy
  "rs" 'dired-rsync
  "r." 'dired-rsync-transient)

(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  "cb" #'byte-compile-current-buffer-file)


(spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode
  "N" #'org-noter)
