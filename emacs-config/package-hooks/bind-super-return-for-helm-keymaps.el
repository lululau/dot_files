(with-eval-after-load 'helm-bookmark
  (define-key helm-bookmark-map (kbd "<s-return>") 'helm-bookmark-run-jump-other-window))

(with-eval-after-load 'helm-buffers
  (define-key helm-buffers-ido-virtual-map (kbd "<s-return>")   'helm-ff-run-switch-other-window)
  (define-key helm-buffer-map (kbd "<s-return>")     'helm-buffer-switch-other-window))

(with-eval-after-load 'helm-grep
  (define-key helm-grep-map (kbd "<s-return>") 'helm-grep-run-other-window-action))

(with-eval-after-load 'helm-files
  (define-key helm-find-files-map (kbd "<s-return>") 'helm-ff-run-switch-other-window))

(with-eval-after-load 'helm-locate
  (define-key helm-generic-files-map (kbd "<s-return>") 'helm-ff-run-switch-other-window))

(with-eval-after-load 'helm-regexp
  (define-key helm-moccur-map (kbd "<s-return>") 'helm-moccur-goto-line-ow))

(with-eval-after-load 'helm-tags
  (define-key helm-etags-map (kbd "<s-return>") 'helm-etags-run-switch-other-window))

(with-eval-after-load 'helm-ag
  (define-key helm-ag-map (kbd "<s-return>") 'helm-ag--run-other-window-action))

(with-eval-after-load 'helm-projectile
  (define-key (nth 3 helm-source-projectile-directories-list) (kbd "<s-return>") 'helm-projectile-dired-find-dir-other-window))
