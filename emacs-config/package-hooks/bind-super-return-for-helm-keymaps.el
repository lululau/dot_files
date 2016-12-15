(with-eval-after-load 'helm-bookmark
  (define-key helm-bookmark-map (if (display-graphic-p) "<s-return>" "s-RET") 'helm-bookmark-run-jump-other-window))

(with-eval-after-load 'helm-buffers
  (define-key helm-buffers-ido-virtual-map (if (display-graphic-p) "<s-return>" "s-RET")   'helm-ff-run-switch-other-window)
  (define-key helm-buffer-map (if (display-graphic-p) "<s-return>" "s-RET")     'helm-buffer-switch-other-window))

(with-eval-after-load 'helm-grep
  (define-key helm-grep-map (if (display-graphic-p) "<s-return>" "s-RET") 'helm-grep-run-other-window-action))

(with-eval-after-load 'helm-files
  (define-key helm-find-files-map (if (display-graphic-p) "<s-return>" "s-RET") 'helm-ff-run-switch-other-window))

(with-eval-after-load 'helm-locate
  (define-key helm-generic-files-map (if (display-graphic-p) "<s-return>" "s-RET") 'helm-ff-run-switch-other-window))

(with-eval-after-load 'helm-regexp
  (define-key helm-moccur-map (if (display-graphic-p) "<s-return>" "s-RET") 'helm-moccur-goto-line-ow))

(with-eval-after-load 'helm-tags
  (define-key helm-etags-map (if (display-graphic-p) "<s-return>" "s-RET") 'helm-etags-run-switch-other-window))

(with-eval-after-load 'helm-ag
  (define-key helm-ag-map (if (display-graphic-p) "<s-return>" "s-RET") 'helm-ag--run-other-window-action))

(with-eval-after-load 'helm-projectile
  (define-key (nth 3 helm-source-projectile-directories-list) (if (display-graphic-p) "<s-return>" "s-RET") 'helm-projectile-dired-find-dir-other-window))
