(with-eval-after-load 'helm-bookmark
  (define-key helm-bookmark-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'helm-bookmark-run-jump-other-window))

(with-eval-after-load 'helm-buffers
  (define-key helm-buffers-ido-virtual-map (kbd (if (display-graphic-p) "<s-return>" "s-RET"))   'helm-ff-run-switch-other-window)
  (define-key helm-buffer-map (kbd (if (display-graphic-p) "<s-return>" "s-RET"))     'helm-buffer-switch-other-window))

(with-eval-after-load 'helm-grep
  (define-key helm-grep-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'helm-grep-run-other-window-action))

(with-eval-after-load 'helm-files
  (define-key helm-find-files-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'helm-ff-run-switch-other-window))

(with-eval-after-load 'helm-locate
  (define-key helm-generic-files-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'helm-ff-run-switch-other-window))

; (with-eval-after-load 'helm-regexp
;   (define-key helm-moccur-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'helm-moccur-goto-line-ow))
;
(with-eval-after-load 'helm-tags
  (define-key helm-etags-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'helm-etags-run-switch-other-window))

(with-eval-after-load 'helm-ag
  (define-key helm-ag-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'helm-ag--run-other-window-action))

(with-eval-after-load 'helm-projectile
  (define-key (nth 2 helm-source-projectile-directories-list) (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'helm-projectile-dired-find-dir-other-window))
