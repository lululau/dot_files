(with-eval-after-load 'calc
  (define-key calc-mode-map (kbd "M-k") #'lx/window-up-fallback-to-switch-frame))
