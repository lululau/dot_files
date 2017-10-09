(with-eval-after-load 'calc
  (define-key calc-mode-map [M-tab] #'calc-roll-up)
  (define-key calc-mode-map (kbd "M-k") #'lx/window-up-fallback-to-switch-frame))

(with-eval-after-load 'calc-yank
  (define-key calc-mode-map [M-tab] #'calc-roll-up)
  (define-key calc-mode-map (kbd "M-k") #'lx/window-up-fallback-to-switch-frame))
