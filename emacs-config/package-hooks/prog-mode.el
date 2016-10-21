(with-eval-after-load 'prog-mode
  (define-key prog-mode-map (kbd "<s-return>") 'jump-to-definition-of-symbol-at-point-other-window))
