(with-eval-after-load 'prog-mode
  (define-key prog-mode-map (kbd (if (display-graphic-p) "<C-return>" "C-RET")) 'jump-to-definition-of-symbol-at-point)
  (define-key prog-mode-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'jump-to-definition-of-symbol-at-point-other-window))
