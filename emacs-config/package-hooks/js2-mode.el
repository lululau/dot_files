(with-eval-after-load 'js2-mode
  (define-key js2-mode-map (kbd (if (display-graphic-p) "<C-return>" "C-RET")) 'spacemacs/jump-to-definition)
  (define-key js2-mode-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'spacemacs/jump-to-definition-other-window))
