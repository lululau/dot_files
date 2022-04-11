(with-eval-after-load 'text-mode
  (define-key text-mode-map (kbd "C-M-i") nil)
  ;; (add-hook 'text-mode-hook #'turn-company-english-helper-on 100)
  )
