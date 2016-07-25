(with-eval-after-load 'inf-ruby-mode
  (define-key inf-ruby-mode-map (kbd "s-W") '(lambda () (interactive) (comint-send-eof) (kill-this-buffer) (delete-window))))
