(with-eval-after-load 'alchemist-iex
  (define-key alchemist-iex-mode-map (kbd "C-S-l") 'comint-clear-buffer)
  (define-key alchemist-iex-mode-map (kbd "s-W") '(lambda () (interactive) (unwind-protect (comint-send-eof)) (kill-this-buffer) (delete-window)))
  (define-key alchemist-iex-mode-map (kbd "M-D") #'(lambda () (interactive) (comint-send-string (inf-ruby-proc) "exit-program\n")))
  (define-key alchemist-iex-mode-map (kbd "C-p") #'comint-previous-input)
  (define-key alchemist-iex-mode-map (kbd "C-n") #'comint-next-input))
