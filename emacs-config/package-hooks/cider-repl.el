(with-eval-after-load 'cider-repl
  (define-key cider-repl-mode-map (kbd "C-p") 'cider-repl-previous-input)
  (define-key cider-repl-mode-map (kbd "C-n") 'cider-repl-next-input)

  (add-hook 'cider-repl-mode-hook #'(lambda ()
                                      (add-hook 'evil-hybrid-state-entry-hook #'end-of-buffer nil t))))
