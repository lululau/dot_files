(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "<M-return>") #'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "<S-return>") 'lsp-find-references))
