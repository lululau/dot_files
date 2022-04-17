(with-eval-after-load 'lsp-java
  (evil-define-key 'normal java-mode-map (kbd "<C-return>") 'lsp-find-implementation)
  (evil-define-key 'normal java-mode-map (kbd "<s-return>") 'spacemacs/jump-to-definition)
  (evil-define-key 'normal java-mode-map (kbd "<S-return>") 'lsp-find-references))
