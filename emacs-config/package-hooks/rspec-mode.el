(spacemacs|use-package-add-hook rspec-mode
  :post-config
  (define-key rspec-mode-map [M-s-tab] #'rspec-toggle-spec-and-target-find-example)
  (define-key enh-ruby-mode-map [M-s-tab] #'rspec-toggle-spec-and-target-find-example))
