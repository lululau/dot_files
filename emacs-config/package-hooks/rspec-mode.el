(spacemacs|use-package-add-hook rspec-mode
  :post-config
  (define-key rspec-mode-map [M-s-tab] #'rspec-toggle-spec-and-target-find-example)
  (define-key ruby-mode-map [M-s-tab] #'rspec-toggle-spec-and-target-find-example)
  (spacemacs/set-leader-keys-for-major-mode 'rspec-compilation-mode
    "tl"    #'(lambda () (interactive) (with-selected-window (cadr (window-list)) (call-interactively 'rspec-run-last-failed)))
    "tr"    #'(lambda () (interactive) (with-selected-window (cadr (window-list)) (call-interactively 'rspec-rerun)))))
