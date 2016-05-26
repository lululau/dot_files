(spacemacs|use-package-add-hook enh-ruby-mode
  :post-config
  (progn
    (define-key enh-ruby-mode-map (kbd "s-r b") 'enh-ruby-toggle-block)
    (define-key enh-ruby-mode-map (kbd "M-s-b") #'toggle-pry-breakpoint)
    (define-key enh-ruby-mode-map (kbd "M-s-c") #'cleanup-pry-breakpoints)
    (dolist (mode '(ruby-mode enh-ruby-mode)) (spacemacs/set-leader-keys-for-major-mode mode "TAB" 'rspec-toggle-spec-and-target))))
