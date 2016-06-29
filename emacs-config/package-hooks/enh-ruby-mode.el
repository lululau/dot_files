(with-eval-after-load 'enh-ruby-mode
  (define-key enh-ruby-mode-map (kbd "s-r b") 'enh-ruby-toggle-block)
  (define-key enh-ruby-mode-map (kbd "M-s-b") #'toggle-pry-breakpoint)
  (define-key enh-ruby-mode-map (kbd "M-s-c") #'cleanup-pry-breakpoints)
  (dolist (mode '(ruby-mode enh-ruby-mode))
    (spacemacs/set-leader-keys-for-major-mode mode "TAB" 'rspec-toggle-spec-and-target)))

(spacemacs|use-package-add-hook enh-ruby-mode
  :post-init
  (progn
    (add-hook
     'enh-ruby-mode-hook
     #'(lambda ()
         (setq evil-shift-width 2)
         (modify-syntax-entry ?: ".")
         (modify-syntax-entry ?! "_")
         (modify-syntax-entry ?? "_")))))

(spacemacs|use-package-add-hook rvm
  :post-init
  (remove-hook
   'enh-ruby-mode-hook
   'rvm-activate-corresponding-ruby))
