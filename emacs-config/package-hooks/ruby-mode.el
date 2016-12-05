(add-hook 'ruby-mode-hook #'(lambda () (setq-local indent-line-function 'ruby-indent-line)))

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "s-r b") 'ruby-toggle-block)
  (define-key ruby-mode-map (kbd "M-s-b") #'toggle-pry-breakpoint)
  (define-key ruby-mode-map (kbd "M-s-c") #'cleanup-pry-breakpoints)
  (define-key ruby-mode-map (kbd "<left-margin> <s-mouse-1>") #'(lambda () (interactive)
                                                                  (comint-send-string (lx/last-pry-proc) (format "b %s:%s\nb\n" (buffer-file-name) (spacemacs//line-at-click)))))
  (dolist (mode '(ruby-mode))
    (spacemacs/set-leader-keys-for-major-mode mode "TAB" 'rspec-toggle-spec-and-target)))

(spacemacs|use-package-add-hook ruby-mode
  :post-init
  (progn
    (add-hook
     'ruby-mode-hook
     #'(lambda ()
         (setq evil-shift-width 2)
         (modify-syntax-entry ?: ".")
         (modify-syntax-entry ?! "_")
         (modify-syntax-entry ?? "_")))))

(spacemacs|use-package-add-hook rvm
  :post-init
  (remove-hook
   'ruby-mode-hook
   'rvm-activate-corresponding-ruby))
