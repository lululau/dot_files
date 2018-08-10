(add-hook 'ruby-mode-hook #'(lambda () (setq-local indent-line-function 'ruby-indent-line)))

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "C-j") 'lx/ruby-send-line)
  (define-key ruby-mode-map (kbd "s-r b") 'ruby-toggle-block)
  (define-key ruby-mode-map (kbd "M-s-b") #'toggle-pry-breakpoint)
  (define-key ruby-mode-map (kbd "M-s-c") #'cleanup-pry-breakpoints)
  (define-key ruby-mode-map (kbd "<left-margin> <s-mouse-1>") #'(lambda () (interactive)
                                                                  (comint-send-string (lx/last-pry-proc) (format "b %s:%s\nb\n" (buffer-file-name) (spacemacs//line-at-click)))))

  (define-key ruby-mode-map (kbd (if (display-graphic-p) "<C-return>" "C-RET")) 'jump-to-definition-of-symbol-at-point)
  (define-key ruby-mode-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'jump-to-definition-of-symbol-at-point-other-window)
  )

(spacemacs|use-package-add-hook robe
  :post-config
  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
    "sa" 'lx/ruby-send-reload
    "sl" 'lx/ruby-send-line
    "sL" 'lx/ruby-send-line-and-go
    "sp" 'lx/ruby-send-paragraph
    "sP" 'lx/ruby-send-paragraph-and-go
    "sp" 'lx/ruby-send-babel-block
    "sP" 'lx/ruby-send-babel-block-and-go))

(spacemacs|use-package-add-hook ruby-mode
  :post-init
  (progn
    (dolist (mode '(ruby-mode))
      (spacemacs/set-leader-keys-for-major-mode mode "TAB" 'rspec-toggle-spec-and-target))
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
