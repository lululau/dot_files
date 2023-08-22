(add-hook 'ruby-mode-hook #'(lambda () (setq-local indent-line-function 'ruby-indent-line)))

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "C-j") 'lx/ruby-send-line)
  (define-key ruby-mode-map (kbd "s-r b") 'ruby-toggle-block)
  (define-key ruby-mode-map (kbd "M-s-b") #'toggle-pry-breakpoint)
  (define-key ruby-mode-map (kbd "M-s-c") #'cleanup-pry-breakpoints)
  (define-key ruby-mode-map (kbd "<left-margin> <s-mouse-1>") #'(lambda () (interactive)
                                                                  (comint-send-string (lx/last-pry-proc) (format "b %s:%s\nb\n" (buffer-file-name) (spacemacs//line-at-click)))))

  ;; (define-key ruby-mode-map (kbd (if (display-graphic-p) "<C-return>" "C-RET")) 'jump-to-definition-of-symbol-at-point)
  ;; (define-key ruby-mode-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'jump-to-definition-of-symbol-at-point-other-window)

  (defun ruby/jump-to-definition ()
    (interactive)
    (if (get-buffer-process (get-ruby-buffer))
        (call-interactively 'robe-jump)
      (call-interactively 'spacemacs/jump-to-definition)))

  (defun ruby/jump-to-definition-other-window ()
    (interactive)
    (let ((pos (point)))
      (switch-to-buffer-other-window (current-buffer))
      (goto-char pos)
      (call-interactively 'ruby/jump-to-definition)))

  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
    "hd" 'robe-doc
    "sa" 'lx/ruby-send-reload
    "sl" 'lx/ruby-send-line
    "sL" 'lx/ruby-send-line-and-go
    "sp" 'lx/ruby-send-paragraph
    "sP" 'lx/ruby-send-paragraph-and-go
    "sr" 'lx/ruby-send-region
    "sR" 'lx/ruby-send-region-and-go
    "RF" 'rubocop-autocorrect-current-file)


  (define-key ruby-mode-map (kbd (if (display-graphic-p) "<C-return>" "C-RET")) 'ruby/jump-to-definition)
  (define-key ruby-mode-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) 'ruby/jump-to-definition-other-window)

  ;; (define-key ruby-mode-map (kbd "s-r s") 'projectile-rails-server)
  (define-key ruby-mode-map (kbd "s-r s") 'vterm-rails/rails-server)
  (define-key ruby-mode-map (kbd "s-r d") 'vterm-rails/rails-dev)
  (define-key ruby-mode-map (kbd "s-r S") 'vterm-rails/sidekiq)
  (define-key ruby-mode-map (kbd "s-r s-v") 'rvm-activate-corresponding-ruby)
  (define-key ruby-mode-map (kbd "s-r /" ) 'lx/helm-ag-search-pry-breakpoints)

  (define-key ruby-mode-map (kbd "s-r c") #'(lambda (arg)
                                    (interactive "P")
                                    (let ((console (get-buffer (format "**%srailsconsole**" (projectile-project-name)))))
                                      (if (and arg console)
                                          (switch-to-buffer console)
                                        (call-interactively 'projectile-bundler-console)))))
  )

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
         (modify-syntax-entry ?? "_")
         (unless (bound-and-true-p rvm--current-ruby) (rvm-activate-corresponding-ruby))))))

(spacemacs|use-package-add-hook rvm
  :post-init
  (remove-hook
   'ruby-mode-hook
   'rvm-activate-corresponding-ruby))
