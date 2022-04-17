(with-eval-after-load 'elisp-mode
  (define-key lisp-interaction-mode-map (kbd "C-M-i") nil)
  (define-key emacs-lisp-mode-map (kbd "C-M-i") nil))

(with-eval-after-load 'elisp-slime-nav
  (define-key elisp-slime-nav-mode-map (kbd "M-.") nil))
