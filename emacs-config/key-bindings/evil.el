(define-key evil-normal-state-map "s" #'(lambda () (interactive) (call-interactively 'evil-substitute) (call-interactively 'indent-for-tab-command)))
(define-key evil-motion-state-map (kbd "t") #'evil-avy-goto-char)
(define-key evil-motion-state-map (kbd "T") #'evil-avy-goto-char-2)
;; (define-key evil-motion-state-map (kbd "SPC SPC") #'evil-avy-goto-char-2)
(define-key evil-motion-state-map (kbd "] c") #'diff-hl-next-hunk)
(define-key evil-motion-state-map (kbd "[ c") #'diff-hl-previous-hunk)
;; (define-key evil-motion-state-map (kbd "C-]") #'jump-to-definition-of-symbol-at-point)
;; (define-key evil-motion-state-map (kbd (if (display-graphic-p) "<C-return>" "C-RET")) #'(lambda () (interactive) (call-interactively (if (eq major-mode 'org-mode) 'org-insert-heading-respect-content 'jump-to-definition-of-symbol-at-point))))

;; (define-key evil-motion-state-map (kbd "<s-mouse-1>") #'(lambda (event) (interactive "e") (mouse-set-point event) (jump-to-definition-of-symbol-at-point)))
(define-key evil-motion-state-map (kbd "<s-mouse-3>") #'evil-jumper/backward)
;; (define-key evil-motion-state-map (kbd "s-q") #'evil-emacs-state)
;; (define-key evil-lisp-state-map (kbd "s-q") #'evil-emacs-state)
;; (define-key evil-emacs-state-map (kbd "C-]") #'jump-to-definition-of-symbol-at-point)
;; (define-key evil-emacs-state-map (kbd (if (display-graphic-p) "<C-return>" "C-RET")) #'(lambda () (interactive) (call-interactively (if (eq major-mode 'org-mode) 'org-insert-heading-respect-content 'jump-to-definition-of-symbol-at-point))))
;; (define-key evil-emacs-state-map (kbd "<s-mouse-1>") #'(lambda (event) (interactive "e") (mouse-set-point event) (jump-to-definition-of-symbol-at-point)))
;; (define-key evil-emacs-state-map (kbd "<s-mouse-3>") #'evil-jumper/backward)
(define-key evil-emacs-state-map (kbd "s-q") #'evil-exit-emacs-state)
(define-key evil-emacs-state-map (kbd "<f11>") #'evil-exit-emacs-state)

;; Use Hybrid style instead of original emacs-state
(define-key evil-motion-state-map (kbd "s-q") #'evil-hybrid-state)
(define-key evil-visual-state-map (kbd "s-q") #'evil-normal-state)
(define-key evil-motion-state-map (kbd "<f11>") #'evil-hybrid-state)
(define-key evil-visual-state-map (kbd "<f11>") #'evil-normal-state)
;; (define-key evil-hybrid-state-map (kbd "C-]") #'jump-to-definition-of-symbol-at-point)
;; (define-key evil-hybrid-state-map (kbd (if (display-graphic-p) "<C-return>" "C-RET")) #'(lambda () (interactive) (call-interactively (if (eq major-mode 'org-mode) 'org-insert-heading-respect-content 'jump-to-definition-of-symbol-at-point))))
;; (define-key evil-hybrid-state-map (kbd "<s-mouse-1>") #'(lambda (event) (interactive "e") (mouse-set-point event) (jump-to-definition-of-symbol-at-point)))
(define-key evil-hybrid-state-map (kbd "<s-mouse-3>") #'evil-jumper/backward)
(define-key evil-hybrid-state-map (kbd "s-q") #'evil-exit-hybrid-state)
(define-key evil-hybrid-state-map (kbd "<f11>") #'evil-exit-hybrid-state)
(define-key evil-motion-state-map (kbd "C-z") #'evil-hybrid-state)
(define-key evil-hybrid-state-map (kbd "C-z") #'evil-exit-hybrid-state)

(define-key evil-normal-state-map (kbd "Y") #'yank-to-end-of-line)
(define-key evil-motion-state-map (kbd "Y") #'yank-to-end-of-line)

(define-key evil-outer-text-objects-map "o" 'evil-a-word)
(define-key evil-inner-text-objects-map "o" 'evil-inner-word)
(define-key evil-outer-text-objects-map "w" 'evil-a-symbol)
(define-key evil-inner-text-objects-map "w" 'evil-inner-symbol)

(define-key evil-normal-state-map "za" (lambda () (interactive) (if (eq major-mode 'web-mode) (web-mode-fold-or-unfold) (evil-toggle-fold))))
(define-key evil-normal-state-map "gf" #'(lambda () (interactive) (if (and (eq 'ruby-mode major-mode) projectile-rails-mode)
                                                                      (call-interactively 'projectile-rails-goto-file-at-point)
                                                                    (call-interactively 'ffap-other-window))))
(define-key evil-ex-completion-map "\C-a" nil)
(define-key evil-ex-completion-map "\C-b" nil)
(define-key evil-ex-completion-map "\C-d" nil)
(define-key evil-ex-completion-map "\C-k" nil)
(define-key evil-normal-state-map (kbd "RET") #'(lambda () (interactive) (evil-insert-newline-below)))
(define-key evil-motion-state-map (kbd "RET") #'(lambda () (interactive) (evil-insert-newline-below)))
(define-key evil-normal-state-map (kbd "<S-return>") #'(lambda () (interactive) (save-excursion (evil-insert-newline-above))))
(define-key evil-motion-state-map (kbd "<S-return>") #'(lambda () (interactive) (save-excursion (evil-insert-newline-above))))
(evil-leader/set-key "SPC" 'avy-goto-char-2)

(with-eval-after-load 'evil-lisp-state
  (define-key evil-lisp-state-map (kbd "C-z") #'evil-hybrid-state)
  (define-key evil-lisp-state-map (kbd "s-q") #'evil-hybrid-state)
  (define-key evil-lisp-state-map (kbd "<f11>") #'evil-hybrid-state))


(evil-define-motion evil-previous-ten-line (count)
  "Move the cursor 10 lines up."
  :type line
  (let (line-move-visual)
    (evil-line-move (- (or count 10)))))

(evil-define-motion evil-next-ten-line (count)
  "Move the cursor 10 lines down."
  :type line
  (let (line-move-visual)
    (evil-line-move (or count 10))))

(define-key evil-motion-state-map (kbd "s-k") #'evil-previous-ten-line)
(define-key evil-motion-state-map (kbd "s-j") #'evil-next-ten-line)
