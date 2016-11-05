(evil-define-text-object evil-a-line (count &optional beg end type)
  (list (line-beginning-position) (line-end-position)))

(define-key evil-outer-text-objects-map "l" 'evil-a-line)
(define-key evil-inner-text-objects-map "l" 'evil-a-line)
