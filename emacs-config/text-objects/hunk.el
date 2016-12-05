(evil-define-text-object evil-a-hunk (count &optional beg end type)
  (let ((hunk (diff-hl-hunk-overlay-at (point))))
    (list (overlay-start hunk) (overlay-end hunk))))

(define-key evil-outer-text-objects-map "h" 'evil-a-hunk)
(define-key evil-inner-text-objects-map "h" 'evil-a-hunk)

