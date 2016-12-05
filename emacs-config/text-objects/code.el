(evil-define-text-object evil-a-code (count &optional beg end type)
  (list
   (save-excursion (mwim-beginning-of-code) (point))
   (save-excursion (mwim-end-of-code) (point))))

(define-key evil-outer-text-objects-map "c" 'evil-a-code)
(define-key evil-inner-text-objects-map "c" 'evil-a-code)
