(evil-define-text-object evil-a-defun (count &optional beg end type)
  (evil-select-an-object 'evil-defun beg end type count))

(evil-define-text-object evil-inner-defun (count &optional beg end type)
  (evil-select-inner-object 'evil-defun beg end type count))

(define-key evil-inner-text-objects-map "m" 'evil-inner-defun)
(define-key evil-outer-text-objects-map "m" 'evil-a-defun)
