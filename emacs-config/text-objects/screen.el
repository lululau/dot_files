;; -*- lexical-binding: t; -*-

(cl-defmacro lx/def-screen-text-object (name docstring)
  "Define a screen text object selecting the visible area."
  `(evil-define-text-object ,name (count &optional beg end type)
     ,docstring
     :type line
     (let* ((wstart (window-start))
            (wend (save-excursion
                    (goto-char wstart)
                    (vertical-motion (1- (window-body-height)))
                    (line-end-position))))
       (evil-range wstart wend 'line))))

(lx/def-screen-text-object evil-inner-screen
  "Select the visible screen area.")
(lx/def-screen-text-object evil-outer-screen
  "Select the visible screen area.")

(define-key evil-inner-text-objects-map "S" 'evil-inner-screen)
(define-key evil-outer-text-objects-map "S" 'evil-outer-screen)
