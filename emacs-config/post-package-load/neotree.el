(spacemacs|use-package-add-hook neotree
  :post-config
  (defcustom neo-theme 'classic
    "*The tree style to display.
`classic' use icon to display, it only it suitable for GUI mode.
`ascii' is the simplest style, it will use +/- to display the fold state,
it suitable for terminal.
`arrow' use unicode arrow.
`nerd' use the nerdtree indentation mode and arrow.
`uni' use unicode characters for arrow, folder and file."
    :group 'neotree
    :type '(choice (const classic)
                   (const ascii)
                   (const arrow)
                   (const nerd)
                   (const uni)))

  (defun neo-buffer--insert-fold-symbol (name)
    "Write icon by NAME, the icon style affected by neo-theme.
`open' write opened folder icon.
`close' write closed folder icon.
`leaf' write leaf icon."
    (let ((n-insert-image (lambda (n)
                            (insert-image (neo-buffer--get-icon n))))
          (n-insert-symbol (lambda (n)
                             (neo-buffer--insert-with-face
                              n 'neo-expand-btn-face))))
      (cond
       ((and window-system (equal neo-theme 'classic))
        (or (and (equal name 'open)  (funcall n-insert-image "open"))
            (and (equal name 'close) (funcall n-insert-image "close"))
            (and (equal name 'leaf)  (funcall n-insert-image "leaf"))))
       ((equal neo-theme 'arrow)
        (or (and (equal name 'open)  (funcall n-insert-symbol "▾"))
            (and (equal name 'close) (funcall n-insert-symbol "▸"))))
       ((equal neo-theme 'nerd)
        (or (and (equal name 'open)  (funcall n-insert-symbol "▾ "))
            (and (equal name 'close) (funcall n-insert-symbol "▸ "))
            (and (equal name 'leaf)  (funcall n-insert-symbol "  "))))
       ((and window-system (equal neo-theme 'uni))
        (or (and (equal name 'open)  (funcall n-insert-symbol "  "))
            (and (equal name 'close) (funcall n-insert-symbol "  "))
            (and (equal name 'leaf)  (funcall n-insert-symbol "   "))))
       (t
        (or (and (equal name 'open)  (funcall n-insert-symbol "-"))
            (and (equal name 'close) (funcall n-insert-symbol "+")))))))

  (define-key neotree-mode-map [tab] #'(lambda () (interactive) (neo-buffer--toggle-expand (neo-buffer--get-filename-current-line)) (neo-buffer--refresh t))))
