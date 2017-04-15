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
                   (const icons)
                   (const nerd)
                   (const uni)))

(defun neo-buffer--insert-fold-symbol (name &optional node-name)
  "Write icon by NAME, the icon style affected by neo-theme.
`open' write opened folder icon.
`close' write closed folder icon.
`leaf' write leaf icon.
Optional NODE-NAME is used for the `icons' theme"
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
     ((equal neo-theme 'icons)
      (unless (require 'all-the-icons nil 'noerror)
        (error "Package `all-the-icons' isn't installed"))
      (or (and (equal name 'open)  (insert (all-the-icons-icon-for-dir node-name "down")))
          (and (equal name 'close) (insert (all-the-icons-icon-for-dir node-name "right")))
          (and (equal name 'leaf)  (insert (format "\t\t\t%s\t" (all-the-icons-icon-for-file node-name))))))
     (t
      (or (and (equal name 'open)  (funcall n-insert-symbol "-"))
          (and (equal name 'close) (funcall n-insert-symbol "+")))))))

  (define-key neotree-mode-map (kbd "s-t") #'(lambda () (interactive) (select-window-by-number 1) (split-window-right-and-focus)))
  (define-key neotree-mode-map (kbd "s-T") #'(lambda () (interactive) (select-window-by-number 1) (split-window-below-and-focus)))
  (define-key neotree-mode-map (kbd "s-n") #'(lambda () (interactive) (select-window-by-number 1) (switch-to-buffer-other-window (generate-new-buffer "*Untitled*"))))
  (define-key neotree-mode-map (kbd "s-N") #'(lambda () (interactive) (select-window-by-number 1) (switch-to-buffer (generate-new-buffer "*Untitled*"))))
  (define-key neotree-mode-map [tab] #'(lambda () (interactive) (neo-buffer--toggle-expand (neo-buffer--get-filename-current-line)) (neo-buffer--refresh t))))
