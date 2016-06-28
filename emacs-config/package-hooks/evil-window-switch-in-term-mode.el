(with-eval-after-load 'term
  (let ((map (lookup-key term-raw-map "\e")))
    (define-key map "h" #'evil-window-left)
    (define-key map "j" #'evil-window-down)
    (define-key map "k" #'evil-window-up)
    (define-key map "l" #'evil-window-right)

    (define-key map "H" #'evil-window-move-far-left)
    (define-key map "J" #'evil-window-move-very-bottom)
    (define-key map "K" #'evil-window-move-very-top)
    (define-key map "L" #'evil-window-move-far-right)))
