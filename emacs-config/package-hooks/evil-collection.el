(with-eval-after-load 'evil-collection-dired
  (evil-collection-define-key 'motion 'dired-mode-map
    (kbd "TAB") 'dired-subtree-toggle
    (kbd "<backtab>") 'dired-subtree-cycle)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "TAB") 'dired-subtree-toggle
    (kbd "<backtab>") 'dired-subtree-cycle))


(with-eval-after-load 'evil-collection-vterm
  (evil-define-operator evil-collection-vterm-delete-forward-char (beg end type register)
    "Delete previous character."
    :motion evil-forward-char
    (interactive "<R><x>")
    (evil-collection-vterm-delete beg end type register))

  (defun evil-collection-vterm-append-line ()
    "Append character at end-of-line."
    (interactive)
    (vterm-goto-char (vterm--get-end-of-line))
    (call-interactively #'evil-append)
    (vterm-send-C-e))

  (evil-collection-define-key 'normal 'vterm-mode-map
    "x" 'evil-collection-vterm-delete-forward-char)

  (evil-collection-define-key 'visual 'vterm-mode-map
    "x" 'evil-collection-vterm-delete-forward-char))
