(with-eval-after-load 'evil-collection-dired
  (evil-collection-define-key 'motion 'dired-mode-map
    (kbd "TAB") 'dired-subtree-toggle
    (kbd "<backtab>") 'dired-subtree-cycle)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "TAB") 'dired-subtree-toggle
    (kbd "<backtab>") 'dired-subtree-cycle
    (kbd "M-j") 'lx/window-down-fallback-to-switch-frame
    (kbd "M-k") 'lx/window-up-fallback-to-switch-frame)

  (with-eval-after-load 'dired-subtree
    (evil-collection-define-key 'normal 'dired-mode-map
      (kbd "TAB") 'dired-subtree-toggle
      (kbd "<backtab>") 'dired-subtree-cycle
      (kbd "M-j") 'lx/window-down-fallback-to-switch-frame
      (kbd "M-k") 'lx/window-up-fallback-to-switch-frame)))


(with-eval-after-load 'evil-collection-vterm
  (evil-define-operator evil-collection-vterm-delete-forward-char (beg end type register)
    "Delete previous character."
    :motion evil-forward-char
    (interactive "<R><x>")
    (evil-collection-vterm-delete beg end type register))

  (evil-define-operator evil-collection-vterm-replace-char (beg end type register)
    "Replace character at point."
    :motion evil-forward-char
    (interactive "<R><x>")
    (evil-collection-vterm-delete beg end type register)
    (evil-collection-vterm-insert))

  (defun evil-collection-vterm-append-line ()
    "Append character at end-of-line."
    (interactive)
    (vterm-goto-char (vterm--get-end-of-line))
    (call-interactively #'evil-append)
    (vterm-send-C-e))

  (defun evil-collection-vterm-setup ()
    "Set up `evil' bindings for `vterm'."
    (evil-set-initial-state 'vterm-mode 'insert)

    (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)

    ;; Open to a better binding...
    (evil-collection-define-key '(normal insert) 'vterm-mode-map
      (kbd "C-c C-z") 'evil-collection-vterm-toggle-send-escape)

    ;; Evil has some "C-" bindings in insert state that shadow regular terminal
    ;; bindings. Don't raw-send "C-c" (prefix key) nor "C-h" (help prefix).
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd "C-a") 'vterm--self-insert
      (kbd "C-b") 'vterm--self-insert     ; Should not be necessary.
      (kbd "C-d") 'vterm--self-insert
      (kbd "C-e") 'vterm--self-insert
      (kbd "C-f") 'vterm--self-insert     ; Should not be necessary.
      (kbd "C-k") 'vterm--self-insert
      (kbd "C-l") 'vterm--self-insert     ; Should not be necessary.
      (kbd "C-n") 'vterm--self-insert
      (kbd "C-o") 'vterm--self-insert
      (kbd "C-p") 'vterm--self-insert
      (kbd "C-q") 'vterm--self-insert     ; Should not be necessary.
      (kbd "C-r") 'vterm--self-insert
      (kbd "C-s") 'vterm--self-insert     ; Should not be necessary.
      (kbd "C-t") 'vterm--self-insert
      (kbd "C-u") 'vterm--self-insert     ; Should not be necessary.
      (kbd "C-v") 'vterm--self-insert     ; Should not be necessary.
      (kbd "C-w") 'vterm--self-insert
      (kbd "C-y") 'vterm--self-insert
      (kbd "C-z") 'vterm--self-insert
      (kbd "<delete>") 'vterm-send-delete)

    (evil-collection-define-key 'normal 'vterm-mode-map
      "[[" 'vterm-previous-prompt
      "]]" 'vterm-next-prompt
      "p" 'vterm-yank
      "a" 'evil-collection-vterm-append
      "A" 'evil-collection-vterm-append-line
      "d" 'evil-collection-vterm-delete
      "D" 'evil-collection-vterm-delete-line
      "x" 'evil-collection-vterm-delete-forward-char
      "r" 'evil-collection-vterm-replace-char
      (kbd "RET") 'vterm-send-return
      "i" 'evil-collection-vterm-insert
      "I" 'evil-collection-vterm-insert-line
      "u" 'vterm-undo
      "c" 'evil-collection-vterm-change
      "C" 'evil-collection-vterm-change-line)

    (evil-collection-define-key 'visual 'vterm-mode-map
      "d" 'evil-collection-vterm-delete
      "r" 'evil-collection-vterm-replace-char
      "x" 'evil-collection-vterm-delete-forward-char)))
