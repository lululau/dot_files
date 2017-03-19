(with-eval-after-load 'evil-commands
  (evil-define-command evil-exit-hybrid-state (&optional buffer message)
    "Exit Emacs state.
Changes the state to the previous state, or to Normal state
if the previous state was Emacs state."
    :keep-visual t
    :suppress-operator t
    (interactive '(nil t))
    (with-current-buffer (or buffer (current-buffer))
      (when (evil-hybrid-state-p)
        (evil-change-to-previous-state buffer message)
        (when (evil-hybrid-state-p)
          (evil-normal-state (and message 1)))))))
