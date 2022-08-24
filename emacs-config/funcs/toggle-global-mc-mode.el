(defun lx/toggle-global-evil-mc-mode ()
  (interactive)
  (if (bound-and-true-p global-evil-mc-mode)
      (progn
        (evil-mc-undo-all-cursors)
        (global-evil-mc-mode -1)
        (message "global evil-mc-mode disabled."))
    (global-evil-mc-mode 1)
    (message "global evil-mc-mode enabled.")))

(defun lx/evil-mc-make-cursors-by-regexp ()
  (interactive)
  (require 'evil-mc)
  (call-interactively 'evil-mc-make-cursors-by-regexp))
