(defun lx/keyboard-quit ()
  (interactive)
  (spacemacs/evil-search-clear-highlight) (copilot-clear-overlay) (keyboard-quit))
