(defun lx/keyboard-quit ()
  (interactive)
  (spacemacs/evil-search-clear-highlight) (copilot-clear-overlay) (keyboard-quit))

(defun lx/tab (arg)
  (interactive "P")
  (or (copilot-accept-completion) (indent-for-tab-command arg)))

(defun lx/reset-hybrid-state-cursor-type-after-tab ()
   (interactive)
   (when (and (eq 'lx/tab this-command) (eq 'hybrid evil-state))
     (setq cursor-type '(bar . 2))
     (set-cursor-color "SkyBlue2")))
