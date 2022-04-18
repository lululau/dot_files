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

(defun lx/copilot-accept-or-forward-word (arg)
  (interactive "P")
  (if (bound-and-true-p copilot--overlay)
      (copilot-accept-completion-by-word 1)
    (call-interactively 'forward-word)))

(defun lx/copilot-accept-or-next-line (arg)
  (interactive "P")
  (if (bound-and-true-p copilot--overlay)
      (copilot-accept-completion-by-line 1)
    (call-interactively 'next-line)))

(defun lx/switch-to-buffer ()
  (interactive)
  (condition-case nil
      (progn
        (setq saved-ido-make-buffer-list-hook ido-make-buffer-list-hook)
        (setq ido-make-buffer-list-hook nil)
        (spacemacs-layouts/non-restricted-buffer-list-helm)
        (setq ido-make-buffer-list-hook saved-ido-make-buffer-list-hook))
    (error (progn
             (setq ido-make-buffer-list-hook saved-ido-make-buffer-list-hook)
             (helm-keyboard-quit)))))

(defun lx/switch-to-project-or-all-buffer ()
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-switch-to-buffer)
    (lx/switch-to-buffer)))
