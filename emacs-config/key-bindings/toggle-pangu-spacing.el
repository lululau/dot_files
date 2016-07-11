(defun lx/toggle-pangu-space-mode()
  (interactive)
  (if (bound-and-true-p pangu-spacing-mode)
      (pangu-spacing-mode -1)
    (pangu-spacing-mode +1)))

(defun lx/toggle-pangu-space-mode-globally()
  (interactive)
  (if (bound-and-true-p global-pangu-spacing-mode)
      (global-pangu-spacing-mode -1)
    (global-pangu-spacing-mode +1)))

(spacemacs/set-leader-keys "tG" 'lx/toggle-pangu-space-mode "t C-g" 'lx/toggle-pangu-space-mode-globally)
