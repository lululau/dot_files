(defvar copilot--hooks-added nil)

(defun copilot-ensure-hooks ()
  (unless copilot--hooks-added
    (add-hook 'prog-mode-hook 'copilot-mode)
    (add-hook 'text-mode-hook 'copilot-mode)
    (add-hook 'clutch-mode-hook (lambda () (copilot-mode -1)))
    (setq copilot--hooks-added t)))

(defun copilot-ensure-and-complete ()
  (interactive)
  (require 'copilot)
  (copilot-ensure-hooks)
  (when (and (derived-mode-p 'prog-mode 'text-mode)
             (not copilot-mode))
    (copilot-mode 1))
  (copilot-complete))
