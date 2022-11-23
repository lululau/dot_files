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
    (call-interactively (if (eq major-mode 'cider-repl-mode) 'cider-repl-next-input 'next-line))))

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
  (cond ((lx/is-remote-buffer) (helm-remote-buffers))
        ((projectile-project-p) (helm-projectile-switch-to-buffer))
        (t (lx/switch-to-buffer))))

(defun lx/is-remote-buffer ()
  (or (eq 'ssh-zsh-vterm-mode major-mode)
      (string-prefix-p "/scp:" default-directory)
      (string-prefix-p "/ssh:" default-directory)))

(defun lx/get-remote-buffer-host ()
  (cond ((eq 'ssh-zsh-vterm-mode major-mode) (plist-get ssh-zsh-vterm-ssh-options :host))
        ((or (string-prefix-p "/scp:" default-directory) (string-prefix-p "/ssh:" default-directory))
         (seq--elt-safe (split-string default-directory ":") 1))))
