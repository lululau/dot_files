;; -*- lexical-binding: t; -*-

;;;###autoload
(defun lx/keyboard-quit ()
  (interactive)
  (spacemacs/evil-search-clear-highlight) (copilot-clear-overlay) (keyboard-quit))

;;;###autoload
(defun lx/tab (arg)
  (interactive "P")
  (or (copilot-accept-completion) (indent-for-tab-command arg)))

;;;###autoload
(defun lx/reset-hybrid-state-cursor-type-after-tab ()
  (interactive)
  (when (and (eq 'lx/tab this-command) (eq 'hybrid evil-state))
    (setq cursor-type '(bar . 2))
    (set-cursor-color "SkyBlue2")))

;;;###autoload
(defun lx/copilot-accept-or-forward-word (arg)
  (interactive "P")
  (if (and (bound-and-true-p copilot--overlay) (copilot--overlay-visible))
      (copilot-accept-completion-by-word 1)
    (call-interactively 'forward-word)))

;;;###autoload
(defun lx/copilot-accept-or-next-line (arg)
  (interactive "P")
  (if (and (bound-and-true-p copilot--overlay) (copilot--overlay-visible))
      (copilot-accept-completion-by-line 1)
    (call-interactively (if (eq major-mode 'cider-repl-mode) 'cider-repl-next-input 'next-line))))

;;;###autoload
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

;;;###autoload
(defun lx/switch-to-project-or-all-buffer ()
  (interactive)
  (cond ((lx/is-remote-buffer) (helm-remote-buffers))
        ((projectile-project-p) (helm-projectile-switch-to-buffer))
        (t (lx/switch-to-buffer))))

;;;###autoload
(defun lx/is-remote-buffer ()
  (or (eq 'ssh-zsh-ghostel-mode major-mode)
      (string-prefix-p "/scp:" default-directory)
      (string-prefix-p "/ssh:" default-directory)))

;;;###autoload
(defun lx/get-remote-buffer-host ()
  (cond ((eq 'ssh-zsh-ghostel-mode major-mode) (plist-get ssh-zsh-ghostel-ssh-options :host))
        ((or (string-prefix-p "/scp:" default-directory) (string-prefix-p "/ssh:" default-directory))
         (seq--elt-safe (split-string default-directory ":") 1))))

;;;###autoload
(defun lx/make-frame ()
  "Create a new frame"
  (interactive)
  (make-frame-command))

;;;###autoload
(defun lx/switch-to-project-3rd-buffer ()
  "Switch to the 3rd buffer in current project"
  (interactive)
  (switch-to-buffer (nth 2 (projectile-project-buffers))))

;;;###autoload
(defun lx/save-buffer ()
  "Call C-x C-s interactively"
  (interactive)
  (call-interactively (key-binding "\C-x\C-s")))

;;;###autoload
(defun lx/open-emacs-init-file ()
  "Open Emacs init.el"
  (interactive)
  (find-file (format "%sinit.el" user-emacs-directory)))

;;;###autoload
(defun lx/update-packages-and-restart ()
  "Update Spacemacs packages without confirmation and restart Emacs only if updates were found.
Handles network and checkout errors gracefully by aborting the restart."
  (interactive)
  (require 'cl-lib)
  (condition-case err
      (let* ((distant-packages (configuration-layer//filter-distant-packages
                                configuration-layer--used-packages t))
             (update-packages (configuration-layer//get-packages-to-update distant-packages))
             (upgrade-count (length (cl-set-difference update-packages dotspacemacs-frozen-packages))))
        (if (zerop upgrade-count)
            (message "All packages are up to date. No restart needed.")
          (configuration-layer/update-packages t)
          (cl-letf (((symbol-function 'save-buffers-kill-emacs) #'kill-emacs)
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (spacemacs/restart-emacs))))
    (error
     (message "Error updating packages: %s" (error-message-string err))
     (user-error "Package update failed: %s" (error-message-string err)))))


