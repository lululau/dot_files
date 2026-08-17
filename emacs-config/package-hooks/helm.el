;; -*- lexical-binding: t; -*-

(defun lx/helm-disable-onewindow-in-side-window ()
  "Force `helm-onewindow-p' off when the helm window is a side window.

Spacemacs displays helm buffers in a bottom side window (see
`spacemacs-helm-display-buffer-regexp').  When completion-in-region is
triggered from inside a minibuffer (e.g. evil-ex \":!cmd<TAB>\"),
`helm--completion-in-region' starts a nested helm session whose window
ends up being that side window while the global `helm-onewindow-p' is
left non-nil.  The idle update timer then runs `helm-update', which
does `(with-helm-window (delete-other-windows))' whenever
`helm-onewindow-p' is non-nil.  A side window can never be the only
window, so Emacs signals \"Cannot make side window the only window\";
because this happens inside the timer it surfaces as \"Error running
timer\" and aborts filtering of typed input.

A side window is structurally never the sole window, so disabling
`helm-onewindow-p' here is always correct and only affects side-window
sessions."
  (let ((win (and (fboundp 'helm-window) (helm-window))))
    (when (and (window-live-p win) (window-parameter win 'window-side))
      (setq helm-onewindow-p nil))))

(defvar lx/helm-imenu-target-buffer nil
  "Buffer captured at `SPC j i' invocation time.
Used by `lx/helm-imenu-candidates-advice' to bypass Helm's stale
`helm-current-buffer' resolution, which often points to a leftover
dired or helm buffer instead of the user's actual document.")

(defun lx/spacemacs-helm-jump-in-buffer-advice (orig-fn &rest args)
  "Capture current buffer and route to the correct helm command.
Binds `lx/helm-imenu-target-buffer' so that `helm-imenu-candidates'
always indexes the buffer the user was actually looking at, regardless
of what `helm-initial-setup' does to `helm-current-buffer'."
  (let ((lx/helm-imenu-target-buffer (current-buffer)))
    (cond
     ((eq major-mode 'org-mode)
      (call-interactively 'helm-org-in-buffer-headings))
     ((and (fboundp 'semantic-active-p) (semantic-active-p))
      (call-interactively 'helm-semantic-or-imenu))
     (t
      (call-interactively 'helm-imenu)))))

(defun lx/helm-imenu-candidates-advice (orig-fn &optional buffer)
  "Run `helm-imenu-candidates' in the user's actual document buffer.
During a Helm session `current-buffer' is the Helm buffer and
`helm-current-buffer' may point to a stale dired window.  This
advice uses `lx/helm-imenu-target-buffer' (set at `SPC j i' time)
as the authoritative target."
  (let ((target (or buffer lx/helm-imenu-target-buffer)))
    (if (and target (buffer-live-p target))
        (with-current-buffer target
          (let ((helm-current-buffer target))
            (condition-case nil
                (funcall orig-fn target)
              (imenu-unavailable nil)
              (error nil))))
      (funcall orig-fn buffer))))

(with-eval-after-load 'helm-imenu
  (advice-add 'helm-imenu-candidates :around #'lx/helm-imenu-candidates-advice))

(with-eval-after-load 'helm
  (advice-add 'spacemacs/helm-jump-in-buffer :around #'lx/spacemacs-helm-jump-in-buffer-advice))

(spacemacs|use-package-add-hook helm
  :post-config
  (advice-add 'helm-imenu-candidates :around #'lx/helm-imenu-candidates-advice)
  (advice-add 'spacemacs/helm-jump-in-buffer :around #'lx/spacemacs-helm-jump-in-buffer-advice)
  (define-key helm-map (kbd "s-m") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "s-l") 'avy-jump-helm-line)
  (define-key helm-map (kbd "s-j") #'(lambda () (interactive) (helm-next-line 5)))
  (define-key helm-map (kbd "s-k") #'(lambda () (interactive) (helm-previous-line 5)))
  (add-hook 'helm-minibuffer-set-up-hook
            #'lx/helm-disable-onewindow-in-side-window))
