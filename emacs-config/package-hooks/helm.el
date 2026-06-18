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

(spacemacs|use-package-add-hook helm
  :post-config
  (define-key helm-map (kbd "s-m") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "s-l") 'avy-jump-helm-line)
  (define-key helm-map (kbd "s-j") #'(lambda () (interactive) (helm-next-line 5)))
  (define-key helm-map (kbd "s-k") #'(lambda () (interactive) (helm-previous-line 5)))
  (add-hook 'helm-minibuffer-set-up-hook
            #'lx/helm-disable-onewindow-in-side-window))
