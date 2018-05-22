;;;###autoload
(defun lx/swap-list-elem (list a b)
  (let* ((elem_a (nth a list))
         (elem_b (nth b list)))
    (setcar (nthcdr a list) elem_b)
    (setcar (nthcdr b list) elem_a)))

;;;###autoload
(defun lx/next-list-index (list idx)
  (if (= (1+ idx) (length list))
      0
    (1+ idx)))

;;;###autoload
(defun lx/previous-list-index (list idx)
  (if (= 0 idx)
      (- (length list) 1)
    (- idx 1)))

;;;###autoload
(defun lx/current-layout-index ()
  (-elem-index (spacemacs//current-layout-name) (persp-names-current-frame-fast-ordered)))

;;;###autoload
(defun lx/move-layout-forward ()
  (interactive)
  (let* ((current (lx/current-layout-index))
         (next (lx/next-list-index (persp-names-current-frame-fast-ordered) current)))
    (lx/swap-list-elem persp-names-cache current next)
    (lx/swap-list-elem (cdr (cdddr persp-minor-mode-menu)) current next))
  (spacemacs/layouts-transient-state/body))

;;;###autoload
(defun lx/move-layout-backward ()
  (interactive)
  (let* ((current (lx/current-layout-index))
         (previous (lx/previous-list-index (persp-names-current-frame-fast-ordered) current)))
    (lx/swap-list-elem (cdr (cdddr persp-minor-mode-menu)) current previous)
    (lx/swap-list-elem persp-names-cache current previous))
  (spacemacs/layouts-transient-state/body))

;;;###autoload
(defun lx/helm-persp-replace-project (arg)
  (interactive "P")
  (let ((current-index (lx/current-layout-index)))
    (spacemacs/layouts-ts-close)
    (call-interactively 'spacemacs/helm-persp-switch-project)
    (while (not (eq current-index (lx/current-layout-index)))
      (lx/move-layout-backward))
    (keyboard-quit)))

;;;###autoload
(defun lx/persp-swith-to-buffer-project ()
  (interactive)
  (save-window-excursion
    (let ((current-index (lx/current-layout-index)))
      (spacemacs/layouts-ts-close)
      (persp-switch (projectile-project-root))
      (while (not (eq current-index (lx/current-layout-index)))
        (lx/move-layout-backward))
      (keyboard-quit))))
