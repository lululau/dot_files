;;;###autoload
(defun lx/remember-previous-persp (&rest args)
  (setq lx/previous-persp (get-current-persp)))

;;;###autoload
(defun lx/switch-to-previous-perp ()
  (interactive)
  (if (and (boundp 'lx/previous-persp) (> (length (persp-names)) 1))
      (persp-switch (if lx/previous-persp (persp-name lx/previous-persp) "Default"))))
