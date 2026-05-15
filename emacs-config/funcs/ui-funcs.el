;;;###autoload
(defun lx/neotree-find-or-toggle ()
  "Find project root in neotree, fallback to toggle"
  (interactive)
  (condition-case nil
      (neotree-find-project-root)
    (error (neotree-toggle))))

;;;###autoload
(defun lx/neotree-toggle ()
  "Toggle neotree sidebar"
  (interactive)
  (neotree-toggle))

;;;###autoload
(defun lx/mouse-scroll-down ()
  "Scroll down 1 line"
  (interactive)
  (scroll-down 1))

;;;###autoload
(defun lx/mouse-scroll-up ()
  "Scroll up 1 line"
  (interactive)
  (scroll-up 1))

;;;###autoload
(defun lx/regenerate-tags ()
  "Ensure ggtags-mode is active and regenerate projectile tags"
  (interactive)
  (unless (boundp 'ggtags-mode) (ggtags-mode))
  (projectile-regenerate-tags))

;;;###autoload
(defun lx/enable-imenu-list ()
  "Enable imenu-list minor mode"
  (interactive)
  (imenu-list-minor-mode 1))

;;;###autoload
(defun lx/toggle-annotate-mode ()
  "Toggle annotate mode"
  (interactive)
  (if (bound-and-true-p annotate-mode) (annotate-mode -1) (annotate-mode)))

;;;###autoload
(defun lx/set-default-font ()
  "Set default monospaced font to 14/16"
  (interactive)
  (lx/set-monospaced-font "SauceCodePro Nerd Font Mono" "黑体-简" 14 16 14 16))

;;;###autoload
(defun lx/unhighlight-all ()
  "Unhighlight all regexps"
  (interactive)
  (unhighlight-regexp t))
