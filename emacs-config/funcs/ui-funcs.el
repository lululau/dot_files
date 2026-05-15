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
