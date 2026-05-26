;; -*- lexical-binding: t; -*-

(unless (fboundp 'lx/startup-profile-mark)
  (load-file (expand-file-name "funcs/startup-profile.el"
                               (file-name-directory load-file-name))))

(let ((current-dir (file-name-directory load-file-name)))
  (lx/startup-profile-mark "emacs-config-init-begin")
  (lx/startup-profile-time "emacs-config/funcs-init"
                           (lambda () (load-file (format "%s/funcs/init.el" current-dir))))
  (dolist (dir '("advices" "aliases" "package-hooks"))
    (lx/startup-profile-load-files (format "emacs-config/%s" dir)
                                   (format "%s/%s" current-dir dir)))
  (lx/startup-profile-mark "emacs-config-init-end"))
