(dolist (dir '("advices"
               "aliases"
               "package-hooks"))
  (dolist (el (directory-files (format "%s/%s" (file-name-directory load-file-name) dir) t "\.el$"))
    (load-file el)))

(add-to-load-path (format "%s%s" (file-name-directory load-file-name) "funcs"))
(require 'custom-func-init)

(add-hook 'persp-before-deactivate-functions 'lx/remember-previous-persp)
(lx/def-window-frame-switch-function 'up)
(lx/def-window-frame-switch-function 'down)
