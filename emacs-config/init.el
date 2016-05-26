(dolist (dir '(
               "funcs"
               "advices"
               "post-package-load"
               "key-bindings"))
  (dolist (el (directory-files (format "~/.config/emacs-config/%s" dir) t "\.el$"))
    (load-file el)))
