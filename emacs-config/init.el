(dolist (dir '(
               "funcs"
               "advices"
               "aliases"
               "package-hooks"))
  (dolist (el (directory-files (format "%s/%s" (file-name-directory load-file-name) dir) t "\.el$"))
    (load-file el)))
