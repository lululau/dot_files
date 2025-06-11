(let ((current-dir (file-name-directory load-file-name)))
  (load-file (format "%s/funcs/init.el" current-dir))
  (dolist (dir '(
                 "advices"
                 "aliases"
                 "package-hooks"))
    (dolist (el (directory-files (format "%s/%s" current-dir dir) t "\.el$"))
      (load-file el))))
