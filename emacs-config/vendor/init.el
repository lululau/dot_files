(dolist (el (directory-files (file-name-directory load-file-name) t "\.el$"))
  (unless (string-match-p "init\.el$" el)
      (load el)))
