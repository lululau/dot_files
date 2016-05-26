(dolist (el (directory-files "~/.config/emacs-config/key-bindings/" t "\.el$"))
  (unless (string-match-p "init\.el$" el)
      (load el)))
