(dolist (el (directory-files "~/.config/emacs-config/text-objects/" t "\.el$"))
  (unless (string-match-p "init\.el$" el)
      (load el)))
