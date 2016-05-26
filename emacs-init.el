(load-file "~/.config/emacs-advices.el")
(mapc 'load-file (directory-files "~/.config/emacs-pkg-post-load-conf" t "\.el$"))
