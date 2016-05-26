(mapc 'load-file (directory-files "~/.config/emacs-config/advices" t "\.el$"))
(mapc 'load-file (directory-files "~/.config/emacs-config/post-package-load" t "\.el$"))
