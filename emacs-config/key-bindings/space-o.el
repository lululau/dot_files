(spacemacs/declare-prefix "o" "user key bindings")
(spacemacs/declare-prefix "oo" "open org files")
(spacemacs/declare-prefix "oi" "open info nodes")
(spacemacs/declare-prefix "oe" "open demo files")
(spacemacs/declare-prefix "op" "open project files")
(spacemacs/declare-prefix "oc" "open config files")
(spacemacs/declare-prefix "ol" "load libraries")

(spacemacs/declare-prefix "ob" "Set ibuffer group")

(defun lx/open-bookmarks.org ()
  (interactive)
  (find-file "~/Documents/org/bookmarks.org"))

(defun lx/open-notes.org ()
  (interactive)
  (find-file "~/Documents/org/notes.org"))

(defun lx/open-rails-guides.org ()
  (interactive)
  (find-file "~/Documents/org/rails-guides-org/rails-guides-index.org"))

(defun lx/open-http-demo ()
  (interactive)
  (find-file "~/Documents/org/demo/demo.http"))

(defun lx/open-ruby-demo ()
  (interactive)
  (find-file "~/Documents/org/demo/demo.rb"))

(defun lx/open-perl-demo ()
  (interactive)
  (find-file "~/Documents/org/demo/demo.pl"))

(defun lx/open-python-demo ()
  (interactive)
  (find-file "~/Documents/org/demo/demo.py"))

(defun lx/open-java-demo ()
  (interactive)
  (find-file "~/Documents/org/demo/Demo.java"))

(defun lx/open-shell-demo ()
  (interactive)
  (find-file "~/Documents/org/demo/demo.sh"))

(defun lx/open-elisp-demo ()
  (interactive)
  (find-file "~/Documents/org/demo/demo.el"))

(defun lx/open-org-demo ()
  (interactive)
  (find-file "~/Documents/org/demo/demo.org"))

(defun lx/open-coffee-demo ()
  (interactive)
  (find-file "~/Documents/org/demo/demo.coffee"))

(defun lx/open-yaml-demo ()
  (interactive)
  (find-file "~/Documents/org/demo/demo.yaml"))

(defun lx/open-c-demo ()
  (interactive)
  (find-file "~/tmp/demo.c"))

(defun lx/open-txt-demo ()
  (interactive)
  (find-file "~/tmp/demo.txt"))

(defun lx/open-html-demo ()
  (interactive)
  (find-file "~/Documents/org/demo/demo.html"))

(defun lx/open-emacs-info ()
  (interactive)
  (org-open-link-from-string "info:emacs#Top"))

(defun lx/open-elisp-info ()
  (interactive)
  (org-open-link-from-string "info:elisp#Top"))

(defun lx/open-org-info ()
  (interactive)
  (org-open-link-from-string "info:org#Top"))

(defun lx/open-mu4e-info ()
  (interactive)
  (org-open-link-from-string "info:mu4e#Top"))

(defun lx/open-magit-info ()
  (interactive)
  (org-open-link-from-string "info:magit#Top"))

(defun lx/open-evil-info ()
  (interactive)
  (org-open-link-from-string "info:evil#Top"))

(defun lx/edit-etc-hosts ()
  (interactive)
  (find-file "/sudo:root@localhost:/etc/hosts"))

(defun lx/edit-zshrc ()
  (interactive)
  (find-file "~/.zshrc"))

(defun lx/edit-pryrc ()
  (interactive)
  (find-file "~/.pryrc"))

(defun lx/edit-ssh-dialog-config ()
  (interactive)
  (find-file "~/.config/ssh-dialog.yml"))

(defun lx/edit-ssh-config ()
  (interactive)
  (find-file "~/.ssh/config"))

(defun lx/edit-authorized-keys ()
  (interactive)
  (find-file "~/.ssh/authorized_keys"))

(defun lx/edit-tmux-config ()
  (interactive)
  (find-file "~/.tmux.conf"))

(defun lx/edit-vimrc ()
  (interactive)
  (find-file "~/.vimrc"))

(defun lx/load-ox-gfm ()
  (interactive)
  (load-library "ox-gfm"))

(defun lx/load-ox-reveal ()
  (interactive)
  (load-library "ox-reveal"))

(defun lx/load-ox-twbs ()
  (interactive)
  (load-library "ox-twbs"))

(spacemacs/set-leader-keys

  ;; Org
  "oob" #'lx/open-bookmarks.org
  "oon" #'lx/open-notes.org
  "oor" #'lx/open-rails-guides.org

  ;; Demo files
  "oeh" #'lx/open-http-demo
  "oel" #'lx/open-elisp-demo
  "oer" #'lx/open-ruby-demo
  "oeP" #'lx/open-python-demo
  "oej" #'lx/open-java-demo
  "oes" #'lx/open-shell-demo
  "oep" #'lx/open-perl-demo
  "oeo" #'lx/open-org-demo
  "oec" #'lx/open-coffee-demo
  "oey" #'lx/open-yaml-demo
  "oeH" #'lx/open-html-demo
  "oeC" #'lx/open-c-demo
  "oet" #'lx/open-txt-demo

  ;; Info bookmarks
  "oie" #'lx/open-emacs-info
  "oil" #'lx/open-elisp-info
  "oio" #'lx/open-org-info
  "oim" #'lx/open-mu4e-info
  "oig" #'lx/open-magit-info
  "oiv" #'lx/open-evil-info

  ;; project tmp files
  "ops" #'lx/find-or-create-projectile-snippet-file
  "oph" #'lx/find-or-create-projectile-request-file
  "opo" #'lx/find-or-create-projectile-snippet-org

  ;; Config files
  "och" #'lx/edit-etc-hosts
  "ocp" #'lx/edit-pryrc
  "ocz" #'lx/edit-zshrc
  "ocd" #'lx/edit-ssh-dialog-config
  "ocs" #'lx/edit-ssh-config
  "ocA" #'lx/edit-authorized-keys
  "oct" #'lx/edit-tmux-config
  "ocv" #'lx/edit-vim-config

  ;; load libraries
  "oll" #'load-library
  "olg" #'lx/load-ox-gfm
  "olr" #'lx/load-ox-reveal
  "olb" #'lx/load-ox-twbs

  ;; Set ibuffer group

  "obp" #'(lambda () (interactive) (setq ibuffer-group-buffers-by 'projects))
  "obm" #'(lambda () (interactive) (setq ibuffer-group-buffers-by 'modes))
  "obn" #'(lambda () (interactive) (setq ibuffer-group-buffers-by nil))
  )
