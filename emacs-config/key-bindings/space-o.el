(spacemacs/declare-prefix "o" "user key bindings")
(spacemacs/declare-prefix "oo" "open org files")
(spacemacs/declare-prefix "oi" "open info nodes")
(spacemacs/declare-prefix "oe" "open demo files")
(spacemacs/declare-prefix "op" "open project files")
(spacemacs/declare-prefix "oc" "open config files")

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

(defun lx/edit-tmux-config ()
  (interactive)
  (find-file "~/.tmux.conf"))

(defun lx/edit-vimrc ()
  (interactive)
  (find-file "~/.vimrc"))

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

  ;; Config files
  "och" #'lx/edit-etc-hosts
  "ocp" #'lx/edit-pryrc
  "ocz" #'lx/edit-zshrc
  "ocd" #'lx/edit-ssh-dialog-config
  "ocs" #'lx/edit-ssh-config
  "oct" #'lx/edit-tmux-config
  "ocv" #'lx/edit-vim-config
  )
