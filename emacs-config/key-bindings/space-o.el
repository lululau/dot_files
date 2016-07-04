(spacemacs/declare-prefix "o" "user key bindings")
(spacemacs/declare-prefix "oo" "open org files")
(spacemacs/declare-prefix "oi" "open info nodes")
(spacemacs/declare-prefix "oe" "open demo files")

(defun lx/open-bookmarks.org ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org"))

(defun lx/open-notes.org ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/notes.org"))

(defun lx/open-rails-guides.org ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/rails-guides-org/rails-guides-index.org"))

(defun lx/open-http-demo ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/demo/demo.http"))

(defun lx/open-ruby-demo ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/demo/demo.rb"))

(defun lx/open-perl-demo ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/demo/demo.pl"))

(defun lx/open-python-demo ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/demo/demo.py"))

(defun lx/open-java-demo ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/demo/Demo.java"))

(defun lx/open-shell-demo ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/demo/demo.sh"))

(defun lx/open-elisp-demo ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/demo/demo.el"))

(defun lx/open-org-demo ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/demo/demo.org"))

(defun lx/open-coffee-demo ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/demo/demo.coffee"))

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

(spacemacs/set-leader-keys

  ;; Org
  "oob" #'lx/open-bookmarks.org
  "oon" #'lx/open-notes.org
  "oor" #'lx/open-rails-guides.org

  ;; Demo files
  "oeh" #'lx/open-http-demo
  "oel" #'lx/open-elisp-demo
  "oer" #'lx/open-ruby-demo
  "oey" #'lx/open-python-demo
  "oej" #'lx/open-java-demo
  "oes" #'lx/open-shell-demo
  "oep" #'lx/open-perl-demo
  "oeo" #'lx/open-org-demo
  "oec" #'lx/open-coffee-demo

  ;; Info bookmarks
  "oie" #'lx/open-emacs-info
  "oil" #'lx/open-elisp-info
  "oio" #'lx/open-org-info
  "oim" #'lx/open-mu4e-info
  "oig" #'lx/open-magit-info
  "oiv" #'lx/open-evil-info)
