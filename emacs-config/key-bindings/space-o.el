(spacemacs/declare-prefix "o" "user key bindings")
(spacemacs/declare-prefix "oo" "open org files")
(spacemacs/declare-prefix "oi" "open info nodes")
(spacemacs/declare-prefix "oe" "open demo files")
(spacemacs/declare-prefix "op" "open project files")
(spacemacs/declare-prefix "oc" "open config files")
(spacemacs/declare-prefix "ol" "load libraries")
(spacemacs/declare-prefix "od" "open most used directories")
(spacemacs/declare-prefix "ob" "Set ibuffer group")

;; Common used directories
(setq lx/dirs '(("oda" applications-dir "/Applications/")
                ("odb" bin-dir "~/bin/")
                ("odC" cafe-dir "~/Cafe/")
                ("odc" cascode-dir "~/cascode/")
                ("odd" downloads-dir "~/Downloads/")
                ("odD" documents-dir "~/Documents/")
                ("odg" github-dir "~/cascode/github.com/")
                ("odu" user-dir "~/")
                ("odh" home-dir "/home/")
                ("odi" images-dir "~/Library/Mobile Documents/com~apple~CloudDocs/images/")
                ("odk" kt-dir "~/kt/")
                ("odm" movies-dir "~/Movies/")
                ("ods" scratch-dir "~/Documents/scratches/")
                ("odS" snips-dir "~/snips/")
                ("ode" emacs-dir "~/.emacs.d/")
                ("odt" tmp-dir "~/tmp/")))

;; Demo files
(setq lx/demo-files
      '(("oea" artist-demo  "~/Documents/org/demo/demo.art")
        ("oeA" scala-demo   "~/cascode/github.com/prog-scala/src/main/scala/lx/demo.scala")
        ("oeb" bigtxt-demo    "~/Documents/org/demo/big.txt")
        ("oeh" http-demo    "~/Documents/org/demo/demo-http.org")
        ("oel" elisp-demo   "~/Documents/org/demo/demo.el")
        ("oer" ruby-demo    "~/Documents/org/demo/demo.rb")
        ("oeP" python-demo  "~/Documents/org/demo/demo.py")
        ("oeJ" java-demo    "~/cascode/java/maven/simple/src/main/java/org/sonatype/mavenbook/App.java")
        ("oej" js-demo      "~/Documents/org/demo/demo.js")
        ("oes" shell-demo   "~/Documents/org/demo/demo.sh")
        ("oeS" swift-demo   "~/Documents/org/demo/demo.swift")
        ("oep" perl-demo    "~/Documents/org/demo/demo.pl")
        ("oeo" org-demo     "~/Documents/org/demo/demo.org")
        ("oeC" coffee-demo  "~/Documents/org/demo/demo.coffee")
        ("oey" yaml-demo    "~/Documents/org/demo/demo.yaml")
        ("oeH" html-demo    "~/Documents/org/demo/demo.html")
        ("oec" c-demo       "~/Documents/org/demo/demo.c")
        ("oeg" go-demo      "~/.go/src/demo/demo.go")
        ("oet" txt-demo     "~/Documents/org/demo/demo.txt")))

;; Config files
(setq lx/config-files
      '(("och" hosts-config "/sudo:root@localhost:/etc/hosts")
        ("ocz" zshrc "~/.zshrc")
        ("ocp" pryrc "~/.pryrc")
        ("ocd" ssh-dialog-config "~/.config/ssh-dialog.yml")
        ("ocs" ssh-config "~/.ssh/config")
        ("ocA" authorized_keys "~/.ssh/authorized_keys")
        ("oct" tmux-conf "~/.tmux.conf")
        ("ocv" vimrc "~/.vimrc")))

;; Org files
(setq lx/org-files
      '(("oob" bookmarks-org "~/Documents/org/bookmarks.org")
        ("oon" notes-org "~/Documents/org/notes.org")
        ("oop" projects-org "~/Documents/org/projects.org")
        ("oor" rails-guides-org "~/Documents/org/rails-guides-org/rails-guides-index.org")))

;; Cheat Sheets
(setq lx/cheatsheets
      '(("osr" emacs-regexp-cheatsheet "~/Documents/org/cheatsheets/emacs-regexp-cheatsheets.org")
        ("oso" org-mode-cheatsheet     "~/Documents/org/cheatsheets/org-mode-cheatsheets.org")))

(defmacro lx/make-open-file-function (name dir)
  `(defun ,(intern (format "lx/open-file-%s" name)) ()
     (interactive)
     (find-file ,dir)))

(let ((result '()))
  (dolist (elem (append lx/dirs lx/demo-files lx/config-files lx/org-files lx/cheatsheets) result)
    (let ((kbd (nth 0 elem))
          (func-name (nth 1 elem))
          (dir (nth 2 elem)))
      (eval `(lx/make-open-file-function ,func-name ,dir))
      (add-to-list 'result kbd t)
      (add-to-list 'result (intern (format "lx/open-file-%s" func-name)) t)))
  (apply 'spacemacs/set-leader-keys result))

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

(defun lx/load-ox-gfm ()
  (interactive)
  (load-library "ox-gfm"))

(defun lx/load-ox-reveal ()
  (interactive)
  (load-library "ox-reveal"))

(defun lx/load-ox-twbs ()
  (interactive)
  (load-library "ox-twbs"))

(defun lx/load-ob-ditaa ()
  (interactive)
  (load-library "ob-ditaa"))

(defun lx/load-ob-calc ()
  (interactive)
  (load-library "ob-calc"))

(defun lx/set-ibuffer-group-buffers-by-projects ()
  (setq ibuffer-group-buffers-by 'projects))

(defun lx/set-ibuffer-group-buffers-by-modes ()
  (setq ibuffer-group-buffers-by 'modes))

(defun lx/set-ibuffer-group-buffers-by-nil ()
  (setq ibuffer-group-buffers-by nil))

(spacemacs/set-leader-keys

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

  ;; load libraries
  "oll" #'load-library
  "olg" #'lx/load-ox-gfm
  "olr" #'lx/load-ox-reveal
  "olb" #'lx/load-ox-twbs
  "old" #'lx/load-ob-ditaa
  "olc" #'lx/load-ob-calc

  ;; Set ibuffer group by
  "obp" #'lx/set-ibuffer-group-buffers-by-projects
  "obm" #'lx/set-ibuffer-group-buffers-by-modes
  "obn" #'lx/set-ibuffer-group-buffers-by-nil
  )
