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
(setq lx/dirs `(("odA" applications-dir ("/Applications/" "/server"))
                ("odb" blog-dir ("~/blog/"))
                ("odB" bin-dir ("~/bin/"))
                ("odC" cafe-dir ("~/Cafe/"))
                ("odc" config-dir ("~/.config/"))
                ("od C-c" cascode-dir ("~/cascode/"))
                ("odd" downloads-dir ("~/Downloads/"))
                ("odD" documents-dir ("~/Documents/" "/data"))
                ("odf" fzf-dir ("~/.fzf/"))
                ("odg" github-dir ("~/cascode/github.com/"))
                ("odH" home-dir ("/home"))
                ("odh" user-dir ("~/"))
                ("odi" icloud-dir ("~/Library/Mobile Documents/com~apple~CloudDocs/"))
                ("odk" kt-dir ("~/kt/"))
                ("odl" las-dir ("~/Library/Application Support/" "/var/run/log"))
                ("odL" lp-dir ("~/Library/Preferences/" "/data/logs"))
                ("odM" movies-dir ("~/Movies/"))
                ("odm" materials-dir ("~/Documents/materials/"))
                ("ods" scratch-dir ("~/Documents/materials/scratches/"))
                ("odS" snips-dir ("~/snips/"))
                ("od C-s" snippets-dir ("~/Documents/materials/snippets"))
                ("ode" emacs-dir (,user-emacs-directory))
                ("odz" spacezsh-dir ("~/.spacezsh/"))
                ("odZ" ohmyzsh-dir ("~/.oh-my-zsh/"))
                ("od/" root-dir ("/"))
                ("odo" org-dir ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org"))
                ("odj" org-journal-dir ("~/Documents/materials/journal"))
                ("odJ" org-jira-dir ("~/Documents/materials/jira"))
                ("odn" notes-dir ("~/Documents/materials/notes"))
                ("odN" evernotes-dir ("~/Documents/evernotes"))
                ("odw" webclips-dir ("~/Documents/materials/webclips"))
                ("odv" volumes-dir ("/Volumes"))
                ("odT" tmux-dir ("~/.tmux"))
                ("odt" tmp-dir ("~/tmp/" "~/liuxiang/tmp" "~/lx/tmp/"))))

;; Demo files
(setq lx/demo-files
      '(("oea" artist-demo ("~/Documents/materials/demo/demo.art"))
        ("oeA" scala-demo ("~/cascode/github.com/prog-scala/src/main/scala/lx/demo.scala"))
        ("oeb" bigtxt-demo ("~/Documents/materials/demo/big.txt"))
        ("oeB" spring-boot-init-list ("~/Documents/materials/demo/spring-boot-init-list.org"))
        ("oeh" http-demo ("~/Documents/materials/demo/demo-http.org"))
        ("oel" elisp-demo ("~/Documents/materials/demo/demo.el"))
        ("oer" ruby-demo ("~/Documents/materials/demo/demo.rb"))
        ("oeP" python-demo ("~/Documents/materials/demo/demo.py"))
        ("oeJ" java-demo ("~/cascode/java/maven/simple/src/main/java/org/sonatype/mavenbook/App.java"))
        ("oej" js-demo ("~/Documents/materials/demo/demo.js"))
        ("oes" shell-demo ("~/Documents/materials/demo/demo.sh"))
        ("oeS" swift-demo ("~/Documents/materials/demo/demo.swift"))
        ("oep" perl-demo ("~/Documents/materials/demo/demo.pl"))
        ("oeo" org-demo ("~/Documents/materials/demo/demo.org"))
        ("oem" markdown-demo ("~/Documents/materials/demo/demo.md"))
        ("oeC" coffee-demo ("~/Documents/materials/demo/clojure/demo/src/demo/demo.clj"))
        ("oey" yaml-demo ("~/Documents/materials/demo/demo.yaml"))
        ("oeH" html-demo ("~/Documents/materials/demo/demo.html"))
        ("oec" c-demo ("~/Documents/materials/demo/demo.c"))
        ("oeg" go-demo ("~/cascode/go/src/demo/demo.go"))
        ("oeq" sql-demo ("~/Documents/materials/demo/demo.sql"))
        ("oeE" elixir-demo ("~/Documents/materials/demo/demo.exs"))
        ("oee" es-demo ("~/Documents/materials/demo/demo.es"))
        ("oek" haskell-demo ("~/Documents/materials/demo/demo.hs"))
        ("oet" txt-demo ("~/Documents/materials/demo/demo.txt"))))

;; Config files
(setq lx/config-files
      '(("och" hosts-config ("/sudo:root@localhost:/etc/hosts" "/etc/hosts"))
        ("ocS" sudoers ("/sudo:root@localhost:/etc/sudoers" "/etc/sudoers"))
        ("ocz" zshrc ("~/.zshrc"))
        ("ocp" pryrc ("~/.pryrc"))
        ("ocl" vrl ("~/.vrl.yml"))
        ("ocg" git ("~/.gitconfig"))
        ("ocd" docker-config ("~/.docker"))
        ("ocs" ssh-config ("~/.ssh/config"))
        ("ocA" authorized_keys ("~/.ssh/authorized_keys"))
        ("oca" ansible-conf-dir ("/etc/ansible"))
        ("oct" tmux-conf ("~/.tmux.conf"))
        ("ocJ" jenkins-builder ("~/.jenkins-builder.yaml"))
        ("ocj" ideavimrc ("~/.ideavimrc"))
        ("ocn" nginx ("/usr/local/etc/nginx" "/etc/nginx/conf.d" "/opt/nginx/conf"))
        ("ock" kubectl ("~/.kube/config"))
        ("ocK" k9s ("~/.k9s"))
        ("oc9" k9s2 ("~/.k9s"))
        ("ocq" arql ("~/.arql.d"))
        ("ocm" maven ("~/.m2/settings.xml"))
        ("ocv" vimrc ("~/.vimrc"))))

;; Org files
(setq lx/org-files
      '(("oob" bookmarks-org ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/bookmarks.org"))
        ("ood" tech-diary-org ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tech_diary.org"))
        ("ooh" homebrews-org ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/homebrews.org"))
        ("oop" projects-org ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/projects.org"))
        ("ool" learnings-org ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/learnings.org"))
        ("oof" life-org ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/life.org"))
        ("ooc" capture-org ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/capture.org"))
        ("ooC" cheatsheet-org ("~/Documents/materials/cheatsheets"))
        ("oot" team-tasks-org ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/team-tasks.org"))
        ("ook" kbd-macros-org ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/kbd-macros.org"))
        ("ooj" jira-org ("~/Documents/materials/jira/recent-issues.org"))
        ("oor" rails-guides-org ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/rails-guides-org/rails-guides-index.org"))))

(setq lx/server-files
      '(("os3" lcl-devb "/scp:lcl.devb:/etc/nginx/conf.d")
      ("os4" ceres-devb "/scp:ceres.devb:/etc/nginx/conf.d")
      ("os5" ll-devb "/scp:ll-devb:/etc/nginx/conf.d")
      ("os6" md-dev "/scp:md.dev:/etc/nginx/conf.d")
      ("os7" lcl-fe1 "/scp:lcl.fe1:/etc/nginx/conf.d")
      ("os8" lcl-fe2 "/scp:lcl.fe2:/etc/nginx/conf.d")
      ("os9" ceres-fe1 "/scp:ceres.fe1:/opt/nginx/conf/conf.d")
      ("osj" jsroot "/scp:jsroot:/data/monitor/prometheus")
      ("osJ" jenkins "/scp:jenkins:/data/jenkins_home/workspace/docker_file")
      ("os2" dev42 "/scp:dev42:~/")
      ("os1" xym "/scp:xym:~/")
      ("os0" lx-kt "/scp:lx.kt:~/")))

;; Cheat Sheets
(setq lx/cheatsheets
      '(("osr" emacs-regexp-cheatsheet ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/cheatsheets/emacs-regexp-cheatsheets.org"))
        ("oso" org-mode-cheatsheet     ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/cheatsheets/org-mode-cheatsheets.org"))))

(defmacro lx/make-open-file-function (name dir)
  `(defun ,(intern (format "lx/open-file-%s" name)) (arg)
     (interactive "P")
     ,(if (stringp dir)
         `(find-file ,dir)
       `(if (not arg)
           (let* ((pwd default-directory)
                  (is-remote (or (string-prefix-p "/scp:" pwd)
                                 (string-prefix-p "/ssh:" pwd)
                                 (eq major-mode 'ssh-zsh-vterm-mode)))
                  (remote-host (if is-remote
                                   (if (eq major-mode 'ssh-zsh-vterm-mode)
                                       (plist-get ssh-zsh-vterm-ssh-options :host)
                                     (seq--elt-safe (split-string pwd ":") 1))
                                  nil))
                  (file-prefix (if remote-host (format "/scp:%s:" remote-host) ""))
                  (files (mapcar (lambda (x) (concat file-prefix x)) ',dir))
                  (existing-file (seq-find (lambda (x) (file-exists-p x)) files)))
             (if (not is-remote)
                 (find-file ,(car dir))
               (find-file (or existing-file (car files)))))
          (let* ((existing-file (seq-find (lambda (x) (file-exists-p x)) ',dir)))
            (find-file (or existing-file (car files))))))))

(let ((result '()))
  (dolist (elem (append lx/dirs lx/demo-files lx/config-files lx/org-files lx/cheatsheets lx/server-files) result)
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
  (interactive)
  (setq ibuffer-group-buffers-by 'projects))

(defun lx/set-ibuffer-group-buffers-by-modes ()
  (interactive)
  (setq ibuffer-group-buffers-by 'modes))

(defun lx/set-ibuffer-group-buffers-by-nil ()
  (interactive)
  (setq ibuffer-group-buffers-by nil))

(defun lx/toggle-maigt-log-date-format ()
  (interactive)
  (let* ((current-value (nth 1 magit-log-margin))
         (new-value (if (eq current-value 'age)
                        "%F %T"
                      'age)))
    (setf (nth 1 magit-log-margin) new-value)
    (message "Set magit-log-date-format to `%s'" new-value)))

(defun lx/helm-remote-locations ()
  (interactive)
  (helm :prompt "Remote locations: "
        :buffer "*helm-remote-locaions*"
        :sources
        (list (helm-build-sync-source "Remote locations"
                :fuzzy-match  t
                :candidates  (list (cons (format "%-20s%s" "lx.kt" "~") "/scp:lx.kt:~/")
                              (cons (format "%-20s%s" "dev42" "~") "/scp:dev42:~")
                              (cons (format "%-20s%s" "dev42" "docker") "ssh:liuxiang@dev42|docker:liuxiang@container:/var/run/docker.sock")
                              (cons (format "%-20s%s" "lcl.devb" "/etc/nginx/conf.d") "/scp:lcl.devb:/etc/nginx/conf.d")
                              (cons (format "%-20s%s" "ceres.devb" "/etc/nginx/conf.d") "/scp:ceres.devb:/etc/nginx/conf.d")
                              (cons (format "%-20s%s" "ll.devb" "/etc/nginx/conf.d") "/scp:ll.devb:/etc/nginx/conf.d")
                              (cons (format "%-20s%s" "md.dev" "/etc/nginx/conf.d") "/scp:md.dev:/etc/nginx/conf.d")
                              (cons (format "%-20s%s" "lcl.fe1" "/etc/nginx/conf.d") "/scp:lcl.fe1:/etc/nginx/conf.d")
                              (cons (format "%-20s%s" "lcl.fe2" "/etc/nginx/conf.d") "/scp:lcl.fe2:/etc/nginx/conf.d")
                              (cons (format "%-20s%s" "ceres.fe1" "/opt/nginx/conf/conf.d") "/scp:ceres.fe1:/opt/nginx/conf/conf.d")
                              (cons (format "%-20s%s" "ceres.fe2" "/opt/nginx/conf/conf.d") "/scp:ceres.fe2:/opt/nginx/conf/conf.d")
                              (cons (format "%-20s%s" "jenkins2" "/data/jenkins_home/workspace/docker_file") "/scp:jenkins2:/data/jenkins_home/workspace/docker_file")
                              (cons (format "%-20s%s" "jenkins2" "/data/jenkins_home/workspace") "/scp:jenkins2:/data/jenkins_home/workspace")
                              (cons (format "%-20s%s" "ceres.api1" "~/") "/scp:ceres.api1:~/")
                              (cons (format "%-20s%s" "ceres.api2" "~/") "/scp:ceres.api2:~/")
                              (cons (format "%-20s%s" "ceres.job1" "~/") "/scp:ceres.job1:~/")
                              (cons (format "%-20s%s" "ceres.job2" "~/") "/scp:ceres.job2:~/")
                              (cons (format "%-20s%s" "ceres.job3" "~/") "/scp:ceres.job3:~/")
                              (cons (format "%-20s%s" "ceres.mq" "~/") "/scp:ceres.mq:~/")
                              (cons (format "%-20s%s" "elk2" "~/") "/scp:elk2:~/")
                              (cons (format "%-20s%s" "gitlab.ktjr" "~/") "/scp:gitlab.ktjr:~/")
                              (cons (format "%-20s%s" "jira" "~/") "/scp:jira:~/")
                              (cons (format "%-20s%s" "kapi1" "~/") "/scp:kapi1:~/")
                              (cons (format "%-20s%s" "kapi2" "~/") "/scp:kapi2:~/")
                              (cons (format "%-20s%s" "kapi3" "~/") "/scp:kapi3:~/")
                              (cons (format "%-20s%s" "khcpro" "~/") "/scp:khcpro:~/")
                              (cons (format "%-20s%s" "kt01" "/etc/nginx/sites-enabled") "/scp:kt01:/etc/nginx/sites-enabled")
                              (cons (format "%-20s%s" "kt02" "/etc/nginx/sites-enabled") "/scp:kt02:/etc/nginx/sites-enabled")
                              (cons (format "%-20s%s" "kt03" "/etc/nginx/sites-enabled") "/scp:kt03:/etc/nginx/sites-enabled")
                              (cons (format "%-20s%s" "kt04" "/etc/nginx/sites-enabled") "/scp:kt04:/etc/nginx/sites-enabled")
                              (cons (format "%-20s%s" "kt05" "/etc/nginx/sites-enabled") "/scp:kt05:/etc/nginx/sites-enabled")
                              (cons (format "%-20s%s" "root@kt01" "/etc/nginx/sites-enabled") "/scp:root@kt01:/etc/nginx/sites-enabled")
                              (cons (format "%-20s%s" "root@kt02" "/etc/nginx/sites-enabled") "/scp:root@kt02:/etc/nginx/sites-enabled")
                              (cons (format "%-20s%s" "root@kt03" "/etc/nginx/sites-enabled") "/scp:root@kt03:/etc/nginx/sites-enabled")
                              (cons (format "%-20s%s" "root@kt04" "/etc/nginx/sites-enabled") "/scp:root@kt04:/etc/nginx/sites-enabled")
                              (cons (format "%-20s%s" "root@kt05" "/etc/nginx/sites-enabled") "/scp:root@kt05:/etc/nginx/sites-enabled")
                              (cons (format "%-20s%s" "lcl.api1" "~/") "/scp:lcl.api1:~/")
                              (cons (format "%-20s%s" "lcl.api2_task" "~/") "/scp:lcl.api2_task:~/")
                              (cons (format "%-20s%s" "lcl.pro1" "~/") "/scp:lcl.pro1:~/")
                              (cons (format "%-20s%s" "lcl.pro2" "~/") "/scp:lcl.pro2:~/")
                              (cons (format "%-20s%s" "lcl.pro3" "~/") "/scp:lcl.pro3:~/")
                              (cons (format "%-20s%s" "mw" "~/") "/scp:mw:~/")
                              (cons (format "%-20s%s" "nexus" "~/") "/scp:nexus:~/")
                              (cons (format "%-20s%s" "R720" "/data") "/scp:R720:/data")
                              (cons (format "%-20s%s" "R730" "/data") "/scp:R730:/data")
                              (cons (format "%-20s%s" "repair.pro" "~/") "/scp:repair.pro:~/")
                              (cons (format "%-20s%s" "seafile" "~/") "/scp:seafile:~/"))
                :action  'find-file
                ))))

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

  "om" #'lx/toggle-global-evil-mc-mode

  "ogt" #'lx/toggle-maigt-log-date-format

  "ofs" #'lx/save-scratch

  "oac" #'carbon-now-sh

  "oss" #'lx/helm-remote-locations
  )
