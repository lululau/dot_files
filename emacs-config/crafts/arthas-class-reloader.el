(defvar arthas-class-reload-docker-list nil)
(defvar arthas-class-reload-k8s-list nil)

(defvar arthas-class-reload-dir (file-name-directory (or load-file-name buffer-file-name)))

(defun arthas-class-reload--find-target-dir (source-file)
  (let* ((dir (replace-regexp-in-string "/$" "" (file-name-directory source-file)))
         (dir-base (file-name-nondirectory dir))
         (pdir (replace-regexp-in-string "/$" "" (file-name-directory dir))))
    (while (not (or (string= pdir "") (and (string= dir-base "src") (seq-contains-p (directory-files pdir) "target"))))
      (setq dir pdir)
      (setq dir-base (file-name-nondirectory dir))
      (setq pdir (replace-regexp-in-string "/$" "" (file-name-directory pdir))))
    (if (not (string= pdir ""))
        (concat pdir "/target"))))

(defun arthas-class-reload--find-class-file (source-file)
  (let* ((target-dir (arthas-class-reload--find-target-dir source-file)))
    (if target-dir
        (let* ((pdir (file-name-directory target-dir))
               (bare-name (file-name-sans-extension source-file))
               (class-file (concat target-dir "/classes/" (string-replace (concat pdir "src/main/java/") "" bare-name ) ".class")))
          (if (file-exists-p class-file)
              class-file)))))

(defun arthas-class-reload--check-class-file-time (source-file class-file)
  (let* ((source-time (file-attribute-modification-time (file-attributes source-file)))
         (class-time (file-attribute-modification-time (file-attributes class-file))))
    (if (time-less-p source-time class-time)
        t
      (yes-or-no-p (format "Class file %s is older than source file %s, continue?" class-file source-file)))))

(defun arthas-class-reload--docker-options ()
  (mapcan (lambda (item)
            (let* ((context (plist-get item :context))
                   (pattern (plist-get item :container-filter))
                   (cmd (format "docker --context '%s' ps --format '{{.Names}}' | perl -ne 'print if /%s/'" context pattern)))
              (mapcar (lambda (name)
                        (cons (format "%s\t%s" context name) (list :context context :name name)))
                      (split-string (shell-command-to-string cmd) "\n" t))))
          arthas-class-reload-docker-list))

(defun arthas-class-reload--reload-class-in-docker (option)
  (let* ((context (plist-get option :context))
         (name (plist-get option :name))
         (cmd (format "%s/arthas-class-reload-class-in-docker.sh '%s' '%s' '%s'" arthas-class-reload-dir context name arthas-class-reload--class-file)))
    (message (shell-command-to-string cmd))))

(defclass arthas-class-reload--docker-source (helm-source-sync)
  ((candidates :initform 'arthas-class-reload--docker-options)
   (action :initform 'arthas-class-reload--reload-class-in-docker)))

(setq arthas-class-reload--docker-options-list
      (helm-make-source "Docker Containers for Arthas Class Reloader" 'arthas-class-reload--docker-source))

(defun arthas-class-reload--read-docker-container ()
  (let ((default-directory "~"))
    (helm-other-buffer '(arthas-class-reload--docker-options-list) "*helm-ascreloader*")))

(defun arthas-class-reload-docker (&optional source-file)
  (interactive)
  (let* ((source-file (or source-file (buffer-file-name)))
         (class-file (arthas-class-reload--find-class-file source-file)))
    (if class-file
        (if (arthas-class-reload--check-class-file-time source-file class-file)
            (let* ((arthas-class-reload--class-file class-file))
                   (arthas-class-reload--read-docker-container)))
      (message "No class file found for %s." source-file))))

(defun arthas-class-reload--k8s-options ()
  (mapcan (lambda (item)
            (let* ((context (plist-get item :context))
                   (ns-filter (plist-get item :namespace-filter))
                   (deploy-filter (plist-get item :deployment-filter))
                   (cmd (format "kubectl --context '%s' get deploy -A -o jsonpath='{range .items[*]}{.metadata.namespace}{\",\"}{.metadata.name}{\"\\n\"}{end}' | perl -F/,/ -ane 'print if $F[0] =~ /%s/ && $F[1] =~ /%s/'" context ns-filter deploy-filter)))
              (mapcar (lambda (name)
                        (let* ((fields (s-split "," name))
                               (ns (nth 0 fields))
                               (deploy (nth 1 fields)))
                          (cons (format "%s\t%s\t%s" context ns deploy) (list :context context :namespace ns :deployment deploy))))
                      (split-string (shell-command-to-string cmd) "\n" t))))
          arthas-class-reload-k8s-list))

(defun arthas-class-reload--reload-class-in-k8s (option)
  (let* ((context (plist-get option :context))
         (ns (plist-get option :namespace))
         (deploy (plist-get option :deployment))
         (cmd (format "%s/arthas-class-reload-class-in-k8s.sh '%s' '%s' '%s' '%s'"
                      arthas-class-reload-dir context ns deploy arthas-class-reload--class-file)))
    (message (shell-command-to-string cmd))))

(defclass arthas-class-reload--k8s-source (helm-source-sync)
  ((candidates :initform 'arthas-class-reload--k8s-options)
   (action :initform 'arthas-class-reload--reload-class-in-k8s)))

(setq arthas-class-reload--k8s-options-list
      (helm-make-source "K8s Deployments for Arthas Class Reloader" 'arthas-class-reload--k8s-source))

(defun arthas-class-reload--read-k8s-container ()
  (let ((default-directory "~"))
    (helm-other-buffer '(arthas-class-reload--k8s-options-list) "*helm-ascreloader*")))

(defun arthas-class-reload-k8s (&optional source-file)
  (interactive)
  (let* ((source-file (or source-file (buffer-file-name)))
         (class-file (arthas-class-reload--find-class-file source-file)))
    (if class-file
        (if (arthas-class-reload--check-class-file-time source-file class-file)
            (let* ((arthas-class-reload--class-file class-file))
              (arthas-class-reload--read-k8s-container)))
      (message "No class file found for %s." source-file))))

(provide 'arthas-class-reloader)
