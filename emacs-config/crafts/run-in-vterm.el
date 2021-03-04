(require 'vterm)

(defvar lx/run-shell-in-vterm-keymap (let ((map (make-sparse-keymap)))
                                       (set-keymap-parent map vterm-mode-map)
                                       (evil-define-key 'hybrid map (kbd "s-1") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "1")))
                                       (evil-define-key 'hybrid map (kbd "s-2") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "2")))
                                       (evil-define-key 'hybrid map (kbd "s-3") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "3")))
                                       (evil-define-key 'hybrid map (kbd "s-4") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "4")))
                                       (evil-define-key 'hybrid map (kbd "s-5") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "5")))
                                       (evil-define-key 'hybrid map (kbd "s-6") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "6")))
                                       (evil-define-key 'hybrid map (kbd "M-1") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "7")))
                                       (evil-define-key 'hybrid map (kbd "M-2") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "8")))
                                       (evil-define-key 'hybrid map (kbd "M-3") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "9")))
                                       (evil-define-key 'hybrid map (kbd "M-4") #'(lambda () (interactive) (vterm-send-C-j) (vterm-send-string "0")))
                                       map))

(defun lx/run-shell-in-vterm (command buffer-name &optional directory exclusive-window)
  (interactive)
  (let* ((buffer (get-buffer buffer-name)))
    (set (intern (format "%s-command" buffer-name)) (list command buffer-name directory exclusive-window))
    (set (intern (format "%s-process-environment" buffer-name)) process-environment)
    (set (intern (format "%s-kill-buffer-on-exit" buffer-name)) (bound-and-true-p vterm-kill-buffer-on-exit))
    (if buffer
        (if (equal buffer (current-buffer))
            (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
                (bury-buffer)
              (delete-window))
          (if exclusive-window
              (switch-to-buffer buffer)
            (pop-to-buffer buffer 'display-buffer-pop-up-window)))
      (let* ((default-directory (or directory user-home-directory))
             (vterm-shell command))
        (unless exclusive-window (split-window-right-and-focus))
        (vterm buffer-name)
        (with-current-buffer buffer-name (use-local-map lx/run-shell-in-vterm-keymap))))))


(defun lx/run-in-vterm (command buffer-name &optional directory exclusive-window)
  (interactive)
  (let* ((buffer (get-buffer buffer-name)))
    (set (intern (format "%s-command" buffer-name)) (list command buffer-name directory exclusive-window))
    (set (intern (format "%s-process-environment" buffer-name)) process-environment)
    (set (intern (format "%s-kill-buffer-on-exit" buffer-name)) (bound-and-true-p vterm-kill-buffer-on-exit))
    (if buffer
        (if (equal buffer (current-buffer))
            (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
                (bury-buffer)
              (delete-window))
          (if exclusive-window
              (switch-to-buffer buffer)
            (pop-to-buffer buffer 'display-buffer-pop-up-window)))
      (let* ((default-directory (or directory user-home-directory))
             (vterm-shell command))
        (unless exclusive-window (split-window-right-and-focus))
        (vterm buffer-name)))))

(defun lx/run-in-vterm/rerun ()
  (interactive)
  (let* ((buffer-name (buffer-name))
         (process (get-buffer-process buffer-name))
         (args (eval (read (format "%s-command" buffer-name))))
         (penv (eval (read (format "%s-process-environment" buffer-name))))
         (kill-on-exit (eval (read (format "%s-kill-buffer-on-exit" buffer-name)))))
    (if process
        (message "Buffer process still running")
      (progn
        (kill-buffer)
        (let ((process-environment penv)
              (vterm-kill-buffer-on-exit kill-on-exit))
          (apply 'lx/run-in-vterm args))))))

(defun lx/run-in-vterm/set-green-box-cursor ()
  (interactive)
  (setq cursor-type 'box)
  (set-cursor-color "#00ff00"))

(defun lx/run-in-vterm/set-blue-bar-cursor ()
  (interactive)
  (setq cursor-type 'bar)
  (set-cursor-color "#6db2e9"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (eq 'vterm-mode (with-current-buffer b major-mode)))
                        (buffer-list)))))

(defclass helm-vterm-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(setq helm-vterm-buffers-list
      (helm-make-source "VTerm Buffers" 'helm-vterm-buffers-source))

(defun helm-vterm-buffers ()
  (interactive)
  (helm-other-buffer '(helm-vterm-buffers-list) "*helm-vterm-buffers*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-ssh-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-ssh-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-ssh-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-ssh-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-ssh-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-ssh-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-ssh-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-ssh-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-ssh-option-list ()
  (mapcar (lambda (host) (cons host host))
          (s-split "\n" (shell-command-to-string "perl -ne 'if (/^Host [^*]/) {s/^Host *//; print;}' ~/.ssh/config") t)))

(defun helm-vterm-ssh-run (host)
  (let ((process-environment '("SSH_INTERACTIVE=1"))
        (cmd (format "ssh %s" host))
        (buffer-name (format "*vterm-ssh-%s*" host)))
    (lx/run-shell-in-vterm cmd buffer-name nil t)))

(defclass helm-vterm-ssh-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-ssh-option-list)
   (action :initform 'helm-vterm-ssh-run)))

(setq helm-vterm-ssh-buffers-list
      (helm-make-source "SSH Buffers" 'helm-vterm-ssh-buffers-source))

(setq helm-vterm-ssh-options-list
      (helm-make-source "SSH Hosts" 'helm-vterm-ssh-options-source))

(defun helm-vterm-ssh ()
  (interactive)
  (helm-other-buffer '(helm-vterm-ssh-buffers-list helm-vterm-ssh-options-list) "*helm-vterm-ssh-buffers*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-arql-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-arql-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-arql-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-arql-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*arql-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-arql-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-arql-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-arql-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-arql-option-list ()
  (mapcar (lambda (host) (cons host host))
          (s-split "\n" (shell-command-to-string "perl -ne 'unless (/^default/) { if (/^\\w/) {s/:$//; print;}}' ~/.arql.d/init.yml") t)))

(defun helm-vterm-arql-run (env)
  (let ((cmd  (format "~/.rvm/gems/ruby-3.0.0/bin/arql -e %s" env))
        (buffer-name (format "*arql-%s*" env)))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defclass helm-vterm-arql-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-arql-option-list)
   (action :initform 'helm-vterm-arql-run)))

(setq helm-vterm-arql-options-list
      (helm-make-source "ARQL Environments" 'helm-vterm-arql-options-source))

(setq helm-vterm-arql-buffers-list
      (helm-make-source "ARQL Buffers" 'helm-vterm-arql-buffers-source))

(defun helm-vterm-arql ()
  (interactive)
  (helm-other-buffer '(helm-vterm-arql-buffers-list helm-vterm-arql-options-list) "*helm-vterm-arql-buffers*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-vrl-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-vrl-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-vrl-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-vrl-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-vrl-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-vrl-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-vrl-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-vrl-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-vrl-option-list ()
  (mapcar (lambda (host) (cons host host))
          (s-split "\n" (shell-command-to-string "perl -ne 'unless (/^default/) { if (/^\\S/) {s/:$//; print;}}' ~/.vrl.yml") t)))

(defun helm-vterm-vrl-run-function (env)
  (let ((cmd  (format "~/bin/vrl %s" env))
        (buffer-name (format "*vterm-vrl-%s*" env)))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defun helm-vterm-vrl-run ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-vterm-vrl-run-function)))

(defun helm-vterm-vrl-run-auto-function (env)
  (let ((cmd  (format "~/bin/vrl %s -a" env))
        (buffer-name (format "*vterm-vrl-%s*" env)))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defvar helm-vterm-vrl-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-RET") 'helm-vterm-vrl-run-function)
    map))

(defclass helm-vterm-vrl-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-vrl-option-list)
   (action :initform (helm-make-actions "vrl auto" 'helm-vterm-vrl-run-auto-function
                                        "vrl" 'helm-vterm-vrl-run-function))
   (keymap :initform 'helm-vterm-vrl-map)))

(setq helm-vterm-vrl-options-list
      (helm-make-source "VRL Environments" 'helm-vterm-vrl-options-source))

(setq helm-vterm-vrl-buffers-list
      (helm-make-source "VRL Buffers" 'helm-vterm-vrl-buffers-source))

(defun helm-vterm-vrl ()
  (interactive)
  (helm-other-buffer '(helm-vterm-vrl-buffers-list helm-vterm-vrl-options-list) "*helm-vterm-vrl-buffers*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-maven-deploy-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-maven-deploy-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-maven-deploy-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-maven-deploy-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-maven-deploy-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-maven-deploy-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-maven-deploy-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-maven-deploy-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-maven-option-list ()
  (let ((project-root-dir (projectile-project-root)))
    (mapcar (lambda (host) (cons host host))
            (s-split "\n" (shell-command-to-string (format "find %s -maxdepth 4 -name pom.xml | perl -pe 's#%s/##;s#/pom.xml##'" project-root-dir project-root-dir)) t))))

(defun helm-vterm-maven-deploy-run (artifact)
  (let* ((vterm-kill-buffer-on-exit nil)
         (project-root-dir (replace-regexp-in-string "/$" "" (projectile-project-root)))
         (default-directory (format "%s/%s" project-root-dir artifact))
         (trigger (if (bound-and-true-p maven-trigger) (format "-Dtrigger=%s" maven-trigger) ""))
         (cmd  (format "mvn -Dmaven.test.skip=true %s -U install deploy" trigger))
         (buffer-name (format "*vterm-maven-deploy-%s/%s*" (file-name-nondirectory project-root-dir) artifact)))
    (lx/run-in-vterm cmd buffer-name default-directory t)))

(defclass helm-vterm-maven-deploy-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-maven-option-list)
   (action :initform 'helm-vterm-maven-deploy-run)))

(setq helm-vterm-maven-deploy-options-list
      (helm-make-source "MAVEN-DEPLOY Artifacts" 'helm-vterm-maven-deploy-options-source))

(setq helm-vterm-maven-deploy-buffers-list
      (helm-make-source "MAVEN-DEPLOY Buffers" 'helm-vterm-maven-deploy-buffers-source))

(defun helm-vterm-maven-deploy ()
  (interactive)
  (helm-other-buffer '(helm-vterm-maven-deploy-buffers-list helm-vterm-maven-deploy-options-list) "*helm-vterm-maven-deploy-buffers*"))

(defun helm-vterm-maven-deploy-current-artifact ()
  (interactive)
  (let* ((dir default-directory)
         (found nil))
    (while (and (not found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (helm-vterm-maven-deploy-run (s-replace (projectile-project-root) "" dir))
        (setq found t))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "pom not found"))))

(defun helm-vterm-maven-deploy-parent-artifact ()
  (interactive)
  (let* ((dir default-directory)
         (found nil)
         (parent-found nil))
    (while (and (not parent-found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (if (not found)
            (setq found t)
          (helm-vterm-maven-deploy-run (s-replace (projectile-project-root) "" dir))
          (setq parent-found t)))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "parent pom not found"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-maven-deps-tree-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-maven-deps-tree-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-maven-deps-tree-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-maven-deps-tree-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-maven-deps-tree-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-maven-deps-tree-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-maven-deps-tree-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-maven-deps-tree-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-maven-deps-tree-run (artifact)
  (let* ((vterm-kill-buffer-on-exit nil)
         (project-root-dir (replace-regexp-in-string "/$" "" (projectile-project-root)))
         (default-directory (format "%s/%s" project-root-dir artifact))
         (trigger (if (bound-and-true-p maven-trigger) (format "-Dtrigger=%s" maven-trigger) ""))
         (cmd  "mvn dependency:tree")
         (buffer-name (format "*vterm-maven-deps-tree-%s/%s*" (file-name-nondirectory project-root-dir) artifact)))
    (lx/run-in-vterm cmd buffer-name default-directory t)))

(defclass helm-vterm-maven-deps-tree-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-maven-option-list)
   (action :initform 'helm-vterm-maven-deps-tree-run)))

(setq helm-vterm-maven-deps-tree-options-list
      (helm-make-source "MAVEN-DEPS-TREE Artifacts" 'helm-vterm-maven-deps-tree-options-source))

(setq helm-vterm-maven-deps-tree-buffers-list
      (helm-make-source "MAVEN-DEPS-TREE Buffers" 'helm-vterm-maven-deps-tree-buffers-source))

(defun helm-vterm-maven-deps-tree ()
  (interactive)
  (helm-other-buffer '(helm-vterm-maven-deps-tree-buffers-list helm-vterm-maven-deps-tree-options-list) "*helm-vterm-maven-deps-tree-buffers*"))

(defun helm-vterm-maven-deps-tree-current-artifact ()
  (interactive)
  (let* ((dir default-directory)
         (found nil))
    (while (and (not found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (helm-vterm-maven-deps-tree-run (s-replace (projectile-project-root) "" dir))
        (setq found t))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "pom not found"))))

(defun helm-vterm-maven-deps-tree-parent-artifact ()
  (interactive)
  (let* ((dir default-directory)
         (found nil)
         (parent-found nil))
    (while (and (not parent-found) (not (string= "/" dir)))
      (when (file-exists-p (format "%s/pom.xml" dir))
        (if (not found)
            (setq found t)
          (helm-vterm-maven-deps-tree-run (s-replace (projectile-project-root) "" dir))
          (setq parent-found t)))
      (setq dir (expand-file-name (format "%s/.." dir))))
    (unless found (message "parent pom not found"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-maven-deps-resolve-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-maven-deps-resolve-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-maven-deps-resolve-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-maven-deps-resolve-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-maven-deps-resolve-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-maven-deps-resolve-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-maven-deps-resolve-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-maven-deps-resolve-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-maven-deps-resolve-run (artifact)
  (let* ((vterm-kill-buffer-on-exit nil)
         (project-root-dir (replace-regexp-in-string "/$" "" (projectile-project-root)))
         (default-directory (format "%s/%s" project-root-dir artifact))
         (trigger (if (bound-and-true-p maven-trigger) (format "-Dtrigger=%s" maven-trigger) ""))
         (cmd  "mvn dependency:resolve")
         (buffer-name (format "*vterm-maven-deps-resolve-%s/%s*" (file-name-nondirectory project-root-dir) artifact)))
    (lx/run-in-vterm cmd buffer-name default-directory t)))

(defclass helm-vterm-maven-deps-resolve-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-maven-option-list)
   (action :initform 'helm-vterm-maven-deps-resolve-run)))

(setq helm-vterm-maven-deps-resolve-options-list
      (helm-make-source "MAVEN-DEPS-RESOLVE Artifacts" 'helm-vterm-maven-deps-resolve-options-source))

(setq helm-vterm-maven-deps-resolve-buffers-list
      (helm-make-source "MAVEN-DEPS-RESOLVE Buffers" 'helm-vterm-maven-deps-resolve-buffers-source))

(defun helm-vterm-maven-deps-resolve ()
  (interactive)
  (helm-other-buffer '(helm-vterm-maven-deps-resolve-buffers-list helm-vterm-maven-deps-resolve-options-list) "*helm-vterm-maven-deps-resolve-buffers*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-vterm-jenkins-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-vterm-jenkins-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-vterm-jenkins-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-vterm-jenkins-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (and (eq 'vterm-mode (with-current-buffer b major-mode))
                          (s-starts-with? "*vterm-jenkins-" (with-current-buffer b (buffer-name)))))
                        (buffer-list)))))

(defclass helm-vterm-jenkins-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-vterm-jenkins-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-vterm-jenkins-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun helm-vterm-jenkins-run (alias)
  (let* ((vterm-kill-buffer-on-exit nil)
         (alias-name (replace-regexp-in-string ":.*" "" alias))
         (jenkins-project-name (replace-regexp-in-string ".*build\s-*\\|:.*" "" alias))
         (cmd  (format "jk %s" alias-name))
         (buffer-name (format "*vterm-jenkins-%s (%s)*" alias-name jenkins-project-name)))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defun helm-vterm-jenkins-option-list ()
  (let ((project-root-dir (projectile-project-root)))
    (mapcar (lambda (alias) (cons alias alias))
            (s-split "\n" (shell-command-to-string "yq e .aliases ~/.jenkins-builder.yaml | grep 'build'") t))))

(defclass helm-vterm-jenkins-options-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-jenkins-option-list)
   (action :initform 'helm-vterm-jenkins-run)))

(setq helm-vterm-jenkins-options-list
      (helm-make-source "JENKINS Aliases" 'helm-vterm-jenkins-options-source))

(setq helm-vterm-jenkins-buffers-list
      (helm-make-source "JENKINS Buffers" 'helm-vterm-jenkins-buffers-source))

(defun helm-vterm-jenkins ()
  (interactive)
  (helm-other-buffer '(helm-vterm-jenkins-buffers-list helm-vterm-jenkins-options-list) "*helm-vterm-jenkins-buffers*"))

(provide 'run-in-vterm)
