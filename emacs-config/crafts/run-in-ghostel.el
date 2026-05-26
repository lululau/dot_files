(require 'ghostel)

(defvar lx/run-in-ghostel/histdb-file (expand-file-name "~/.histdb/zsh-history.db"))

(defun lx/run-in-ghostel (command buffer-name &optional directory exclusive-window)
  (interactive)
  (let* ((buffer (get-buffer buffer-name)))
    (set (intern (format "%s-command" buffer-name)) (list command buffer-name directory exclusive-window))
    (set (intern (format "%s-process-environment" buffer-name)) process-environment)
    (set (intern (format "%s-kill-buffer-on-exit" buffer-name)) (bound-and-true-p ghostel-kill-buffer-on-exit))
    (set (intern (format "%s-kill-buffer-on-normal-exit" buffer-name)) (bound-and-true-p ghostel-kill-buffer-on-normal-exit))
    (if buffer
        (if (equal buffer (current-buffer))
            (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
                (bury-buffer)
              (delete-window))
          (if exclusive-window
              (switch-to-buffer buffer)
            (pop-to-buffer buffer 'display-buffer-pop-up-window)))
      (let* ((default-directory (or directory user-home-directory))
             (command-parts (split-string-and-unquote command))
             (buffer (generate-new-buffer buffer-name)))
        (unless exclusive-window (split-window-right-and-focus))
        (with-current-buffer buffer
          (ghostel-mode)
          (setq ghostel--managed-buffer-name "")
          (setq-local ghostel-kill-buffer-on-exit ghostel-kill-buffer-on-exit)
          (setq-local ghostel-kill-buffer-on-normal-exit
                      (and ghostel-kill-buffer-on-exit ghostel-kill-buffer-on-normal-exit)))
        (pop-to-buffer buffer (append display-buffer--same-window-action
                                      '((category . comint))))
        (ghostel-exec buffer (car command-parts) (cdr command-parts))
        buffer))))


(defun lx/run-in-projectile-ghostel--buffer-directory (buffer)
  (let ((sym (intern (format "%s-command" (buffer-name buffer)))))
    (or (and (boundp sym) (nth 2 (symbol-value sym)))
        (with-current-buffer buffer default-directory))))

(defun lx/run-in-projectile-ghostel--buffer-in-scope-p (buffer scope-root)
  (let* ((scope (file-name-as-directory (expand-file-name scope-root)))
         (dir (lx/run-in-projectile-ghostel--buffer-directory buffer)))
    (and dir (string-prefix-p scope (expand-file-name dir)))))

(defun lx/run-in-projectile-ghostel--find-buffer (buffer-name scope-root)
  (let ((buffer (get-buffer buffer-name)))
    (when (and buffer (buffer-live-p buffer)
               (lx/run-in-projectile-ghostel--buffer-in-scope-p buffer scope-root))
      buffer)))

(defun lx/run-in-projectile-ghostel--scope-root ()
  (or (and (fboundp 'projectile-project-p) (projectile-project-p)
           (projectile-project-root))
      (and (buffer-file-name) (file-name-directory (buffer-file-name)))
      user-home-directory))

(defun lx/run-in-projectile-ghostel--scope-label ()
  (or (and (fboundp 'projectile-project-p) (projectile-project-p)
           (projectile-project-name))
      (and (buffer-file-name)
           (let ((dir (file-name-directory (buffer-file-name))))
             (and dir (file-name-nondirectory (directory-file-name dir)))))
      "~"))

(defun lx/run-in-projectile-ghostel (command buffer-name &optional directory exclusive-window)
  (interactive)
  (let* ((scope-root (lx/run-in-projectile-ghostel--scope-root))
         (buffer-name (replace-regexp-in-string
                       "%p"
                       (lx/run-in-projectile-ghostel--scope-label)
                       buffer-name))
         (directory (or directory scope-root))
         (buffer (lx/run-in-projectile-ghostel--find-buffer buffer-name scope-root))
         (real-get-buffer (symbol-function 'get-buffer)))
    (when-let* ((wrong (and (not buffer) (get-buffer buffer-name))))
      (when (and (derived-mode-p 'ghostel-mode wrong)
                 (not (lx/run-in-projectile-ghostel--buffer-in-scope-p wrong scope-root)))
        (kill-buffer wrong)))
    (cl-letf (((symbol-function 'get-buffer)
               (lambda (name)
                 (if (equal name buffer-name)
                     buffer
                   (funcall real-get-buffer name)))))
      (lx/run-in-ghostel command buffer-name directory exclusive-window))))

(defun lx/run-in-ghostel/rerun ()
  (interactive)
  (let* ((buffer-name (buffer-name))
         (process (get-buffer-process buffer-name))
         (args (eval (read (format "%s-command" buffer-name))))
         (penv (eval (read (format "%s-process-environment" buffer-name))))
         (kill-on-exit (eval (read (format "%s-kill-buffer-on-exit" buffer-name))))
         (kill-on-normal-exit (eval (read (format "%s-kill-buffer-on-normal-exit" buffer-name)))))
    (if process
        (message "Buffer process still running")
      (progn
        (kill-buffer)
        (let ((process-environment penv)
              (ghostel-kill-buffer-on-exit kill-on-exit)
              (ghostel-kill-buffer-on-normal-exit kill-on-normal-exit))
          (apply 'lx/run-in-ghostel args))))))

(defun lx/run-in-ghostel/set-green-box-cursor ()
  (interactive)
  (setq cursor-type 'box)
  (set-cursor-color "#00ff00"))

(defun lx/run-in-ghostel/set-blue-bar-cursor ()
  (interactive)
  (setq cursor-type 'bar)
  (set-cursor-color "#6db2e9"))

(defun lx/run-in-ghostel/set-default-directory (dir)
  "Update `default-directory' from shell (OSC 51 \"update-pwd\")."
  (interactive)
  (let ((dir
         (if (eq major-mode 'ssh-zsh-ghostel-mode)
             (progn
               (require 'zsh-ghostel-ssh)
               (ssh-zsh-ghostel--tramp-default-directory
                (plist-get ssh-zsh-ghostel-ssh-options :host)
                dir))
           dir)))
    (helm-dired-history--update dir)
    (setq default-directory dir
          list-buffers-directory dir)))

(defun lx/run-in-ghostel/download (file)
  (interactive)
  (let* ((remote-host (if (eq major-mode 'ssh-zsh-ghostel-mode)
                          (plist-get ssh-zsh-ghostel-ssh-options :host)
                        nil)))
    (if (not remote-host)
        (message "Not in ssh-zsh-ghostel-mode")
      (let* ((remote-file (format "%s:%s" remote-host file))
             (local-dir (helm-dired-history-read-file-name "Local directory: "
                                             "~/tmp/"
                                             "~/tmp/"))
             (local-dir (shell-quote-argument (if (string-suffix-p "/" local-dir) local-dir (concat local-dir "/"))))
             (remote-file (replace-regexp-in-string "/$" "" remote-file))
             (cmd (format "rsync -rzP '%s' %s" remote-file local-dir))
             (buffer-name (format "*rsync: %s -> %s*" remote-file local-dir))
             (ghostel-kill-buffer-on-exit nil))
        (lx/run-in-ghostel cmd buffer-name)))))

(defun lx/run-in-ghostel/upload (dir)
  (interactive)
  (let* ((remote-host (if (eq major-mode 'ssh-zsh-ghostel-mode)
                          (plist-get ssh-zsh-ghostel-ssh-options :host)
                        nil)))
    (if (not remote-host)
        (message "Not in ssh-zsh-ghostel-mode")
      (let* ((remote-dir (format "%s:%s" remote-host dir))
             (local-file (helm-dired-history-read-file-name "Local File: "
                                              "~/tmp/"
                                              "~/tmp/"))
             (remote-dir (if (string-suffix-p "/" remote-dir) remote-dir (concat remote-dir "/")))
             (local-file (shell-quote-argument (replace-regexp-in-string "/$" "" local-file)))
             (cmd (format "rsync -rzP %s '%s'" local-file remote-dir))
             (buffer-name (format "*rsync: %s -> %s*" local-file remote-dir))
             (ghostel-kill-buffer-on-exit nil))
        (lx/run-in-ghostel cmd buffer-name)))))

(defun lx/run-in-ghostel/find-remote-file (file &optional host)
  (interactive)
  (let* ((remote-host (if (eq major-mode 'ssh-zsh-ghostel-mode)
                          (plist-get ssh-zsh-ghostel-ssh-options :host)
                        nil))
         (file-prefix (if remote-host (format "/scp:%s:" remote-host) ""))
         (file (concat file-prefix file)))
    (find-file-other-window file)))

(defun lx/run-in-ghostel/sudo-find-remote-file (file &optional host)
  (interactive)
  (let* ((remote-host (if (eq major-mode 'ssh-zsh-ghostel-mode)
                          (plist-get ssh-zsh-ghostel-ssh-options :host)
                        nil))
         (file-prefix (if remote-host (format "/sudo:root@%s:" remote-host) ""))
         (file (concat file-prefix file)))
    (find-file-other-window file)))

(defun lx/run-in-ghostel/sql-escape (sql)
  (string-replace "'" "''" (string-replace "\0" "" sql)))

(defun lx/run-in-ghostel/histdb-query (sql)
  (let ((sql (replace-regexp-in-string "'+" "'\"\\&\"'" sql)))
    (start-process-shell-command "histdb-query" nil (format "sqlite3 -cmd '.timeout 1000' '%s' '%s'" lx/run-in-ghostel/histdb-file sql))))

(defun lx/run-in-ghostel/save-history-to-ghostel (session hostname cmd pwd started)
  (interactive)
  (let* ((remote-host (if (eq major-mode 'ssh-zsh-ghostel-mode)
                          (plist-get ssh-zsh-ghostel-ssh-options :host)
                        nil))
         (remote-host (concat "'" remote-host "'"))
         (cmd (concat "'" (lx/run-in-ghostel/sql-escape cmd) "'"))
         (pwd (concat "'" (lx/run-in-ghostel/sql-escape pwd) "'")))
    (if (> (length remote-host) 2)
        (lx/run-in-ghostel/histdb-query (format "insert into commands (argv) values (%s);
insert into places   (host, dir) values (%s, %s);
insert into history
  (session, command_id, place_id, start_time)
select
  %s,
  commands.rowid,
  places.rowid,
  %s
from
  commands, places
where
  commands.argv = %s and
  places.host = %s and
  places.dir = %s
;" cmd remote-host pwd session started cmd remote-host pwd)))))

(defun lx/run-in-ghostel/update-history-outcome-to-ghostel (session hostname retval finished)
  (interactive)
  (let* ((remote-host (if (eq major-mode 'ssh-zsh-ghostel-mode)
                          (plist-get ssh-zsh-ghostel-ssh-options :host)
                        nil))
         (remote-host (concat "'" remote-host "'")))
    (if (> (length remote-host) 2)
        (lx/run-in-ghostel/histdb-query (format "update history set exit_status = %s, duration = %s - start_time
where rowid = (select max(h.rowid) from history h join places p on h.place_id = p.rowid where h.session = %s and p.host = %s)" retval finished session remote-host)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-ghostel-buffers-list--init ()
  (require 'dired)
  (helm-attrset 'candidates (funcall (helm-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-attr
                                               'filtered-candidate-transformer
                                               helm-ghostel-buffers-list))
                         for b in (if allbufs
                                      (helm-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-attr 'candidates)
                                     helm-ghostel-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defun helm-ghostel-buffer-list ()
  (let ((directory (expand-file-name default-directory)))
    (mapcar 'buffer-name
            (seq-filter (lambda (b)
                          (seq-contains-p '(ghostel-mode zsh-ghostel-mode ssh-zsh-ghostel-mode pry-ghostel-mode) (with-current-buffer b major-mode)))
                        (buffer-list)))))

(defclass helm-ghostel-buffers-source (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-ghostel-buffer-list
    :custom function
    :documentation)
   (init :initform 'helm-ghostel-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(setq helm-ghostel-buffers-list
      (helm-make-source "Ghostel Buffers" 'helm-ghostel-buffers-source))

(defun helm-ghostel-buffers ()
  (interactive)
  (helm-other-buffer '(helm-ghostel-buffers-list) "*helm-ghostel-buffers*"))

(provide 'run-in-ghostel)
