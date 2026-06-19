;;;###autoload
(defun lx/toggle-emacs-tmux ()
  "Toggle *tmux-emacs* tmux session maximized-window"
  (interactive)
  (if (eq major-mode 'zsh-ghostel-mode)
      (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
          (bury-buffer)
        (delete-window))
    (lx/run-in-zsh-ghostel "tmux-attach-or-create emacs" "*tmux-emacs*")))

;;;###autoload
(defun lx/toggle-emacs-tmux-popup ()
  "Toggle *tmux-emacs* tmux session popup"
  (interactive)
  (if (eq major-mode 'zsh-ghostel-mode)
      (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
          (bury-buffer)
        (delete-window))
    (lx/run-in-zsh-ghostel "tmux-attach-or-create emacs" "*tmux-emacs*" nil 'popup)))

;;;###autoload
(defun lx/toggle-project-tmux-session ()
  "Toggle project tmux session maximized-window"
  (interactive)
  (let* ((remote-host (lx/get-remote-buffer-host)))
    (if remote-host
        (let* ((process-environment '("SSH_INTERACTIVE=1"))
               (cmd (format "ssh %s" remote-host))
               (buffer-name (format "*zsh-ghostel-ssh-%s*" remote-host)))
          (lx/run-ssh-in-zsh-ghostel cmd buffer-name (plist-put nil :host remote-host) nil))
      (let* ((project-root (projectile-project-root)))
        (if project-root
            (let* ((cmd (format "tmux-attach-or-create %s %s" project-root project-root))
                   (buffer-name (format "*tmux-%s*" project-root)))
              (lx/run-in-zsh-ghostel cmd buffer-name)
              (with-current-buffer buffer-name
                (setq default-directory project-root)))
          (lx/run-in-zsh-ghostel "tmux-attach-or-create emacs" "*tmux-emacs*"))))))

;;;###autoload
(defun lx/toggle-project-tmux-session-popup ()
  "Toggle project tmux session popup"
  (interactive)
  (if (eq major-mode 'zsh-ghostel-mode)
      (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
          (bury-buffer)
        (delete-window))
    (let* ((remote-host (lx/get-remote-buffer-host)))
      (if remote-host
          (let* ((process-environment '("SSH_INTERACTIVE=1"))
                 (cmd (format "ssh %s" remote-host))
                 (buffer-name (format "*zsh-ghostel-ssh-%s*" remote-host)))
            (lx/run-ssh-in-zsh-ghostel cmd buffer-name (plist-put nil :host remote-host) nil 'popup))
        (let* ((project-root (projectile-project-root)))
          (if project-root
              (let* ((cmd (format "tmux-attach-or-create %s %s" project-root project-root))
                     (buffer-name (format "*tmux-%s*" project-root)))
                (lx/run-in-zsh-ghostel cmd buffer-name nil 'popup)
                (with-current-buffer buffer-name
                  (setq default-directory project-root)))
            (lx/run-in-zsh-ghostel "tmux-attach-or-create emacs" "*tmux-emacs*" nil 'popup)))))))

;;;###autoload
(defun lx/toggle-pry-window ()
  "Toggle pry window"
  (interactive)
  (lx/run-in-pry-ghostel (cdr (assoc "pry" inf-ruby-implementations)) "*pry*"))

;;;###autoload
(defun lx/split-right-and-ghostel ()
  "Split window right and run ghostel"
  (interactive)
  (call-interactively 'split-window-right-and-focus)
  (ghostel))

;;;###autoload
(defun lx/run-arql ()
  "Run arql in pry ghostel"
  (interactive)
  (if (bound-and-true-p arql-env)
      (lx/run-in-pry-ghostel (format "~/.rvm/gems/default/bin/arql -e %s" arql-env)
                              (format "*arql-%s*" arql-env) (projectile-project-root))
    (lx/run-in-pry-ghostel "~/.rvm/gems/default/bin/arql -e jicai.dev,zhzx.dev" "*arql-jicai-zhzx*"
                            (projectile-project-root) t)))

;;;###autoload
(defun lx/run-jshell ()
  "Run jshell in ghostel"
  (interactive)
  (lx/run-in-ghostel "/Library/Java/JavaVirtualMachines/jdk-21.jdk/Contents/Home/bin/jshell --class-path ~/.m2/final/ktjr-common.jar --start ~/.config/default.jsh" "*jshell*"))

;;;###autoload
(defun lx/run-arthas ()
  "Run arthas in ghostel"
  (interactive)
  (lx/run-in-ghostel "as.sh" "*arthas*" default-directory))

;;;###autoload
(defun lx/run-alidash ()
  "Run alidash in ghostel"
  (interactive)
  (lx/run-in-ghostel "alidash" "*alidash*" nil t))

;;;###autoload
(defun lx/run-flo ()
  "Run flo in ghostel"
  (interactive)
  (lx/run-in-ghostel "flo" "*flo*" nil t))

;;;###autoload
(defun lx/run-tuinnel ()
  "Run tuinnel in ghostel"
  (interactive)
  (lx/run-in-ghostel "tuinnel" "*tuinnel*" nil t))

;;;###autoload
(defun lx/run-k9s ()
  "Run k9s in ghostel"
  (interactive)
  (lx/run-in-ghostel "~/bin/k9s" "*k9s*" nil t))

;;;###autoload
(defun lx/run-htop ()
  "Run htop in ghostel"
  (interactive)
  (lx/run-in-ghostel "htop" "*htop*" default-directory t))

;;;###autoload
(defun lx/run-ptpython ()
  "Run ptipython in ghostel"
  (interactive)
  (lx/run-in-ghostel "~/Library/Python/3.12/bin/ptipython" "*ptpython*"))

;;;###autoload
(defun lx/ssh-to-lx-sd ()
  "ssh to lx.sd via ghostel"
  (interactive)
  (helm-zsh-ghostel-ssh-run "lx.sd"))

;;;###autoload
(defun lx/ssh-to-jicai-prod1 ()
  "ssh to jicai.prod1 via ghostel"
  (interactive)
  (helm-zsh-ghostel-ssh-run "jicai.prod1"))

;;;###autoload
(defun lx/ssh-to-jicai-prod2 ()
  "ssh to jicai.prod2 via ghostel"
  (interactive)
  (helm-zsh-ghostel-ssh-run "jicai.prod2"))

;;;###autoload
(defun lx/ssh-to-jicai-prod3 ()
  "ssh to jicai.prod3 via ghostel"
  (interactive)
  (helm-zsh-ghostel-ssh-run "jicai.prod3"))

;;;###autoload
(defun lx/ssh-to-jicai-uat ()
  "ssh to jicai.uat via ghostel"
  (interactive)
  (helm-zsh-ghostel-ssh-run "jicai.uat"))

;;;###autoload
(defun lx/ssh-to-manjaro-z4 ()
  "ssh to manjaro.z4 via ghostel"
  (interactive)
  (helm-zsh-ghostel-ssh-run "manjaro.z4"))

;;;###autoload
(defun lx/ssh-to-lx ()
  "ssh to lx.local via ghostel"
  (interactive)
  (helm-zsh-ghostel-ssh-run "lx"))

;;;###autoload
(defun lx/ssh-to-ueos-dev ()
  "ssh to ueos.dev via ghostel"
  (interactive)
  (helm-zsh-ghostel-ssh-run "ueos.dev"))

;;;###autoload
(defun lx/ssh-to-jicai-dev ()
  "ssh to jicai.dev via ghostel"
  (interactive)
  (helm-zsh-ghostel-ssh-run "jicai.dev"))

;;;###autoload
(defun lx/vrl-jicai-dev ()
  "Run vrl jicai-dev"
  (interactive)
  (helm-ghostel-vrl-run-auto-function "jicai-dev"))

;;;###autoload
(defun lx/vrl-jicai-uat ()
  "Run vrl jicai-uat"
  (interactive)
  (helm-ghostel-vrl-run-auto-function "jicai-uat"))

;;;###autoload
(defun lx/vrl-jicai-prod ()
  "Run vrl jicai-prod"
  (interactive)
  (helm-ghostel-vrl-run-auto-function "jicai-prod"))

;;;###autoload
(defun lx/run-bandwhich ()
  "Run bandwhich in ghostel"
  (interactive)
  (let ((ghostel-kill-buffer-on-exit t))
    (lx/run-in-ghostel "bandwhich" "*ghostel-bandwhich*" nil t)))

;;;###autoload
(defun lx/run-update-all ()
  "Run update-all in ghostel"
  (interactive)
  (let ((ghostel-kill-buffer-on-exit nil))
    (lx/run-in-ghostel "update-all" "*ghostel-cmd-update-all*" nil t)))

;;;###autoload
(defun lx/run-obsidian-sync ()
  "Run obsidian-sync.sh in ghostel"
  (interactive)
  (let ((ghostel-kill-buffer-on-exit nil))
    (lx/run-in-ghostel "obsidian-sync.sh" "*ghostel-cmd-obsidian-sync*" nil t)))

;;;###autoload
(defun lx/run-listening-ports ()
  "Run lsof listening ports in ghostel"
  (interactive)
  (let ((ghostel-kill-buffer-on-exit nil))
    (lx/run-in-ghostel "sudo lsof -Pn -iTCP -sTCP:LISTEN" "*ghostel-cmd-listening-ports*" default-directory t)))

;;;###autoload
(defun lx/run-duf ()
  "Run duf in ghostel"
  (interactive)
  (let ((ghostel-kill-buffer-on-exit nil))
    (lx/run-in-ghostel "sudo duf --only local" "*ghostel-cmd-duf*" default-directory t)))

;;;###autoload
(defun lx/run-jira-cli ()
  "Run jira CLI in ghostel"
  (interactive)
  (let ((ghostel-kill-buffer-on-exit t))
    (lx/run-in-ghostel "jira issue list -q \"assignee = currentUser() AND status not in (CLOSE, closed, Resolved, 'ON HOLD') and project != XYC\"" "*ghostel-cmd-jira-cli*" nil t)))

;;;###autoload
(defun lx/run-git-multi-status ()
  "Run git multi-status in ghostel"
  (interactive)
  (let* ((ghostel-kill-buffer-on-exit nil)
         (root (projectile-project-root))
         (root-base-name (car (last (split-string root "/" t))))
         (buffer-name (format "*ghostel-cmd-git-multi-status-%s*" root-base-name)))
    (lx/run-in-ghostel "git multi-status" buffer-name root t)))

;;;###autoload
(defun lx/run-git-multi-branch ()
  "Run git multi-branch in ghostel"
  (interactive)
  (let* ((ghostel-kill-buffer-on-exit nil)
         (root (projectile-project-root))
         (root-base-name (car (last (split-string root "/" t))))
         (buffer-name (format "*ghostel-cmd-git-multi-branch-%s*" root-base-name)))
    (lx/run-in-ghostel "git multi-branch" buffer-name root t)))

;;;###autoload
(defun lx/run-git-multi-pull ()
  "Run git multi-pull in ghostel"
  (interactive)
  (let* ((ghostel-kill-buffer-on-exit nil)
         (root (projectile-project-root))
         (root-base-name (car (last (split-string root "/" t))))
         (buffer-name (format "*ghostel-cmd-git-multi-pull-%s*" root-base-name)))
    (lx/run-in-ghostel "git multi-pull" buffer-name root t)))

;;;###autoload
(defun lx/run-git-remote-branches (&optional arg)
  "Run git remote-branches in ghostel"
  (interactive "P")
  (let* ((ghostel-kill-buffer-on-exit nil)
         (buffer-name "*ghostel-cmd-git-remote-branches*")
         (remote (if arg (magit-read-remote "Remote") "")))
    (lx/run-in-ghostel (format "git remote-branches %s" remote) buffer-name (magit-toplevel) t)))

;;;###autoload
(defun lx/pop-to-arql-console (arg)
  "Pop to arql console buffer for current project"
  (interactive "P")
  (let ((console (get-buffer (format "*%s-arql*" (projectile-project-name)))))
    (if console
        (pop-to-buffer console)
      (message "Buffer `%s' not found." console))))

;;;###autoload
(defun lx/run-chatsh ()
  "Run chatsh in pry ghostel"
  (interactive)
  (lx/run-in-pry-ghostel "chatsh 4" "*chatsh*"))


;;;###autoload
(defun lx/run-jless (&optional file)
  "Open FILE in jless via ghostel in other window.
If FILE is nil, use the current buffer's file.
In dired mode, use the file at point."
  (interactive)
  (let* ((file (or file
                   (if (derived-mode-p 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
         (cmd (format "jless %s" (shell-quote-argument file)))
         (buffer-name (format "*jless-%s*" (file-name-nondirectory file))))
    (lx/run-in-ghostel cmd buffer-name nil nil)))


;;;###autoload
(defun lx/run-lnav (&optional file)
  "Open FILE in lnav via ghostel in other window.
If FILE is nil, use the current buffer's file.
In dired mode, use the file at point."
  (interactive)
  (let* ((file (or file
                   (if (derived-mode-p 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
         (cmd (format "lnav %s" (shell-quote-argument file)))
         (buffer-name (format "*lnav-%s*" (file-name-nondirectory file))))
    (lx/run-in-ghostel cmd buffer-name nil nil)))
