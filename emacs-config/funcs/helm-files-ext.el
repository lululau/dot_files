;; -*- lexical-binding: t; -*-

(defvar helm-fd-switches)
(defvar helm-pattern)

(defun lx/helm-fd-keywords-process ()
  "Search with fd, requiring every shell-quoted term in `helm-pattern'."
  (let ((patterns (delete "" (split-string-shell-command helm-pattern))))
    (if patterns
        (let ((helm-fd-switches
               (append helm-fd-switches
                       (mapcar (lambda (pattern) (concat "--and=" pattern))
                               patterns)))
              (helm-pattern ""))
          (helm-fd-process))
      ;; Async Helm sources require a process, even with no candidates.
      (let ((process (make-pipe-process :name "fd-empty" :noquery t)))
        (delete-process process)
        process))))

;;;###autoload
(defun lx/helm-find-project-files (&optional raw)
  "Find project files matching all space-separated regular expressions.
Use quotes to group a pattern containing spaces.  Patterns may match in
any order.  With prefix argument RAW, use the original fd command-line
syntax, including options and search directories."
  (interactive "P")
  (require 'helm-fd)
  (require 'shell)
  (let ((directory (projectile-project-root)))
    (helm-set-local-variable 'helm-fd-switches helm-fd-switches)
    (if raw
        (helm-fd-1 directory)
      (cl-assert (executable-find helm-fd-executable) nil "Could not find fd executable")
      (cl-assert (not (file-remote-p directory)) nil "Fd not supported on remote directories")
      (let ((default-directory directory))
        (helm :sources (helm-make-source "Fd (all keywords)" 'helm-fd-class
                         :candidates-process #'lx/helm-fd-keywords-process
                         :header-name
                         (lambda (name)
                           (format "%s (%s)"
                                   name (abbreviate-file-name default-directory)))
                         :help-message
                         "Space-separated regexps must all match, in any order.
Quote patterns containing spaces.  Use C-u before this command for raw fd syntax.")
              :prompt "Project files (AND): "
              :buffer "*helm fd*")))))

(defun lx/helm-dired-histories--run (candidates &optional prompt buffer)
  "Run Helm over CANDIDATES with optional PROMPT and BUFFER."
  (helm :prompt (or prompt "Dired Histories: ")
        :buffer (or buffer "*helm-dired-histories*")
        :sources
        (list (helm-build-sync-source (format "%s History" "Dired")
                :header-name (lambda (name)
                               (concat name (substitute-command-keys
                                             helm-find-files-doc-header)))
                :mode-line  helm-read-file-name-mode-line-string
                :candidates candidates
                :nohighlight t
                :fuzzy-match t
                :action 'find-file))))

;;;###autoload
(defun lx/helm-dired-histories ()
  (interactive)
  (lx/helm-dired-histories--run helm-dired-history-variable))

;;;###autoload
(defun lx/helm-remote-dired-histories ()
  "Like `lx/helm-dired-histories', but filter to current host in /rpc: or `ssh-zsh-ghostel-mode' buffers."
  (interactive)
  (let* ((remote-host (and (or (eq 'ssh-zsh-ghostel-mode major-mode)
                               (string-prefix-p "/rpc:" default-directory))
                           (lx/get-remote-buffer-host)))
         (candidates (if remote-host
                         (let ((pattern (concat "\\`/[^:]+:" (regexp-quote remote-host) ":")))
                           (seq-filter (lambda (dir) (string-match-p pattern dir))
                                       helm-dired-history-variable))
                       helm-dired-history-variable)))
    (lx/helm-dired-histories--run
     candidates
     (if remote-host
         (format "Dired Histories (%s): " remote-host)
       "Dired Histories: ")
     (if remote-host
         (format "*helm-dired-histories: %s*" remote-host)
       "*helm-dired-histories*"))))

;;;###autoload
(defun lx/projectile-dired-histories ()
  (cond ((with-helm-current-buffer (lx/is-remote-buffer))
         (with-helm-current-buffer (let ((remote-host (lx/get-remote-buffer-host)))
                                     (seq-filter (lambda (dir) (s-matches? (concat remote-host ":") dir)) helm-dired-history-variable))))
        ((projectile-project-p) (let ((project-root (projectile-project-root)))
                                  (seq-filter (lambda (dir) (s-starts-with? project-root dir)) helm-dired-history-variable)))
        (t helm-dired-history-variable)))

;;;###autoload
(defun lx/helm-projectile-dired-histories ()
  (interactive)
  (helm :prompt "Project Dired Histories: "
        :buffer "*helm-projectile-dired-histories*"
        :sources
        (list (helm-build-sync-source "Projectile Dired History"
                :header-name (lambda (name)
                               (concat name (substitute-command-keys
                                             helm-find-files-doc-header)))
                :mode-line  helm-read-file-name-mode-line-string
                :candidates 'lx/projectile-dired-histories
                :nohighlight t
                :fuzzy-match t
                :action 'find-file))))
