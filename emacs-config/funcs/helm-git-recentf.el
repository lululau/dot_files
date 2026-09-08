;; -*- lexical-binding: t; -*-

(defvar helm-git-recentf--cache (make-hash-table :test #'equal)
  "Cache of tracked file lists for `helm-git-recentf', keyed by repo root.")

(defun helm-git-recentf--root ()
  "Return the Git toplevel for the current buffer, or nil."
  (or (and (fboundp 'magit-toplevel)
           (ignore-errors (magit-toplevel)))
      (and (fboundp 'vc-git-root)
           (vc-git-root default-directory))
      (locate-dominating-file default-directory ".git")))

(defun helm-git-recentf--commit-times (root oids)
  "Return a hash table mapping commit OIDS under ROOT to unix timestamps."
  (let ((times (make-hash-table :test #'equal)))
    (when oids
      (with-temp-buffer
        (let ((default-directory root))
          (dolist (oid oids)
            (insert oid "\n"))
          (unless (zerop (call-process-region
                          (point-min) (point-max)
                          "git" t t nil
                          "log" "--pretty=format:%H\t%ct"
                          "--no-walk=unsorted" "--stdin"))
            (error "git log --no-walk failed")))
        (goto-char (point-min))
        (while (re-search-forward "\\([0-9a-f]+\\)\t\\([0-9]+\\)" nil t)
          (puthash (match-string 1)
                   (string-to-number (match-string 2))
                   times))))
    times))

(defun helm-git-recentf--compute-tracked-files (root)
  "Return tracked repo-relative paths under ROOT by last commit mtime."
  (let ((default-directory root)
        (entries nil)
        (oids nil)
        (seen (make-hash-table :test #'equal)))
    (with-temp-buffer
      (unless (zerop (call-process "git" nil t nil
                                   "last-modified" "--recursive" "-z"))
        (error "git last-modified failed"))
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9a-f]+\\)\t\\([^\0]*\\)\0" nil t)
        (let* ((oid (match-string 1))
               (path (match-string 2))
               (abs (expand-file-name path root)))
          (unless (file-directory-p abs)
            (push (cons oid path) entries)
            (unless (gethash oid seen)
              (puthash oid t seen)
              (push oid oids))))))
    (let ((times (helm-git-recentf--commit-times root oids)))
      (mapcar #'cdr
              (sort entries
                    (lambda (a b)
                      (> (gethash (car a) times 0)
                         (gethash (car b) times 0))))))))

(defun helm-git-recentf--tracked-files (root &optional force)
  "Return cached tracked last-modified files for ROOT.
With FORCE non-nil, recompute and refresh the cache."
  (when force
    (remhash root helm-git-recentf--cache))
  (or (gethash root helm-git-recentf--cache)
      (puthash root (helm-git-recentf--compute-tracked-files root)
               helm-git-recentf--cache)))

(defun helm-git-recentf--untracked-files (root)
  "Return untracked (non-ignored) repo-relative paths under ROOT by mtime.
Always recomputed: untracked files change frequently and listing is cheap."
  (let ((default-directory root)
        (entries nil))
    (with-temp-buffer
      (unless (zerop (call-process "git" nil t nil
                                   "ls-files" "--others" "--exclude-standard" "-z"))
        (error "git ls-files --others failed"))
      (goto-char (point-min))
      (while (re-search-forward "\\([^\0]+\\)\0" nil t)
        (let* ((path (match-string 1))
               (abs (expand-file-name path root))
               (attrs (and (not (file-directory-p abs))
                           (file-attributes abs))))
          (when attrs
            (push (cons (file-attribute-modification-time attrs) path)
                  entries)))))
    (mapcar #'cdr
            (sort entries
                  (lambda (a b)
                    (time-less-p (car b) (car a)))))))

(defun helm-git-recentf--file-source (name files root)
  "Build a Helm sync source named NAME for FILES under ROOT."
  (helm-build-sync-source name
    :candidates
    (lambda ()
      (helm-projectile--files-display-real files root))
    :fuzzy-match helm-projectile-fuzzy-match
    :keymap helm-projectile-find-file-map
    :help-message 'helm-ff-help-message
    :mode-line helm-read-file-name-mode-line-string
    :action helm-projectile-file-actions
    :persistent-action #'helm-projectile-file-persistent-action
    :persistent-help "Preview file"))

;;;###autoload
(defun helm-git-recentf (&optional arg)
  "Find files in the current Git repository by recent modification.
Shows two Helm sources:
- Git untracked: non-ignored untracked files, sorted by filesystem mtime
- Git recent files: tracked files via `git last-modified --recursive',
  sorted by last commit time

With prefix ARG, refresh the tracked-file cache first.
If the current buffer is not inside a Git repository, signal
\"Not git repository\"."
  (interactive "P")
  (require 'helm-projectile)
  (let ((root (helm-git-recentf--root)))
    (unless root
      (user-error "Not git repository"))
    (let* ((helm-ff-transformer-show-only-basename nil)
           (helm-boring-file-regexp-list nil)
           (root (file-name-as-directory (expand-file-name root)))
           (untracked (helm-git-recentf--untracked-files root))
           (tracked (helm-git-recentf--tracked-files root arg))
           (base-name (file-name-nondirectory (directory-file-name root))))
      (helm :sources (list
                      (helm-git-recentf--file-source
                       "Git untracked" untracked root)
                      (helm-git-recentf--file-source
                       "Git recent files" tracked root))
            :buffer (concat "*helm git recentf: " base-name "*")
            :truncate-lines helm-projectile-truncate-lines
            :prompt "Recently modified file: "))))
