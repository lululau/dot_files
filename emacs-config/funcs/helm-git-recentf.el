;; -*- lexical-binding: t; -*-

(defvar helm-git-recentf--cache (make-hash-table :test #'equal)
  "Cache of relative file lists for `helm-git-recentf', keyed by repo root.")

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

(defun helm-git-recentf--compute-files (root)
  "Return repo-relative paths under ROOT ordered by last modification time."
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

(defun helm-git-recentf--files (root &optional force)
  "Return cached last-modified files for ROOT.
With FORCE non-nil, recompute and refresh the cache."
  (when force
    (remhash root helm-git-recentf--cache))
  (or (gethash root helm-git-recentf--cache)
      (puthash root (helm-git-recentf--compute-files root)
               helm-git-recentf--cache)))

;;;###autoload
(defun helm-git-recentf (&optional arg)
  "Find files in the current Git repository by last commit modification time.
Uses `git last-modified --recursive'.  With prefix ARG, refresh the cache
first.  If the current buffer is not inside a Git repository, signal
\"Not git repository\"."
  (interactive "P")
  (require 'helm-projectile)
  (let ((root (helm-git-recentf--root)))
    (unless root
      (user-error "Not git repository"))
    (let* ((helm-ff-transformer-show-only-basename nil)
           (helm-boring-file-regexp-list nil)
           (root (file-name-as-directory (expand-file-name root)))
           (files (helm-git-recentf--files root arg))
           (base-name (file-name-nondirectory (directory-file-name root))))
      (helm :sources (list
                      (helm-build-sync-source "Git recent files"
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
            :buffer (concat "*helm git recentf: " base-name "*")
            :truncate-lines helm-projectile-truncate-lines
            :prompt "Recently modified file: "))))
