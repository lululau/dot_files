;; -*- lexical-binding: t; -*-

(add-hook 'evil-collection-setup-hook #'(lambda (&rest args)
                                          (evil-define-key 'normal dired-mode-map [?\S-\ ] nil)))

(with-eval-after-load 'dired
  ;; 递归删除/回收不再确认
  (setq dired-recursive-deletes 'always)
  ;; 批量删除也不再弹出确认
  (setq dired-deletion-confirmer (lambda (&rest _) t))

  ;; <mouse-2> 智能行为：目录→展开/折叠子树，文件→在新窗口打开，否则→移动光标
  (defun lx/dired-mouse-2-smart (event)
    "Smart mouse-2 in dired: toggle subtree for dirs, open files, or set point."
    (interactive "e")
    (mouse-set-point event)
    (let ((file (dired-get-filename t t)))
      (cond
        ((and file (file-directory-p file))
        (dired-subtree-toggle))
        (file
        (dired-mouse-find-file-other-window event))
        (t
        (mouse-set-point event)))))

  (define-key dired-mode-map (kbd "n") nil)
  (define-key dired-mode-map (kbd "g") nil)
  (define-key dired-mode-map (kbd "G") nil)
  (define-key dired-mode-map (kbd "?") nil)
  (define-key dired-mode-map (kbd "e") nil)
  (define-key dired-mode-map (kbd "v") nil)
  (define-key dired-mode-map (kbd "V") nil)
  (define-key dired-mode-map (kbd "f") 'spacemacs/helm-find-files)
  (define-key dired-mode-map (kbd "F") 'spacemacs/helm-find-files-recursively)
  (define-key dired-mode-map (kbd ")") 'dired-up-directory)
  (define-key dired-mode-map (kbd "-") 'dired-up-directory)
  (define-key dired-mode-map (kbd "S-SPC") nil)
  (define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "gr") #'revert-buffer)
  (define-key dired-mode-map (kbd "C-L") #'dired-do-relsymlink)
  (define-key dired-mode-map [mouse-2] #'lx/dired-mouse-2-smart)

  (with-eval-after-load 'evil-collection-dired
    (evil-define-key 'normal dired-mode-map (kbd "f") 'spacemacs/helm-find-files)
    (evil-define-key 'normal dired-mode-map (kbd "F") 'spacemacs/helm-find-files-recursively)
    (evil-define-key 'normal dired-mode-map (kbd "s") 'dired-sort-toggle-or-edit)
    (evil-define-key 'normal dired-mode-map (kbd "S") 'hydra-dired-quick-sort/body)
    (evil-define-key 'normal dired-mode-map (kbd "g1") 'dired-jump-to-latest-file)
    ;; mouse-2 必须通过 dired-mode-hook + evil-local-set-key 绑定，
    ;; 因为 evil-collection-dired 创建的 Auxiliary keymap 优先级
    ;; 高于 dired-mode-map 和 evil-define-key，会覆盖常规绑定。
    (add-hook 'dired-mode-hook
              (lambda ()
                (evil-local-set-key 'normal [mouse-2] #'lx/dired-mouse-2-smart)))
    )

  (unless (or (display-graphic-p) (lx/system-is-linux))
    (defun dired-delete-file (file &optional recursive trash)
      (call-process "trash" nil nil nil file)))

  (defun dired-sort-by-date ()
    ;; Toggle between sort by date/name.  Reverts the buffer.
    (let ((sorting-by-date (string-match-p dired-sort-by-date-regexp
                                          dired-actual-switches))
    ;; Regexp for finding (possibly embedded) -t switches.
    (switch-regexp "\\(\\`\\| \\)-\\([a-su-zA-Z]*\\)\\(t\\)\\([^ ]*\\)")
    case-fold-search)
      ;; Remove the -t switch.
      (while (string-match switch-regexp dired-actual-switches)
        (if (and (equal (match-string 2 dired-actual-switches) "")
          (equal (match-string 4 dired-actual-switches) ""))
      ;; Remove a stand-alone -t switch.
      (setq dired-actual-switches
      (replace-match "" t t dired-actual-switches))
    ;; Remove a switch of the form -XtY for some X and Y.
    (setq dired-actual-switches
          (replace-match "" t t dired-actual-switches 3))))

      (setq dired-actual-switches
            (concat dired-actual-switches
                    (if (string-match-p "\\`-[[:alnum:]]+\\'"
                                        dired-actual-switches)
                        "t"
                      " -t"))))
    (dired-sort-set-mode-line)
    (revert-buffer))

  (defun dired-jump-to-latest-file()
    (interactive)
    (dired-sort-by-date)
    (evil-goto-first-line 4))

  (defun dired-dotfiles-toggle ()
    "Show/hide dot-files"
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
          (progn
            (set (make-local-variable 'dired-dotfiles-show-p) nil)
            (message "h")
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        (progn (revert-buffer) ; otherwise just revert to re-show
               (set (make-local-variable 'dired-dotfiles-show-p) t)))))

  (defun dired-backup-file ()
    "Backup file(s) at point by copying to filename.YYYYMMDDHHMMSS.bak.
For directories, copy recursively."
    (interactive)
    (dolist (file (dired-get-marked-files))
      (let ((backup-name (concat file (format-time-string ".%Y%m%d%H%M%S") ".bak")))
        (if (file-directory-p file)
            (copy-directory file backup-name)
          (copy-file file backup-name))
        (message "Backed up: %s -> %s" file backup-name)))
    (revert-buffer))

  (define-advice spacemacs/copy-file-path (:around (orig-fun &rest args) dired-multiple)
    "In Dired, copy marked file paths (if any) or file path at point, newline-separated."
    (if (derived-mode-p 'dired-mode)
        (let* ((marked-files (dired-get-marked-files nil 'marked))
               (files (or marked-files
                          (let ((file (dired-get-filename nil t)))
                            (and file (list file)))))
               (truenames (mapcar #'file-truename files)))
          (if truenames
              (let ((joined-paths (mapconcat #'identity truenames "\n")))
                (kill-new joined-paths)
                (message "%s" joined-paths))
            (user-error "No file at point")))
      (apply orig-fun args)))

  (define-advice spacemacs/copy-file-name (:around (orig-fun &rest args) dired-multiple)
    "In Dired, copy marked filenames (if any) or filename at point, newline-separated."
    (if (derived-mode-p 'dired-mode)
        (let* ((marked-files (dired-get-marked-files nil 'marked))
               (files (or marked-files
                          (let ((file (dired-get-filename nil t)))
                            (and file (list file)))))
               (filenames (mapcar #'file-name-nondirectory files)))
          (if filenames
              (let ((joined-names (mapconcat #'identity filenames "\n")))
                (kill-new joined-names)
                (message "%s" joined-names))
            (user-error "No file at point")))
      (apply orig-fun args)))

  (define-advice spacemacs/copy-directory-path (:around (orig-fun &rest args) dired-multiple)
    "In Dired, copy directory paths of marked items (if any) or item at point, newline-separated."
    (if (derived-mode-p 'dired-mode)
        (let* ((marked-files (dired-get-marked-files nil 'marked))
               (files (or marked-files
                          (let ((file (dired-get-filename nil t)))
                            (and file (list file)))))
               (dir-paths (mapcar (lambda (f)
                                    (if (file-directory-p f)
                                        (file-name-as-directory (file-truename f))
                                      (file-name-directory (file-truename f))))
                                  files))
               (unique-dirs (delete-dups dir-paths)))
          (if unique-dirs
              (let ((joined-dirs (mapconcat #'identity unique-dirs "\n")))
                (kill-new joined-dirs)
                (message "%s" joined-dirs))
            (user-error "No file at point")))
      (apply orig-fun args)))
  )

(with-eval-after-load 'dired-x
  (define-key dired-mode-map (kbd "N") nil))

(with-eval-after-load 'helm-dired-history
  ;; helm-dired-history ignores DEFAULT-FILENAME; honor it so compress etc. can prefill.
  (defun helm-dired-history-read-file-name
      (prompt &optional dir default-filename mustmatch initial predicate)
    (let* ((helm-mode-reverse-history nil)
           (base-dir (or dir default-directory))
           (basename (or initial default-filename))
           (start (cond
                   ((and basename (not (file-name-absolute-p basename)))
                    (expand-file-name basename base-dir))
                   (basename)
                   (dir))))
      (helm-read-file-name prompt
                           :name base-dir
                           :initial-input start
                           :default default-filename
                           :history helm-dired-history-variable))))

(with-eval-after-load 'dired-aux
  (setq dired-compress-file-suffixes
        '(
          ;; "tar -zxf" isn't used because it's not available on the
          ;; Solaris10 version of tar. Solaris10 becomes obsolete in 2021.
          ;; Same thing on AIX 7.1.
          ("\\.rar\\'" "" "rar x %i")
          ("\\.tar\\.gz\\'" "" "gzip -dc %i | tar -xf -")
          ("\\.tgz\\'" "" "gzip -dc %i | tar -xf -")
          ("\\.gz\\'" "" "gunzip")
          ("\\.lz\\'" "" "lzip -d")
          ("\\.Z\\'" "" "uncompress")
          ;; For .z, try gunzip.  It might be an old gzip file,
          ;; or it might be from compact? pack? (which?) but gunzip handles both.
          ("\\.z\\'" "" "gunzip")
          ("\\.dz\\'" "" "dictunzip")
          ("\\.tbz\\'" ".tar" "bunzip2")
          ("\\.bz2\\'" "" "bunzip2")
          ("\\.xz\\'" "" "unxz")
          ("\\.zip\\'" "" "unzip -o -d %o %i")
          ("\\.tar\\.zst\\'" "" "unzstd -c %i | tar -xf -")
          ("\\.tzst\\'" "" "unzstd -c %i | tar -xf -")
          ("\\.zst\\'" "" "unzstd --rm")
          ("\\.7z\\'" "" "7z x -aoa -o%o %i")
          ;; This item controls naming for compression.
          ("\\.tar\\'" ".tgz" nil)
          ;; This item controls the compression of directories.  Its REGEXP
          ;; element should never match any valid file name.
          ("\000" ".tar.gz" "tar -cf - %i | gzip -c9 > %o")))
  (setq dired-compress-files-alist
        '(("\\.tar\\.gz\\'" . "tar -cf - %i | gzip -c9 > %o")
          ("\\.tar\\.bz2\\'" . "tar -cf - %i | bzip2 -c9 > %o")
          ("\\.tar\\.xz\\'" . "tar -cf - %i | xz -c9 > %o")
          ("\\.tar\\.zst\\'" . "tar -cf - %i | zstd -19 -o %o")
          ("\\.rar\\'" . "rar a %o %i")
          ("\\.zip\\'" . "zip %o -r --filesync %i")))

  (defun lx/dired-compress-default-filename ()
    "Default archive name for marked dired entries: basename.tgz."
    (let ((files (dired-get-marked-files nil nil nil nil t)))
      (when files
        (concat (file-name-nondirectory (directory-file-name (car files)))
                ".tgz"))))

  (defun dired-do-compress-to ()
    "Compress selected files and directories to an archive.
Prompt for the archive file name.
Choose the archiving command based on the archive file-name extension
and `dired-compress-files-alist'."
    (interactive nil dired-mode)
    (require 'cl-lib)
    (let* ((in-files (dired-get-marked-files nil nil nil nil t))
           (default-name (lx/dired-compress-default-filename))
           (out-file (expand-file-name
                      (read-file-name "Compress to: "
                                      default-directory
                                      default-name
                                      nil
                                      default-name)))
           (rule (cl-find-if
                  (lambda (x)
                    (string-match (car x) out-file))
                  dired-compress-files-alist)))
      (cond ((not rule)
             (error
              "No compression rule found for %s, see `dired-compress-files-alist'"
              out-file))
            ((and (file-exists-p out-file)
                  (not (y-or-n-p
                        (format "%s exists, overwrite?"
                                (abbreviate-file-name out-file)))))
             (message "Compression aborted"))
            (t
             (when (zerop
                    (dired-shell-command
                     (format-spec (cdr rule)
                                  `((?o . ,(shell-quote-argument
                                            (file-local-name out-file)))
                                    (?i . ,(mapconcat
                                            (lambda (in-file)
                                              (shell-quote-argument
                                               (file-relative-name in-file)))
                                            in-files " "))))))
               (message (ngettext "Compressed %d file to %s"
                                  "Compressed %d files to %s"
                                  (length in-files))
                        (length in-files)
                        (file-name-nondirectory out-file))))))
    (dired-post-do-command)))

(with-eval-after-load 'dired-rsync
  (defun dired-rsync--do-run (command details)
    "Run rsync COMMAND in a unique buffer, passing DETAILS to sentinel."
    (apply #'make-process
           (append (list :name "*rsync*"
                         :buffer (format "%s @ %s"
                                         dired-rsync-proc-buffer-prefix
                                         (current-time-string))
                         :command (list shell-file-name
                                        shell-command-switch
                                        command)
                         :sentinel (lambda (proc desc)
                                     (dired-rsync--sentinel proc desc details))
                         :filter (lambda (proc string)
                                   (dired-rsync--filter proc string)))
                   (list :coding 'mac)))
    (dired-rsync--update-modeline))

  (defun dired-rsync (dest)
    "Asynchronously copy files in dired to `DEST' using rsync.

    `DEST' can be a relative filename and will be processed by
    `expand-file-name' before being passed to the rsync command.

    This function runs the copy asynchronously so Emacs won't block whilst
    the copy is running.  It also handles both source and destinations on
    ssh/scp tramp connections."
    ;; Interactively grab dest if not called with
    (interactive
     (list (helm-dired-history-read-file-name "rsync to: " (dired-dwim-target-directory))))

    (setq dest (expand-file-name dest))

    (let* ((sfiles (funcall dired-rsync-source-files))
           (cmd (dired-rsync--build-cmd sfiles dest)))
      (dired-rsync--do-run cmd
                           (list :marked-files sfiles
                                 :dired-buffer (current-buffer))))))
