(add-hook 'evil-collection-setup-hook #'(lambda (&rest args)
                                          (evil-define-key 'normal dired-mode-map [?\S-\ ] nil)))

(with-eval-after-load 'dired
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
  (define-key dired-mode-map (kbd "gr") #'spacemacs/safe-revert-buffer)
  (define-key dired-mode-map (kbd "C-L") #'dired-do-symlink)
  (with-eval-after-load 'evil-collection-dired
    (evil-define-key 'normal dired-mode-map (kbd "f") 'spacemacs/helm-find-files)
    (evil-define-key 'normal dired-mode-map (kbd "F") 'spacemacs/helm-find-files-recursively)
    (evil-define-key 'normal dired-mode-map (kbd "s") 'dired-sort-toggle-or-edit)
    (evil-define-key 'normal dired-mode-map (kbd "S") 'hydra-dired-quick-sort/body))

  (unless (or (display-graphic-p) (lx/system-is-linux))
    (defun dired-delete-file (file &optional recursive trash)
      (call-process "trash" nil nil nil file)))

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
               (set (make-local-variable 'dired-dotfiles-show-p) t))))))

(with-eval-after-load 'dired-x
  (define-key dired-mode-map (kbd "N") nil))

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
          ("\\.zip\\'" . "zip %o -r --filesync %i"))))

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
