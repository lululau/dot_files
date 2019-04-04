(global-set-key (kbd "s-O") 'spacemacs/helm-find-files-recursively)

(defvar helm-ff--directory-files-recursively-hash (make-hash-table :test 'equal))
(setq helm-source-find-files-recursively nil)


(defun spacemacs/helm-find-files-recursively (arg)
  "Custom spacemacs implementation for calling helm-find-files-1.
Removes the automatic guessing of the initial value based on thing at point. "
  (interactive "P")
  ;; fixes #10882 and #11270
  (require 'helm-files)
  (let* ((hist (and arg helm-ff-history (helm-find-files-history)))
         (default-input hist)
         (input (cond ((and (eq major-mode 'dired-mode) default-input)
                       (file-name-directory default-input))
                      ((and (not (string= default-input ""))
                            default-input))
                      (t (expand-file-name (helm-current-directory))))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1-recursively input)))

(defun helm-find-files-1-recursively (fname &optional preselect)
  "Find FNAME filename with PRESELECT filename preselected.

Use it for non--interactive calls of `helm-find-files'."
  (require 'tramp)
  ;; Resolve FNAME now outside of helm.
  ;; [FIXME] When `helm-find-files-1' is used directly from lisp
  ;; and FNAME is an abbreviated path, for some reasons
  ;; `helm-update' is called many times before resolving
  ;; the abbreviated path (Issue #1939) so be sure to pass a
  ;; full path to helm-find-files-1.
  (unless (string-match-p helm-ff-url-regexp fname)
    (setq fname (expand-file-name (substitute-in-file-name fname))))
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (setq helm-find-files--toggle-bookmark nil)
  (let* ( ;; Be sure we don't erase the precedent minibuffer if some.
         (helm-ff-auto-update-initial-value
          (and helm-ff-auto-update-initial-value
               (not (minibuffer-window-active-p (minibuffer-window)))))
         (tap (thing-at-point 'filename))
         (def (and tap (or (file-remote-p tap)
                           (expand-file-name tap)))))
    (helm-set-local-variable 'helm-follow-mode-persistent nil)
    (unless helm-source-find-files-recursively
      (setq helm-source-find-files-recursively (helm-make-source
                                    "Find Files" 'helm-source-ffiles-recursively)))
    (when (helm-attr 'follow helm-source-find-files-recursively)
      (helm-attrset 'follow -1 helm-source-find-files-recursively))
    (helm-ff-setup-update-hook)
    (add-hook 'helm-resume-after-hook 'helm-ff--update-resume-after-hook)
    (unwind-protect
         (helm :sources 'helm-source-find-files-recursively
               :input fname
               :case-fold-search helm-file-name-case-fold-search
               :preselect preselect
               :ff-transformer-show-only-basename
               helm-ff-transformer-show-only-basename
               :default def
               :prompt "Find files or url: "
               :buffer "*helm find files*")
      (helm-ff--update-resume-after-hook nil t)
      (setq helm-ff-default-directory nil))))

(defclass helm-source-ffiles-recursively (helm-source-sync)
  ((header-name
    :initform (lambda (name)
                (concat name (substitute-command-keys
                              helm-find-files-doc-header))))
   (init
    :initform (lambda ()
                (setq helm-ff-auto-update-flag
                      helm-ff-auto-update-initial-value)
                (setq helm-ff--auto-update-state
                      helm-ff-auto-update-flag)
                (helm-set-local-variable 'bookmark-make-record-function
                                         #'helm-ff-make-bookmark-record)
                (require 'helm-external)))
   (candidates :initform 'helm-find-files-get-candidates-recursively)
   (filtered-candidate-transformer
    :initform '(helm-ff-sort-candidates
                (lambda (candidates _source)
                  (cl-loop for f in candidates
                           for ff = (lx/helm-ff-filter-candidate f)
                           when ff collect ff))))
   (persistent-action-if :initform 'helm-find-files-persistent-action-if)
   (persistent-help :initform "Hit1 Expand Candidate, Hit2 or (C-u) Find file")
   (help-message :initform 'helm-ff-help-message)
   (mode-line :initform (list "File(s)" helm-mode-line-string))
   (volatile :initform t)
   (cleanup :initform 'helm-find-files-cleanup)
   (migemo :initform t)
   (nohighlight :initform t)
   (keymap :initform helm-find-files-map)
   (candidate-number-limit :initform 'helm-ff-candidate-number-limit)
   (action-transformer
    :initform 'helm-find-files-action-transformer)
   (action :initform 'helm-find-files-actions)
   (before-init-hook :initform 'helm-find-files-before-init-hook)
   (after-init-hook :initform 'helm-find-files-after-init-hook)
   (group :initform 'helm-files)))

(defun helm-find-files-get-candidates-recursively (&optional require-match)
  "Create candidate list for `helm-source-find-files'."
  (let* ((path          (helm-ff-set-pattern helm-pattern))
         (dir-p         (file-accessible-directory-p path))
         basedir
         invalid-basedir
         non-essential
         (tramp-verbose helm-tramp-verbose)) ; No tramp message when 0.
    ;; Tramp check if path is valid without waiting a valid
    ;; connection and may send a file-error.
    (setq helm--ignore-errors (file-remote-p path))
    (set-text-properties 0 (length path) nil path)
    ;; Issue #118 allow creation of newdir+newfile.
    (unless (or
             ;; A tramp file name not completed.
             (string= path "Invalid tramp file name")
             ;; An empty pattern
             (string= path "")
             (and (string-match-p ":\\'" path)
                  (helm-ff--tramp-postfixed-p path))
             ;; Check if base directory of PATH is valid.
             (helm-aif (file-name-directory path)
                 ;; If PATH is a valid directory IT=PATH,
                 ;; else IT=basedir of PATH.
                 (file-directory-p it)))
      ;; BASEDIR is invalid, that's mean user is starting
      ;; to write a non--existing path in minibuffer
      ;; probably to create a 'new_dir' or a 'new_dir+new_file'.
      (setq invalid-basedir t))
    ;; Don't set now `helm-pattern' if `path' == "Invalid tramp file name"
    ;; like that the actual value (e.g /ssh:) is passed to
    ;; `helm-ff--tramp-hostnames'.
    (unless (or (string= path "Invalid tramp file name")
                invalid-basedir)      ; Leave  helm-pattern unchanged.
      (setq helm-ff-auto-update-flag  ; [1]
            ;; Unless auto update is disabled start auto updating only
            ;; at third char.
            (unless (or (null helm-ff--auto-update-state)
                        ;; But don't enable auto update when
                        ;; deleting backward.
                        helm-ff--deleting-char-backward
                        (and dir-p (not (string-match-p "/\\'" path))))
              (or (>= (length (helm-basename path)) 3) dir-p)))
      ;; At this point the tramp connection is triggered.
      (helm-log
       "Pattern=%S"
       (setq helm-pattern (helm-ff--transform-pattern-for-completion path)))
      ;; This have to be set after [1] to allow deleting char backward.
      (setq basedir (or (helm-aand
                         (if (and dir-p helm-ff-auto-update-flag)
                             ;; Add the final "/" to path
                             ;; when `helm-ff-auto-update-flag' is enabled.
                             (file-name-as-directory path)
                           (if (string= path "")
                               "/" (file-name-directory path)))
                         (expand-file-name it))
                        default-directory))
      (setq helm-ff-default-directory
            (if (string= helm-pattern "")
                (expand-file-name "/")  ; Expand to "/" or "c:/"
                ;; If path is an url *default-directory have to be nil.
                (unless (or (string-match helm-ff-url-regexp path)
                            (and helm--url-regexp
                                 (string-match helm--url-regexp path)))
                  basedir))))
    (when (and (string-match ":\\'" path)
               (file-remote-p basedir nil t))
      (setq helm-pattern basedir))
    (cond ((string= path "Invalid tramp file name")
           (or (helm-ff--tramp-hostnames) ; Hostnames completion.
               (prog2
                   ;; `helm-pattern' have not been modified yet.
                   ;; Set it here to the value of `path' that should be now
                   ;; "Invalid tramp file name" and set the candidates list
                   ;; to ("Invalid tramp file name") to make `helm-pattern'
                   ;; match single candidate "Invalid tramp file name".
                   (setq helm-pattern path)
                   ;; "Invalid tramp file name" is now printed
                   ;; in `helm-buffer'.
                   (list path))))
          ((or (and (file-regular-p path)
                    (eq last-repeatable-command 'helm-execute-persistent-action))
               ;; `ffap-url-regexp' don't match until url is complete.
               (string-match helm-ff-url-regexp path)
               invalid-basedir
               (and (not (file-exists-p path)) (string-match "/$" path))
               (and helm--url-regexp (string-match helm--url-regexp path)))
           (list path))
          ((string= path "") (helm-ff-directory-files "/"))
          ;; Check here if directory is accessible (not working on Windows).
          ((and (file-directory-p path) (not (file-readable-p path)))
           (list (format "file-error: Opening directory permission denied `%s'" path)))
          ;; A fast expansion of PATH is made only if `helm-ff-auto-update-flag'
          ;; is enabled.
          ((and dir-p helm-ff-auto-update-flag)
           (helm-ff-directory-files-recursively path))
          (t (append (unless (or require-match
                                 ;; Check here if path is an existing
                                 ;; file before adding it to
                                 ;; candidates, it was previously done
                                 ;; in the sort function but this
                                 ;; create a bug with remote files
                                 ;; when path is at the same time a
                                 ;; pattern matching a candidate and a
                                 ;; real candidate e.g. ack and
                                 ;; ack-grep in /usr/bin. This is due
                                 ;; presumably to a latency more
                                 ;; important with remote files which
                                 ;; lead to a confusion with the
                                 ;; pattern matching one candidate and
                                 ;; the real candidate which is same
                                 ;; as pattern.
                                 (file-exists-p path)
                                 ;; When `helm-ff-auto-update-flag' has been
                                 ;; disabled, whe don't want PATH to be added on top
                                 ;; if it is a directory.
                                 dir-p)
                       (list path))
                     (helm-ff-directory-files-recursively basedir))))))

(defun helm-ff-directory-files-recursively (directory)
  "List contents of DIRECTORY.
Argument FULL mean absolute path.
It is same as `directory-files' but always returns the
dotted filename '.' and '..' even on root directories in Windows
systems."
  (setq directory (file-name-as-directory
                   (expand-file-name directory)))
  (let* (file-error
         (ls   (condition-case err
                   (helm-list-directory-recursively directory)
                 ;; Handle file-error from here for Windows
                 ;; because predicates like `file-readable-p' and friends
                 ;; seem broken on emacs for Windows systems (always returns t).
                 ;; This should never be called on GNU/Linux/Unix
                 ;; as the error is properly intercepted in
                 ;; `helm-find-files-get-candidates' by `file-readable-p'.
                 (file-error
                  (prog1
                      (list (format "%s:%s"
                                    (car err)
                                    (mapconcat 'identity (cdr err) " ")))
                    (setq file-error t)))))
        (dot  (concat directory "."))
        (dot2 (concat directory "..")))
    (puthash directory (+ (length ls) 2) helm-ff--directory-files-recursively-hash)
    (append (and (not file-error) (list dot dot2)) ls)))



(defun helm-list-directory-recursively (directory)
  "List directory DIRECTORY.

If DIRECTORY is remote use `helm-list-directory-function' otherwise use
`directory-files'."
  (let* ((remote (file-remote-p directory 'method))
         (helm-list-directory-function
          (if (and remote (not (string= remote "ftp")))
              helm-list-directory-function
            #'helm-list-dir-lisp))
         (remote-fn-p (eq helm-list-directory-function
                          'helm-list-dir-external))
         (sort-method (cl-case helm-ff-initial-sort-method
                        (newest (if (and remote remote-fn-p)
                                    "-t" #'file-newer-than-file-p))
                        (size (if (and remote remote-fn-p)
                                  "-S" #'helm-ff-file-larger-that-file-p))
                        (t nil))))
    (if remote
        (funcall helm-list-directory-function directory sort-method)
        (directory-files-recursively directory directory-files-no-dot-files-regexp t))))

(defun lx/helm-ff-filter-candidate (file)
  "`filter-one-by-one' Transformer function for `helm-source-find-files'."
  ;; Handle boring files
  (let ((basename (s-replace default-directory "" file))
        dot)
    (unless (and helm-ff-skip-boring-files
                 (helm-ff-boring-file-p basename))

      ;; Handle tramp files with minimal highlighting.
      (if (and (or (string-match-p helm-tramp-file-name-regexp helm-pattern)
                   (helm-file-on-mounted-network-p helm-pattern)))
          (let* (hostp
                 (disp (if (and helm-ff-transformer-show-only-basename
                                (not (setq dot (helm-dir-is-dot file))))
                           (or (setq hostp
                                     (helm-ff--get-host-from-tramp-invalid-fname
                                      file))
                               basename)
                         file)))
            ;; Filename with cntrl chars e.g. foo^J
            ;; This will not work as long as most tramp file handlers doesn't
            ;; handle such case, e.g. file-name-all-completions,
            ;; directory-files, file-name-nondirectory etc...
            ;; Keep it though in case they fix this upstream...
            (setq disp (replace-regexp-in-string "[[:cntrl:]]" "?" disp))
            (cond (;; Dot directories . and ..
                   dot (propertize file 'face 'helm-ff-dotted-directory))
                  ;; Directories.
                  ((get-text-property 1 'helm-ff-dir file)
                   (cons (propertize disp 'face 'helm-ff-directory) file))
                  ;; Executable files.
                  ((get-text-property 1 'helm-ff-exe file)
                   (cons (propertize disp 'face 'helm-ff-executable) file))
                  ;; Symlinks.
                  ((get-text-property 1 'helm-ff-sym file)
                   (cons (propertize disp 'face 'helm-ff-symlink) file))
                  ;; Regular files.
                  ((get-text-property 1 'helm-ff-file file)
                   (cons (propertize disp 'face 'helm-ff-file) file))
                  ;; non existing files.
                  (t (cons (helm-ff-prefix-filename
                            (propertize disp 'face 'helm-ff-file)
                            hostp (unless hostp 'new-file))
                           file))))

        ;; Highlight local files showing everything, symlinks, exe,
        ;; dirs etc...
        (let* ((disp (if (and helm-ff-transformer-show-only-basename
                              (not (setq dot (helm-dir-is-dot file)))
                              (not (and helm--url-regexp
                                        (string-match helm--url-regexp file)))
                              (not (string-match helm-ff-url-regexp file)))
                         (or (helm-ff--get-host-from-tramp-invalid-fname file)
                             basename)
                       file))
               (attr (file-attributes file))
               (type (car attr))
               x-bit)
          ;; Filename cntrl chars e.g. foo^J
          (setq disp (replace-regexp-in-string "[[:cntrl:]]" "?" disp))
          (cond ((string-match "file-error" file) file)
                (;; A dead symlink.
                 (and (stringp type)
                      (not (helm-ff-valid-symlink-p file))
                      (not (string-match "^\\.#" basename)))
                 (cons (propertize disp 'face 'helm-ff-invalid-symlink)
                       file))
                ;; A dotted directory symlinked.
                ((and dot (stringp type))
                 (cons (propertize disp 'face 'helm-ff-dotted-symlink-directory)
                       file))
                ;; A dotted directory.
                ((helm-ff-dot-file-p file)
                 (cons (propertize disp 'face 'helm-ff-dotted-directory)
                       file))
                ;; A symlink.
                ((stringp type)
                 (cons (propertize disp 'display
                                   (concat (propertize disp 'face 'helm-ff-symlink)
                                           " -> "
                                           (propertize (abbreviate-file-name type)
                                                       'face 'helm-ff-truename)))
                       file))
                ;; A directory.
                ((eq t type)
                 (cons (propertize disp 'face 'helm-ff-directory)
                       file))
                ;; A character device file.
                ((and attr (string-match
                            "\\`[cp]" (setq x-bit (substring (nth 8 attr) 0 4))))
                 (cons (propertize disp 'face 'helm-ff-pipe)
                       file))
                ;; A socket file.
                ((and attr (string-match "\\`[s]" x-bit))
                 (cons (propertize disp 'face 'helm-ff-socket)
                       file))
                ;; An executable file.
                ((and attr
                      (string-match
                       "x\\'" x-bit))
                 (cons (propertize disp 'face 'helm-ff-executable)
                       file))
                ;; An executable file with suid
                ((and attr (string-match "s\\'" x-bit))
                 (cons (propertize disp 'face 'helm-ff-suid)
                       file))
                ;; A file.
                ((and attr (null type))
                 (cons (propertize disp 'face 'helm-ff-file)
                       file))
                ;; A non--existing file.
                (t (cons (helm-ff-prefix-filename
                          (propertize disp 'face 'helm-ff-file) nil 'new-file)
                         file))))))))

(provide 'helm-find-files-recursively)
