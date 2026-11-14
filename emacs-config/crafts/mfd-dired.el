;;; mfd-dired.el --- Run Rust `mfd' (Spotlight) and Dired the output  -*- lexical-binding: t; -*-

;; Based on the Rust MFD: https://github.com/lululau/mfd  (mfd-rs)

(require 'dired)
(require 'subr-x)

(defgroup mfd-dired nil
  "Run an `mfd' command and Dired the output."
  :group 'dired
  :prefix "mfd-")

(defcustom mfd-program "mfd"
  "Name or path of the `mfd' (Rust) executable."
  :type 'string
  :group 'mfd-dired)

(defcustom mfd-ls-switches "-dilsb"
  "`ls' switches used to list each file found by `mfd'.
Must contain -d (and -l for a long listing); -b quotes odd file names."
  :type 'string
  :group 'mfd-dired)

(defcustom mfd-ls-subdir-switches "-alb"
  "`ls' switches for inserting subdirectories in `*mfd*' buffers."
  :type 'string
  :group 'mfd-dired)

(defcustom mfd-name-arg "-n"
  "Argument used to specify a file name pattern (substring or glob)."
  :type 'string
  :group 'mfd-dired)

(defvar mfd-args nil
  "Last arguments given to `mfd' by \\[mfd-dired].")

(defvar mfd-args-history nil)

(defvar dired-sort-inhibit)

(defun mfd-dired--split (string)
  "Split STRING into arguments using shell quoting rules."
  (if (fboundp 'split-string-shell-command)
      (split-string-shell-command string)
    (split-string-and-unquote string)))

(defun mfd-dired--executable ()
  "Return the full path of the mfd executable, or signal an error."
  (or (executable-find mfd-program)
      (let ((cargo (expand-file-name "~/.cargo/bin/mfd")))
        (and (file-executable-p cargo) cargo))
      (user-error "Cannot find `%s'; install with `cargo install mfd'"
                  mfd-program)))

(defun mfd-dired--command (program args dir)
  "Build the argv list running PROGRAM with ARGS (a string) in DIR.
Results are NUL separated, then listed by `ls'."
  (append
   (list "sh" "-c"
         (concat "\"$0\" \"$@\" -0 | xargs -0 ls "
                 (shell-quote-argument mfd-ls-switches))
         program)
   (mfd-dired--split args)
   (list (directory-file-name dir))))

;;;###autoload
(defun mfd-dired (dir args)
  "Run `mfd' with ARGS in DIR and go into Dired mode on the output.
The command is essentially

    mfd ARGS -0 DIR | xargs -0 ls -dilsb"
  (interactive (list (read-directory-name "Run mfd in directory: " nil "" t)
                     (read-string "Run mfd (with args): " mfd-args
                                  '(mfd-args-history . 1))))
  (let ((dired-buffers dired-buffers)
        (program (mfd-dired--executable)))
    (setq dir (file-name-as-directory (expand-file-name dir)))
    (or (file-directory-p dir)
        (error "mfd-dired needs a directory: %s" dir))
    (switch-to-buffer (get-buffer-create "*mfd*"))
    (let ((old (get-buffer-process (current-buffer))))
      (when old
        (if (or (not (eq (process-status old) 'run))
                (yes-or-no-p "A `mfd' process is running; kill it? "))
            (ignore-errors (delete-process old))
          (error "Cannot have two processes in `%s' at once" (buffer-name)))))
    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir
          mfd-args args)
    (dired-mode dir mfd-ls-switches)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map "\C-c\C-k" #'kill-mfd)
      (use-local-map map))
    (setq-local dired-sort-inhibit t)
    (let ((saved-args args))
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (mfd-dired dir saved-args))))
    (setq-local dired-subdir-alist
                (list (cons default-directory (point-min-marker))))
    (setq-local dired-subdir-switches mfd-ls-subdir-switches)
    (setq buffer-read-only nil)
    (insert "  " dir ":\n")
    (let ((point (point)))
      (insert "  " (string-join (cons (file-name-nondirectory program)
                                      (mfd-dired--split args))
                                " ")
              "\n")
      (dired-insert-set-properties point (point)))
    (setq buffer-read-only t)
    (let* ((errbuf (generate-new-buffer " *mfd-stderr*"))
           (proc (make-process
                  :name "mfd"
                  :buffer (current-buffer)
                  :command (mfd-dired--command program args dir)
                  :connection-type 'pipe
                  :noquery t
                  :stderr errbuf
                  :filter #'mfd-dired-filter
                  :sentinel #'mfd-dired-sentinel)))
      (process-put proc 'mfd-stderr errbuf)
      (process-put proc 'mfd-pending "")
      (move-marker (process-mark proc) (point-max) (current-buffer)))
    (setq mode-line-process '(":%s"))))

(defun kill-mfd ()
  "Kill the `mfd' process running in the current buffer."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc (eq (process-status proc) 'run)
         (eq (process-filter proc) #'mfd-dired-filter)
         (ignore-errors (delete-process proc)))))

;;;###autoload
(defun mfd-name-dired (dir pattern)
  "Run `mfd' in DIR, listing files whose name matches PATTERN."
  (interactive
   (list (read-directory-name "mfd-name (directory): " nil "" t)
         (read-string "mfd-name (filename pattern): ")))
  (mfd-dired dir (concat mfd-name-arg " " (shell-quote-argument pattern))))

;;;###autoload
(defun mfd-grep-dired (dir text)
  "Run `mfd' in DIR, listing files whose content matches TEXT (-q)."
  (interactive
   (list (read-directory-name "mfd-grep (directory): " nil "" t)
         (read-string "mfd-grep (content text): ")))
  (mfd-dired dir (concat "-q " (shell-quote-argument text))))

(defalias 'mfd-content-dired #'mfd-grep-dired)
(defalias 'lookfor-dired #'mfd-grep-dired)

(defun mfd-dired-filter (proc string)
  "Filter for `mfd-dired' processes: insert complete lines, indented."
  (let ((buf (process-buffer proc)))
    (if (buffer-live-p buf)
        (with-current-buffer buf
          (let* ((inhibit-read-only t)
                 (data (concat (process-get proc 'mfd-pending) string))
                 (end (let ((i (string-match-p "\n[^\n]*\\'" data)))
                        (and i (1+ i)))))
            (if (not end)
                (process-put proc 'mfd-pending data)
              (process-put proc 'mfd-pending (substring data end))
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (process-mark proc))
                  (let ((beg (point)))
                    (dolist (line (split-string (substring data 0 end) "\n" t))
                      (insert "  " line "\n"))
                    (dired-insert-set-properties beg (point))
                    (set-marker (process-mark proc) (point))))))))
      (delete-process proc))))

(defun mfd-dired-sentinel (proc state)
  "Sentinel for `mfd-dired' processes."
  (let ((buf (process-buffer proc))
        (errbuf (process-get proc 'mfd-stderr)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (err (and (buffer-live-p errbuf)
                        (string-trim (with-current-buffer errbuf
                                       (buffer-string))))))
          (save-excursion
            (goto-char (point-max))
            (let ((point (point)))
              (when (and err (not (string-empty-p err)))
                (dolist (l (split-string err "\n" t))
                  (insert "  mfd: " l "\n")))
              (insert "\n  mfd " (string-trim-right state))
              (insert " at " (substring (current-time-string) 0 19) "\n")
              (dired-insert-set-properties point (point))))
          (setq mode-line-process (concat ":" (symbol-name (process-status proc))))
          (force-mode-line-update)
          (message "mfd-dired %s finished." (current-buffer)))))
    (when (buffer-live-p errbuf) (kill-buffer errbuf))))

(provide 'mfd-dired)

;;; mfd-dired.el ends here
