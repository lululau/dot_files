;;; zsh-expn-dired.el --- Run a zsh expansion command and Dired the output -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This package provides `zsh-expn-dired', which runs a zsh glob expansion
;; and opens a Dired buffer on the output.
;;
;; Example pattern:
;;   **/*(Lm+100OL)         ; recursively list files >= 100MB, sorted by size descending
;;   *(.om[1,10])           ; 10 most recently modified plain files
;;   ls -lf **/*(Lm+100OL)   ; explicit ls command with flags

;;; Code:

(require 'dired)
(require 'subr-x)

(defgroup zsh-expn-dired nil
  "Run a `zsh' expansion command and Dired the output."
  :group 'dired
  :prefix "zsh-expn-")

(defcustom zsh-expn-program "zsh"
  "The zsh executable to use."
  :type 'string
  :group 'zsh-expn-dired)

(defcustom zsh-expn-ls-program "ls"
  "The default `ls' program to use."
  :type 'string
  :group 'zsh-expn-dired)

(defcustom zsh-expn-ls-switches "-lfdh"
  "Default ls switches to use with zsh expansion.
The switches should include `-l' for long listing and `-f' to preserve
zsh's glob sort order.  `-d' is recommended to list directories themselves
rather than their contents."
  :type 'string
  :group 'zsh-expn-dired)

(defcustom zsh-expn-ls-option
  (cons "-lfdh" "-ld")
  "Option passed to ls and switches given to Dired.
The car is the default `ls' switches used in zsh commands.
The cdr is the switch string passed to `dired-mode'."
  :type '(cons (string :tag "Ls Switches")
	       (string :tag "Dired Switches"))
  :group 'zsh-expn-dired)

(defcustom zsh-expn-ls-subdir-switches "-al"
  "`ls' switches for inserting subdirectories in `*zsh-expn*' buffers."
  :type 'string
  :group 'zsh-expn-dired)

(defvar zsh-expn-pattern nil
  "Last pattern given to `zsh-expn-dired'.")

(defvar zsh-expn-history '("**/*(Lm+100OL)")
  "History of zsh glob patterns entered in the minibuffer.")

(defvar dired-sort-inhibit)

(defun zsh-expn-dired--parse-input (input)
  "Parse user INPUT into a plist `(:cmd ... :switches ... :pattern ...)'.
INPUT can be a bare glob pattern like `**/*(Lm+100OL)',
or a full ls invocation like `ls -lf **/*(Lm+100OL)'."
  (let ((re "\\`[ \t]*\\(?:\\([a-zA-Z0-9_/-]*ls\\)[ \t]+\\)?\\(?:\\(-[a-zA-Z0-9]+\\)[ \t]+\\)?\\(.*\\)\\'")
        (trimmed (string-trim (or input ""))))
    (if (string-match re trimmed)
        (let ((cmd (match-string 1 trimmed))
              (switches (match-string 2 trimmed))
              (pat (match-string 3 trimmed)))
          (list :cmd (if (and cmd (not (string-empty-p cmd)))
                         cmd
                       zsh-expn-ls-program)
                :switches (if (and switches (not (string-empty-p switches)))
                              switches
                            (car zsh-expn-ls-option))
                :pattern (if (and pat (not (string-empty-p (string-trim pat))))
                             (string-trim pat)
                           "*(.)")))
      (list :cmd zsh-expn-ls-program
            :switches (car zsh-expn-ls-option)
            :pattern (if (string-empty-p trimmed) "*(.)" trimmed)))))

;;;###autoload
(defun zsh-expn-dired (dir pattern)
  "Run a zsh expansion in DIR and go into Dired mode on the output.
PATTERN is a zsh wildcard / glob expression (e.g. `**/*(Lm+100OL)').
You may also provide a full command starting with `ls' (e.g. `ls -lf **/*(Lm+100OL)')."
  (interactive
   (let* ((default (or zsh-expn-pattern "**/*(Lm+100OL)"))
          (prompt (format "Run zsh (pattern, default %s): " default))
          (d (read-directory-name "Run zsh expansion in directory: " nil "" t))
          (pat (read-string prompt nil '(zsh-expn-history . 1) default)))
     (list d pat)))
  (let ((dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a trailing slash.
    (setq dir (file-name-as-directory (expand-file-name dir)))
    (or (file-directory-p dir)
        (error "zsh-expn-dired needs a directory: %s" dir))
    (switch-to-buffer (get-buffer-create "*zsh-expn*"))

    ;; See if there's still a process running, and offer to kill it.
    (let ((proc (get-buffer-process (current-buffer))))
      (when proc
        (if (or (not (eq (process-status proc) 'run))
                (yes-or-no-p
                 (format-message "A `zsh-expn' process is running; kill it? ")))
            (condition-case nil
                (progn
                  (interrupt-process proc)
                  (sit-for 1)
                  (delete-process proc))
              (error nil))
          (error "Cannot have two processes in `%s' at once" (buffer-name)))))

    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir
          zsh-expn-pattern pattern)

    (let* ((parsed (zsh-expn-dired--parse-input pattern))
           (cmd (plist-get parsed :cmd))
           (switches (plist-get parsed :switches))
           (pat (plist-get parsed :pattern))
           (display-cmd (format "%s %s %s" cmd switches pat))
           (script (format "setopt null_glob; files=( %s ); if (( ${#files} > 2000 )); then print -rNC1 -- \"${files[@]}\" | xargs -0 %s %s; elif (( ${#files} )); then %s %s -- \"${files[@]}\"; fi"
                           pat cmd switches cmd switches))
           (proc (start-process "zsh-expn" (current-buffer)
                                zsh-expn-program "-c" script)))

      (dired-mode dir (or switches (cdr zsh-expn-ls-option)))
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map "\C-c\C-k" #'kill-zsh-expn)
        (use-local-map map))

      (make-local-variable 'dired-sort-inhibit)
      (setq dired-sort-inhibit t)
      (set (make-local-variable 'revert-buffer-function)
           `(lambda (ignore-auto noconfirm)
              (zsh-expn-dired ,dir ,zsh-expn-pattern)))

      ;; Set subdir-alist so that Tree Dired will work:
      (if (fboundp 'dired-simple-subdir-alist)
          (dired-simple-subdir-alist)
        (set (make-local-variable 'dired-subdir-alist)
             (list (cons default-directory (point-min-marker)))))
      (set (make-local-variable 'dired-subdir-switches) zsh-expn-ls-subdir-switches)

      (setq buffer-read-only nil)
      ;; Subdir headerline must come first because the first marker in subdir-alist points there.
      (insert "  " dir ":\n")
      ;; Header command line
      (let ((point (point)))
        (insert "  " display-cmd "\n")
        (dired-insert-set-properties point (point)))
      (setq buffer-read-only t)

      (set-process-filter proc #'zsh-expn-dired-filter)
      (set-process-sentinel proc #'zsh-expn-dired-sentinel)
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) (point) (current-buffer))
      (setq mode-line-process '(":%s")))))

(defun kill-zsh-expn ()
  "Kill the `zsh-expn' process running in the current buffer."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc (eq (process-status proc) 'run)
         (eq (process-filter proc) #'zsh-expn-dired-filter)
         (condition-case nil
             (delete-process proc)
           (error nil)))))

(defun zsh-expn-dired-filter (proc string)
  "Filter for \\[zsh-expn-dired] processes."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (let ((buffer-read-only nil)
                    (beg (point-max))
                    (l-opt (and (consp zsh-expn-ls-option)
                                (string-match "l" (cdr zsh-expn-ls-option))))
                    (ls-regexp (concat "^ +[^ \t\r\n]+\\( +[^ \t\r\n]+\\) +"
                                       "[^ \t\r\n]+ +[^ \t\r\n]+\\( +[0-9]+\\)")))
                (goto-char beg)
                (insert string)
                (goto-char beg)
                (or (looking-at "^")
                    (forward-line 1))
                (while (and (not (eobp)) (looking-at "^"))
                  (insert "  ")
                  (forward-line 1))
                ;; Convert ` ./FILE' to ` FILE'
                ;; This would lose if the current chunk of output
                ;; starts or ends within the ` ./', so back up a bit:
                (goto-char (- beg 3))
                (while (search-forward " ./" nil t)
                  (delete-region (point) (- (point) 2)))
                ;; Pad the number of links and file size.
                (when l-opt
                  (goto-char beg)
                  (goto-char (line-beginning-position))
                  (while (re-search-forward ls-regexp nil t)
                    (replace-match (format "%4s" (match-string 1))
                                   nil nil nil 1)
                    (replace-match (format "%9s" (match-string 2))
                                   nil nil nil 2)
                    (forward-line 1)))
                ;; Add text properties to complete lines.
                (goto-char (point-max))
                (if (search-backward "\n" (process-mark proc) t)
                    (progn
                      (dired-insert-set-properties (process-mark proc)
                                                   (1+ (point)))
                      (move-marker (process-mark proc) (1+ (point)))))))))
      ;; The buffer has been killed.
      (delete-process proc))))

(defun zsh-expn-dired-sentinel (proc state)
  "Sentinel for \\[zsh-expn-dired] processes."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (let ((buffer-read-only nil))
            (save-excursion
              (goto-char (point-max))
              (let ((point (point)))
                (insert "\n  zsh-expn " state)
                (forward-char -1)
                (insert " at " (substring (current-time-string) 0 19))
                (dired-insert-set-properties point (point)))
              (setq mode-line-process
                    (concat ":"
                            (symbol-name (process-status proc))))
              (delete-process proc)
              (force-mode-line-update)))
          (message "zsh-expn-dired %s finished." (current-buffer))))))

(provide 'zsh-expn-dired)

;;; zsh-expn-dired.el ends here
