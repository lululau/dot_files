(require 'dired)

(defvar fd-program (purecopy "fd")
  "The default fd program.")

(defgroup fd-dired nil
  "Run a `fd' command and Dired the output."
  :group 'dired
  :prefix "fd-")

;; fd's -ls corresponds to these switches.
;; Note -b, at least GNU fd quotes spaces etc. in filenames
(defcustom fd-ls-option
  (cons "-ls" "-dilsb")
  ""
  :version "24.1"	       ; add tests for -ls and -exec + support
  :type '(cons (string :tag "fd Option")
	       (string :tag "Ls Switches"))
  :group 'fd-dired)

(defcustom fd-ls-subdir-switches
  (if (string-match "-[a-z]*b" (cdr fd-ls-option))
      "-alb"
    "-al")
  "`ls' switches for inserting subdirectories in `*fd*' buffers.
This should contain the \"-l\" switch.
Use the \"-F\" or \"-b\" switches if and only if you also use
them for `fd-ls-option'."
  :version "24.1"			; add -b test
  :type 'string
  :group 'fd-dired)

(defvar fd-args nil
  "Last arguments given to `fd' by \\[fd-dired].")

;; History of fd-args values entered in the minibuffer.
(defvar fd-args-history nil)

(defvar dired-sort-inhibit)

;;;###autoload
(defun fd-dired (dir args)
  "Run `fd' and go into Dired mode on a buffer of the output.
The command run (after changing into DIR) is essentially

    fd . \\( ARGS \\) -ls

except that the car of the variable `fd-ls-option' specifies what to
use in place of \"-ls\" as the final argument."
  (interactive (list (read-directory-name "Run fd in directory: " nil "" t)
		     (read-string "Run fd (with args): " fd-args
				  '(fd-args-history . 1))))
  (let ((dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq dir (file-name-as-directory (expand-file-name dir)))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
	(error "fd-dired needs a directory: %s" dir))
    (switch-to-buffer (get-buffer-create "*fd*"))

    ;; See if there's still a `fd' running, and offer to kill
    ;; it first, if it is.
    (let ((fd (get-buffer-process (current-buffer))))
      (when fd
	(if (or (not (eq (process-status fd) 'run))
		(yes-or-no-p
		 (format-message "A `fd' process is running; kill it? ")))
	    (condition-case nil
		(progn
		  (interrupt-process fd)
		  (sit-for 1)
		  (delete-process fd))
	      (error nil))
	  (error "Cannot have two processes in `%s' at once" (buffer-name)))))

    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir
	  fd-args args	      ; save for next interactive call
	  args (concat fd-program
		       " " args " "
		        " . " "| xargs -I {} ls -ld {} "))
    ;; Start the fd process.
    (shell-command (concat args "&") (current-buffer))
    ;; The next statement will bomb in classic dired (no optional arg allowed)
    (dired-mode dir (cdr fd-ls-option))
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map "\C-c\C-k" 'kill-fd)
      (use-local-map map))
    (make-local-variable 'dired-sort-inhibit)
    (setq dired-sort-inhibit t)
    (set (make-local-variable 'revert-buffer-function)
	 `(lambda (ignore-auto noconfirm)
	    (fd-dired ,dir ,fd-args)))
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
	;; will work even with nested dired format (dired-nstd.el,v 1.15
	;; and later)
	(dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (set (make-local-variable 'dired-subdir-alist)
	   (list (cons default-directory (point-min-marker)))))
    (set (make-local-variable 'dired-subdir-switches) fd-ls-subdir-switches)
    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " dir ":\n")
    ;; Make second line a ``fd'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    (let ((point (point)))
      (insert "  " args "\n")
      (dired-insert-set-properties point (point)))
    (setq buffer-read-only t)
    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-filter proc (function fd-dired-filter))
      (set-process-sentinel proc (function fd-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) (point) (current-buffer)))
    (setq mode-line-process '(":%s"))))

(defun kill-fd ()
  "Kill the `fd' process running in the current buffer."
  (interactive)
  (let ((fd (get-buffer-process (current-buffer))))
    (and fd (eq (process-status fd) 'run)
	 (eq (process-filter fd) (function fd-dired-filter))
	 (condition-case nil
	     (delete-process fd)
	   (error nil)))))

;;;###autoload
(defun fd-name-dired (dir pattern)
  ""
  (interactive
   "Dfd-name (directory): \nsfd-name (filename wildcard): ")
  (fd-dired dir (shell-quote-argument pattern)))

;; This functionality suggested by
;; From: oblanc@watcgl.waterloo.edu (Olivier Blanc)
;; Subject: fd-dired, lookfor-dired
;; Date: 10 May 91 17:50:00 GMT
;; Organization: University of Waterloo

(defun fd-dired-filter (proc string)
  ;; Filter for \\[fd-dired] processes.
  (let ((buf (process-buffer proc))
	(inhibit-read-only t))
    (if (buffer-name buf)
	(with-current-buffer buf
	  (save-excursion
	    (save-restriction
	      (widen)
	      (let ((buffer-read-only nil)
		    (beg (point-max))
		    (l-opt (and (consp fd-ls-option)
				(string-match "l" (cdr fd-ls-option))))
		    (ls-regexp (concat "^ +[^ \t\r\n]+\\( +[^ \t\r\n]+\\) +"
				       "[^ \t\r\n]+ +[^ \t\r\n]+\\( +[0-9]+\\)")))
		(goto-char beg)
		(insert string)
		(goto-char beg)
		(or (looking-at "^")
		    (forward-line 1))
		(while (looking-at "^")
		  (insert "  ")
		  (forward-line 1))
		;; Convert ` ./FILE' to ` FILE'
		;; This would lose if the current chunk of output
		;; starts or ends within the ` ./', so back up a bit:
		(goto-char (- beg 3))	; no error if < 0
		(while (search-forward " ./" nil t)
		  (delete-region (point) (- (point) 2)))
		;; Pad the number of links and file size.  This is a
		;; quick and dirty way of getting the columns to line up
		;; most of the time, but it's not foolproof.
		(when l-opt
		  (goto-char beg)
		  (goto-char (line-beginning-position))
		  (while (re-search-forward ls-regexp nil t)
		    (replace-match (format "%4s" (match-string 1))
				   nil nil nil 1)
		    (replace-match (format "%9s" (match-string 2))
				   nil nil nil 2)
		    (forward-line 1)))
		;; fd all the complete lines in the unprocessed
		;; output and process it to add text properties.
		(goto-char (point-max))
		(if (search-backward "\n" (process-mark proc) t)
		    (progn
		      (dired-insert-set-properties (process-mark proc)
						   (1+ (point)))
		      (move-marker (process-mark proc) (1+ (point)))))))))
      ;; The buffer has been killed.
      (delete-process proc))))

(defun fd-dired-sentinel (proc state)
  ;; Sentinel for \\[fd-dired] processes.
  (let ((buf (process-buffer proc))
	(inhibit-read-only t))
    (if (buffer-name buf)
	(with-current-buffer buf
	  (let ((buffer-read-only nil))
	    (save-excursion
	      (goto-char (point-max))
	      (let ((point (point)))
		(insert "\n  fd " state)
		(forward-char -1)		;Back up before \n at end of STATE.
		(insert " at " (substring (current-time-string) 0 19))
		(dired-insert-set-properties point (point)))
	      (setq mode-line-process
		    (concat ":"
			    (symbol-name (process-status proc))))
	      ;; Since the buffer and mode line will show that the
	      ;; process is dead, we can delete it now.  Otherwise it
	      ;; will stay around until M-x list-processes.
	      (delete-process proc)
	      (force-mode-line-update)))
	  (message "fd-dired %s finished." (current-buffer))))))


(provide 'fd-dired)

;;; fd-dired.el ends here
