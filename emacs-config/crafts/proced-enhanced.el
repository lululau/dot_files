;;; proced-enhanced.el --- Enhanced proced with filter, tree, lsof, sigkill  -*- lexical-binding: t; -*-

(require 'proced)
(require 'run-in-ghostel)

(defvar-keymap proced-enhanced-mode-map
  :parent proced-mode-map
  "f"   #'proced-enhanced-filter
  "C-f" #'proced-enhanced-filter-clear
  "t"   #'proced-enhanced-pstree
  "l"   #'proced-enhanced-lsof
  "K"   #'proced-enhanced-sigkill)

(define-minor-mode proced-enhanced-mode
  "Enhanced proced with filter, tree, lsof, and sigkill commands."
  :lighter " Proced+"
  :keymap proced-enhanced-mode-map)

(defun proced-enhanced--get-pids ()
  "Return a list of PIDs: marked processes first, fall back to PID at point."
  (if-let ((marked (proced-marked-processes)))
      (mapcar #'car marked)
    (when-let ((pid (proced-pid-at-point)))
      (list pid))))

(defun proced-enhanced-pstree (pids)
  "Show pstree for PID(s).
Uses marked processes, or the process at point."
  (interactive (list (proced-enhanced--get-pids)) proced-mode)
  (if pids
      (let ((ghostel-kill-buffer-on-exit nil))
        (lx/run-in-ghostel (format "~/bin/pstree %s" (mapconcat #'number-to-string pids " "))
                           "*pstree*" nil t))
    (message "No process at point")))

(defun proced-enhanced-lsof (pid)
  "Show open files for the process at point via lsof."
  (interactive (list (proced-pid-at-point)) proced-mode)
  (if pid
      (let ((ghostel-kill-buffer-on-exit nil))
        (lx/run-in-ghostel (format "lsof -Pnp %d" pid) (format "*lsof-%d*" pid) nil t))
    (message "No process at point")))

(defun proced-enhanced-sigkill (pids)
  "Send SIGKILL to marked processes or the process at point."
  (interactive (list (proced-enhanced--get-pids)) proced-mode)
  (if pids
      (when (y-or-n-p (format "Kill %s? " (mapconcat #'number-to-string pids " ")))
        (dolist (pid pids)
          (signal-process pid 9))
        (proced-update t))
    (message "No process at point")))

(defvar-local proced-enhanced-filter-string nil
  "Current filter string for proced-enhanced.")

(defvar-local proced-enhanced--overlays nil
  "List of filter overlays in current proced buffer.")

(defvar-local proced-enhanced--regex-mode nil
  "Whether filter is in regex mode.")

(defun proced-enhanced--apply-filter (filter-str)
  "Apply FILTER-STR to proced buffer using overlays."
  (setq proced-enhanced-filter-string
        (if (string= filter-str "") nil filter-str))
  ;; Remove old overlays
  (dolist (ov proced-enhanced--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq proced-enhanced--overlays nil)
  ;; If empty filter, show all
  (when (and filter-str (not (string= filter-str "")))
    (condition-case nil
        (let ((case-fold-search t)
              (pattern (if proced-enhanced--regex-mode
                           filter-str
                         (regexp-quote filter-str))))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line-text (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position))))
                (unless (string-match-p pattern line-text)
                  (let ((ov (make-overlay (line-beginning-position)
                                          (1+ (line-end-position)))))
                    (overlay-put ov 'invisible t)
                    (push ov proced-enhanced--overlays))))
              (forward-line))))
      (invalid-regexp
       (dolist (ov proced-enhanced--overlays)
         (when (overlay-buffer ov)
           (delete-overlay ov)))
       (setq proced-enhanced--overlays nil)))))

(defun proced-enhanced-filter ()
  "Incremental filter proced buffer by process name/args.
Supports M-r to toggle between plain text and regexp matching."
  (interactive nil proced-mode)
  (let ((proced-buffer (current-buffer))
        (regex-mode nil)
        (prompt-end nil)
        (minibuffer-local-map (copy-keymap minibuffer-local-map)))
    ;; M-r toggles regex mode
    (define-key minibuffer-local-map "\M-r"
      (lambda ()
        "Toggle plain/regex filter mode."
        (interactive)
        (setq regex-mode (not regex-mode))
        (message "%s mode" (if regex-mode "Regexp" "Plain text"))
        (let ((text (buffer-substring-no-properties prompt-end (point-max))))
          (with-current-buffer proced-buffer
            (setq proced-enhanced--regex-mode regex-mode)
            (proced-enhanced--apply-filter text)))))
    ;; C-g cancels and clears filter
    (define-key minibuffer-local-map [remap abort-recursive-edit]
      (lambda ()
        "Cancel filter and show all."
        (interactive)
        (with-current-buffer proced-buffer
          (setq proced-enhanced--regex-mode nil)
          (proced-enhanced--apply-filter ""))
        (abort-recursive-edit)))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq prompt-end (minibuffer-prompt-end))
          (add-hook 'post-command-hook
                    (lambda ()
                      (let ((text (buffer-substring-no-properties prompt-end (point-max))))
                        (with-current-buffer proced-buffer
                          (setq proced-enhanced--regex-mode regex-mode)
                          (proced-enhanced--apply-filter text))))
                    nil t))
      (read-from-minibuffer "Filter (M-r: regexp): "))))

(defun proced-enhanced-filter-clear ()
  "Clear the proced-enhanced filter."
  (interactive nil proced-mode)
  (proced-enhanced--apply-filter ""))

(defun proced-enhanced--clear-overlays-on-update (&rest _args)
  "Reset overlay list when proced updates (buffer is erased)."
  (setq proced-enhanced--overlays nil))

(advice-add 'proced-update :before
            #'proced-enhanced--clear-overlays-on-update)

(defun proced-enhanced--do-mark-all (fn mark)
  "Advice on `proced-do-mark-all' to skip invisible lines when filter is active."
  (if (not proced-enhanced-filter-string)
      (funcall fn mark)
    (let ((buffer-read-only nil)
          (mark (cond ((eq mark t) ?*)
                      ((null mark) ?\s)
                      (t mark))))
      (goto-char (point-min))
      (while (not (eobp))
        (unless (get-char-property (line-beginning-position) 'invisible)
          (insert (char-to-string mark))
          (delete-char 1))
        (forward-line)))))

(advice-add 'proced-do-mark-all :around
            #'proced-enhanced--do-mark-all)

(evil-define-key 'normal proced-enhanced-mode-map
  "f" #'proced-enhanced-filter
  "t" #'proced-enhanced-pstree
  "l" #'proced-enhanced-lsof
  "K" #'proced-enhanced-sigkill)

(provide 'proced-enhanced)
