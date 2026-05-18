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

(add-hook 'proced-mode-hook #'proced-enhanced-mode)

(defun proced-enhanced--get-pids ()
  "Return a list of PIDs: marked processes first, fall back to PID at point."
  (if-let ((marked (proced-marked-processes)))
      (mapcar #'car marked)
    (list (proced-pid-at-point))))

(defun proced-enhanced-pstree (pids)
  "Show pstree for PID(s).
Uses marked processes, or the process at point."
  (interactive (list (proced-enhanced--get-pids)) proced-mode)
  (let* ((command (format "~/bin/pstree %s" (mapconcat #'number-to-string pids " "))))
    (lx/run-in-ghostel command "*pstree*")))

(defun proced-enhanced-lsof (pid)
  "Show open files for the process at point via lsof."
  (interactive (list (proced-pid-at-point)) proced-mode)
  (lx/run-in-ghostel (format "lsof -Pnp %d" pid) (format "*lsof-%d*" pid)))

(defun proced-enhanced-sigkill (pids)
  "Send SIGKILL to marked processes or the process at point."
  (interactive (list (proced-enhanced--get-pids)) proced-mode)
  (when (y-or-n-p (format "Kill %s? " (mapconcat #'number-to-string pids " ")))
    (dolist (pid pids)
      (signal-process pid 9))
    (proced-update t)))

;; --- Task 6: Incremental Filter ---

(defvar-local proced-enhanced-filter-string nil
  "Current filter string for proced-enhanced.")

(defvar-local proced-enhanced--overlays nil
  "List of filter overlays in current proced buffer.")

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
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-text (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))))
          (unless (string-match-p (regexp-quote filter-str) line-text)
            (let ((ov (make-overlay (line-beginning-position)
                                    (1+ (line-end-position)))))
              (overlay-put ov 'invisible t)
              (push ov proced-enhanced--overlays))))
        (forward-line)))))

(defun proced-enhanced-filter ()
  "Incremental filter proced buffer by process name/args."
  (interactive nil proced-mode)
  (let ((minibuffer-local-map (copy-keymap minibuffer-local-map)))
    ;; C-g cancels and clears filter
    (define-key minibuffer-local-map [remap abort-recursive-edit]
      (lambda ()
        "Cancel filter and show all."
        (interactive)
        (proced-enhanced--apply-filter "")
        (abort-recursive-edit)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'post-command-hook
                    (lambda ()
                      (proced-enhanced--apply-filter
                       (minibuffer-contents)))
                    nil t))
      (read-from-minibuffer "Filter: "))))

(defun proced-enhanced-filter-clear ()
  "Clear the proced-enhanced filter."
  (interactive nil proced-mode)
  (proced-enhanced--apply-filter ""))

;; --- Task 7: Integration Polish ---

(defun proced-enhanced--clear-overlays-on-update (&rest _args)
  "Reset overlay list when proced updates (buffer is erased)."
  (setq proced-enhanced--overlays nil))

(advice-add 'proced-update :before
            #'proced-enhanced--clear-overlays-on-update)

(provide 'proced-enhanced)
