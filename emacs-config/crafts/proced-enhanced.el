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

(provide 'proced-enhanced)
