(defun lx/kill-processes (processes)
  (interactive)
  (mapc 'kill-process processes))

(defun lx/get-all-processes-except (keep-process-names)
  (interactive)
  (seq-filter (lambda (proc) (not (seq-contains-p keep-process-names (process-name proc)))) (process-list)))
(defun lx/kill-except-default-processes ()
  (interactive)
  (lx/kill-processes (lx/get-all-processes-except '("server" "edit-server" "emacsql-sqlite"))))