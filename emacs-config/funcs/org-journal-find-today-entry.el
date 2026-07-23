;;;###autoload
(defun org-journal-find-today-entry ()
  "Open today's journal entry (other window by default)."
  (interactive)
  (org-journal-new-entry t (current-time) t))

;;;###autoload
(defun org-journal-find-today-entry-same-window ()
  "Open today's journal entry in the current window."
  (interactive)
  (let ((org-journal-find-file-fn 'find-file)
        (org-journal-find-file 'find-file))
    (org-journal-new-entry t (current-time) t)))

;;;###autoload
(defun org-journal-new-todo-entry ()
  (interactive)
  (org-journal-new-entry nil (current-time) t))
