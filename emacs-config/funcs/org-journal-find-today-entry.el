;;;###autoload
(defun org-journal-find-today-entry ()
  "Open today's journal entry (other window by default)."
  (interactive)
  (org-journal-new-entry t (current-time) t))

;;;###autoload
(defun org-journal-find-today-entry-same-window ()
  "Open today's journal entry in the current window."
  (interactive)
  ;; Only bind the current variable name.  Since 2.3.0,
  ;; `org-journal-find-file' is an obsolete alias of
  ;; `org-journal-find-file-fn'.  Let-binding the obsolete name while
  ;; org-journal is not yet loaded makes package load fail with:
  ;; "Don't know how to make a let-bound variable an alias".
  (let ((org-journal-find-file-fn #'find-file))
    (org-journal-new-entry t (current-time) t)))

;;;###autoload
(defun org-journal-new-todo-entry ()
  (interactive)
  (org-journal-new-entry nil (current-time) t))
