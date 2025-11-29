;;;###autoload
(defun org-journal-find-today-entry ()
  (interactive)
  (org-journal-new-entry t (current-time) t))


;;;###autoload
(defun org-journal-new-todo-entry ()
  (interactive)
  (org-journal-new-entry nil (current-time) t))
