(defun org-journal-find-today-entry ()
  (interactive)
  (require 'org-journal)
  (org-journal--sanity-checks)
  (org-journal--create-journal-dir)
  (funcall org-journal-find-file (org-journal--get-entry-path nil)))


(defun org-journal-new-todo-entry ()
  (interactive)
  (org-journal-new-entry nil (current-time) t))
