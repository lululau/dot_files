;;;###autoload
(defun lx/open-org-calendar ()
  "Open org calendar via calfw"
  (interactive)
  (require 'calfw-org)
  (cfw:open-org-calendar))

;;;###autoload
(defun lx/org-refile ()
  "Org refile with prefix"
  (interactive)
  (org-refile '(4)))
