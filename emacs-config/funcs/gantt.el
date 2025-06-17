;;;###autoload
(defun gantt ()
  (interactive)
  (require 'ox-taskjuggler)
  (rvm-use-default)
  (org-taskjuggler-export-process-and-open))

