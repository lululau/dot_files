(defun my-mu4e-action-view-with-xwidget (msg)
  "View the body of the message inside xwidget-webkit."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (mu4e-error "No xwidget support available"))
  (xwidget-webkit-browse-url (concat "file://" (mu4e~write-body-to-html msg)) t))

