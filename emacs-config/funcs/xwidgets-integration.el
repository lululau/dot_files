(defun xwidget-webkit-browse-mu4e-msg (url &rest args)
  (require 'xwidget)
  (if (buffer-live-p xwidget-webkit-last-session-buffer)
      (switch-to-buffer xwidget-webkit-last-session-buffer))
  (xwidget-webkit-browse-url url)
  (switch-to-buffer xwidget-webkit-last-session-buffer))

(defun my-mu4e-action-view-with-xwidget (msg)
  "View the body of the message inside xwidget-webkit."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (mu4e-error "No xwidget support available"))
  (let ((browse-url-browser-function 'xwidget-webkit-browse-mu4e-msg))
    (mu4e-action-view-in-browser msg)))
