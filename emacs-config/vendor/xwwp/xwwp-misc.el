(require 'xwwp)

(defun xwwp-goto-root (&optional xwidget)
  "Toggle reader mode in current XWIDGET session."
  (interactive)
  (let* ((xwidget (or xwidget (xwidget-webkit-current-session)))
    (url (xwidget-webkit-uri xwidget))
    (root (replace-regexp-in-string "\\([a-z]+://[^/]*\\)/.*$" "\\1" url)))
    (xwidget-webkit-execute-script
     xwidget
     (format "document.location.href = \"%s\";" root))))

(provide 'xwwp-misc)
