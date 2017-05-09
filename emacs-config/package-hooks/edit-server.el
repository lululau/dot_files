(unless (display-graphic-p)
  (with-eval-after-load 'edit-server
    (defun edit-server-start (&optional verbose)
      "Start the edit server.

If argument VERBOSE is non-nil, logs all server activity to buffer
`*edit-server-log*'.  When called interactivity, a prefix argument
will cause it to be verbose."
      (interactive "P"))))
