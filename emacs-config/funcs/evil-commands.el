(defun vi/del-org-props ()
  (interactive)
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute "g/:PROPERTIES:/.,/:END:/d")))
