(defun delete-window-or-bury-buffer ()
  (interactive)
  (let* ((window-list (window-list))
         (window-count (length window-list)))
    (if (eq 1 window-count)
        (if (eq (selected-window) (car window-list))
            (bury-buffer))
      (delete-window))))