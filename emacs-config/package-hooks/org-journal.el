(with-eval-after-load 'org-journal
  (defun org-journal--insert-entry (time org-extend-today-until-active-p &optional todo)
    "Insert a new entry."
    (unless (eq (current-column) 0) (insert "\n"))
    (let* ((day-discrepancy (- (time-to-days (current-time)) (time-to-days time)))
           (timestamp (cond
                       ;; “time” is today, use normal timestamp format
                       ((= day-discrepancy 0)
                        (format-time-string org-journal-time-format))
                       ;; “time” is yesterday with org-extend-today-until,
                       ;; use different timestamp format if available
                       ((and (= day-discrepancy 1) org-extend-today-until-active-p)
                        (if (not (string-equal org-journal-time-format-post-midnight ""))
                            (format-time-string org-journal-time-format-post-midnight)
                          (format-time-string org-journal-time-format)))
                       ;; “time” is on some other day, use blank timestamp
                       (t ""))))
      (insert (concat org-journal-time-prefix (if todo "TODO " "DONE ") timestamp)))
    (run-hooks 'org-journal-after-entry-create-hook))


  (defun org-journal-new-scheduled-entry (prefix &optional scheduled-time insert-at-point)
    "Create a new entry in the future with an active timestamp.
With non-nil prefix argument create a regular entry instead of a TODO entry."
    (interactive "P")
    (let ((time (or scheduled-time (org-time-string-to-time (org-read-date nil nil nil "Date:"))))
          org-journal-carryover-items)
      (org-journal-new-entry nil time (not prefix) insert-at-point)
      (save-excursion
        (insert "\n")
        (insert "SCHEDULED: ")
        (org-insert-time-stamp time t)
        (org-cycle))))


  (defun org-journal-new-entry-after-advice (prefix &optional time todo insert-at-point)
    (unless todo
      (save-excursion
        (insert "\n")
        (insert "SCHEDULED: ")
        (org-insert-time-stamp (current-time) t)
        (org-cycle))))

  (advice-add 'org-journal-new-entry :after #'org-journal-new-entry-after-advice))
