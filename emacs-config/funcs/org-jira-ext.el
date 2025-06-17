;;; -*- lexical-binding: t -*-

;;;###autoload
(defun org-jira-cleanup-get-issues-from-custom-jql (arg)
  (interactive "P")
  (find-file "~/Documents/materials/jira/recent-issues.org")
  (if arg
      (progn
        (erase-buffer)
        (save-buffer)
        (call-interactively 'org-jira-get-issues-from-custom-jql))
    (evil-exit-hybrid-state)
    (goto-char (point-min))
    (org-shifttab 2)))


;;;###autoload
(defun org-jira-progress-issue-to-resolved ()
  (interactive)
  (ensure-on-issue
    (let* ((issue-id (org-jira-id))
           (action "5")
           (fields nil)
           (org-jira-rest-fields fields)
           (field-key)
           (custom-fields-collector nil)
           (custom-fields nil))
      (jiralib-progress-workflow-action
       issue-id
       action
       custom-fields
       (cl-function
        (lambda (&key data &allow-other-keys)
          (org-jira-refresh-issue)))))))
