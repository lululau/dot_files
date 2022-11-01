;;; -*- lexical-binding: t -*-

(with-eval-after-load 'org-jira

  (defun jiralib-call (method callback &rest params)
    "Invoke the Jira METHOD, then CALLBACK with supplied PARAMS.

  This function should be used for all JIRA interface calls, as the
  method ensures the user is logged in and invokes `soap-invoke'
  with the correct service name and authentication token.

  All JIRA interface methods take an authentication token as the
  first argument.  The authentication token is supplied by this
  function, so PARAMS should omit this parameter.  For example, the
  \"getIssue\" method takes two parameters: auth and key, however,
  when invoking it through `jiralib-call', the call should be:

    (jiralib-call \"getIssue\" KEY)

  CALLBACK should be the post processing function to run with the
  completed data from the request result, which can be accessed with:

    (cl-getf data :data)

  as such, the CALLBACK should follow this type of form:

    (cl-function
      (lambda (&rest data &allow-other-keys)
        (print (cl-getf data :data))))

  If CALLBACK is set to nil then the request will occur with sync.
  This produces a noticeable slowdown and is not recommended by
  request.el, so if at all possible, it should be avoided."
    ;; @TODO :auth: Probably pass this all the way down, but I think
    ;; it may be OK at the moment to just set the variable each time.

    (setq jiralib-complete-callback
          ;; Don't run with async if we don't have a login token yet.
          (if jiralib-token callback nil))

    ;; If we don't have a regex set, ensure it is set BEFORE any async
    ;; calls are processing, or we're going to have a bad time.
    ;; This should only end up running once per session.
    (unless jiralib-issue-regexp
      (let ((projects (mapcar (lambda (e) (downcase (cdr (assoc 'key e))))
                              (append (jiralib--rest-call-it
                                      "/rest/api/2/project"
                                      :params '((expand . "description,lead,url,projectKeys")))
                                      nil)
                              )))
        (when projects
          (setq jiralib-issue-regexp
                (concat "\\<" (regexp-opt projects) "-[0-9]+\\>")))))

    (if (not jiralib-use-restapi)
        (car (apply 'jiralib--call-it method params))
      (unless jiralib-token
        (call-interactively 'jiralib-login))
      (cl-case (intern method)
        ('getStatuses (jiralib--rest-call-it "/rest/api/2/status"))
        ('getIssueTypes (jiralib--rest-call-it "/rest/api/2/issuetype"))
        ('getSubTaskIssueTypes (jiralib--rest-call-it "/rest/api/2/issuetype"))
        ('getIssueTypesByProject
        (let ((response (jiralib--rest-call-it (format "/rest/api/2/project/%s" (first params)))))
          (cl-coerce (cdr (assoc 'issueTypes response)) 'list)))
        ('getUser (jiralib--rest-call-it "/rest/api/2/user" :params `((username . ,(first params)))))
        ('getVersions (jiralib--rest-call-it (format "/rest/api/2/project/%s/versions" (first params))))

        ;; Worklog calls
        ('getWorklogs
        (jiralib--rest-call-it (format "/rest/api/2/issue/%s/worklog" (first params))))

        ('addWorklog
        (jiralib--rest-call-it (format "/rest/api/2/issue/%s/worklog" (first params))
                                :type "POST"
                                :data (json-encode (second params))))

        ('updateWorklog
        (jiralib--rest-call-it (format "/rest/api/2/issue/%s/worklog/%s" (first params) (second params))
                                :type "PUT"
                                :data (json-encode (third params))))

        ('addWorklogAndAutoAdjustRemainingEstimate
        (jiralib--rest-call-it (format "/rest/api/2/issue/%s/worklog" (first params))
                                :type "POST"
                                :data (json-encode (second params))))

        ('addComment (jiralib--rest-call-it
                      (format "/rest/api/2/issue/%s/comment" (first params))
                      :type "POST"
                      :data (json-encode (second params))))
        ('createIssue
        ;; Creating the issue doesn't return it, a second call must be
        ;; made to pull it in by using the self key in response.
        (let ((response (jiralib--rest-call-it
                          "/rest/api/2/issue"
                          :type "POST"
                          :data (json-encode (first params)))))
          (jiralib--rest-call-it (cdr (assoc 'self response)) :type "GET")
          ))
        ('createIssueWithParent
        (let ((response (jiralib--rest-call-it
                          "/rest/api/2/issue"
                          :type "POST"
                          :data (json-encode (first params)))))
          (jiralib--rest-call-it (cdr (assoc 'self response)) :type "GET")
          ))
        ('editComment (jiralib--rest-call-it
                      (format "/rest/api/2/issue/%s/comment/%s" (first params) (second params))
                      :data (json-encode `((body . ,(third params))))
                      :type "PUT"))
        ('getBoard  (jiralib--rest-call-it (format "/rest/agile/1.0/board/%s"  (first params))))
        ('getBoards (apply 'jiralib--agile-call-it "/rest/agile/1.0/board" 'values params))
        ('getComment (org-jira-find-value
                      (jiralib--rest-call-it
                        (format "/rest/api/2/issue/%s/comment/%s" (first params) (second params)))
                      'comments))
        ('getComments (org-jira-find-value
                      (jiralib--rest-call-it
                        (format "/rest/api/2/issue/%s/comment" (first params)))
                      'comments))
        ('getAttachmentsFromIssue (org-jira-find-value
                                  (jiralib--rest-call-it
                                    (format "/rest/api/2/issue/%s?fields=attachment" (first params)))
                                  'comments))
        ('getComponents (jiralib--rest-call-it
                        (format "/rest/api/2/project/%s/components" (first params))))
        ('getIssue (jiralib--rest-call-it
                    (format "/rest/api/2/issue/%s" (first params))))
        ('getIssuesFromBoard  (apply 'jiralib--agile-call-it
            (format "rest/agile/1.0/board/%d/issue" (first params))
            'issues
            (cdr params)))
        ('getSprintsFromBoard  (jiralib--rest-call-it (format "/rest/agile/1.0/board/%s/sprint"  (first params))))
        ('getIssuesFromSprint  (apply 'jiralib--agile-call-it
            (format "rest/agile/1.0/sprint/%d/issue" (first params))
            'issues
            (cdr params)))
        ('getIssuesFromJqlSearch  (append (cdr ( assoc 'issues (jiralib--rest-call-it
                                                                "/rest/api/2/search"
                                                                :type "POST"
                                                                :data (json-encode `((jql . ,(first params))
                                                                                    (maxResults . ,(second params)))))))
                                          nil))
        ('getPriorities (jiralib--rest-call-it
                        "/rest/api/2/priority"))
        ('getProjects (jiralib--rest-call-it "rest/api/2/project"))
        ('getProjectsNoSchemes (append (jiralib--rest-call-it
                                        "/rest/api/2/project"
                                        :params '((expand . "description,lead,url,projectKeys"))) nil))
        ('getResolutions (append (jiralib--rest-call-it
                                  "/rest/api/2/resolution") nil))
        ('getAvailableActions
        (mapcar
          (lambda (trans)
            `(,(assoc 'name trans) ,(assoc 'id trans)))
          (cdadr (jiralib--rest-call-it (format "/rest/api/2/issue/%s/transitions" (first params))))))
        ('getFieldsForAction (org-jira-find-value (car (let ((issue (first params))
                                                            (action (second params)))
                                                        (seq-filter (lambda (trans)
                                                                      (or (string-equal action (org-jira-find-value trans 'id))
                                                                          (string-equal action (org-jira-find-value trans 'name))))
                                                                    (cdadr (jiralib--rest-call-it
                                                                            (format "/rest/api/2/issue/%s/transitions" (first params))
                                                                            :params '((expand . "transitions.fields")))))))
                                                  'fields))
        ('progressWorkflowAction (jiralib--rest-call-it
                                  (format "/rest/api/2/issue/%s/transitions" (first params))
                                  :type "POST"
                                  :data (json-encode `(,(car (second params)) ,(car (third params))))))
        ('getUsers
        (jiralib--rest-call-it (format "/rest/api/2/user/assignable/search?project=%s&maxResults=10000" (first params))
                                :type "GET"))
        ('updateIssue (jiralib--rest-call-it
                      (format "/rest/api/2/issue/%s" (first params))
                      :type "PUT"
                      :data (json-encode `((fields . ,(second params)))))))))

  (defun jiralib-get-user-account-id (project full-name)
    "Return the account-id (accountId) of the user with FULL-NAME (displayName) in PROJECT."
    (cl-loop for user in (jiralib-get-users project)
             when (rassoc full-name user)
             return (cdr (assoc 'name user))))

  (defun org-jira-get-assignable-users (project-key)
    "Get the list of assignable users for PROJECT-KEY, adding user set jira-users first."
    (append
     '(("Unassigned" . nil))
     org-jira-users
     (mapcar (lambda (user)
               (cons (org-jira-decode (cdr (assoc 'name user)))
                     (org-jira-decode (cdr (assoc 'name user)))))
             (jiralib-get-users project-key))))


  (defun org-jira-update-issue-details (issue-id filename &rest rest)
    "Update the details of issue ISSUE-ID in FILENAME.  REST will contain optional input."
    (ensure-on-issue-id-with-filename issue-id filename
      ;; Set up a bunch of values from the org content
      (let* ((org-issue-components (org-jira-get-issue-val-from-org 'components))
            (org-issue-labels (org-jira-get-issue-val-from-org 'labels))
            (org-issue-description (org-trim (org-jira-get-issue-val-from-org 'description)))
            (org-issue-priority (org-jira-get-issue-val-from-org 'priority))
            (org-issue-type (org-jira-get-issue-val-from-org 'type))
            (org-issue-type-id (org-jira-get-issue-val-from-org 'type-id))
            (org-issue-assignee (cl-getf rest :assignee (org-jira-get-issue-val-from-org 'assignee)))
            (project (replace-regexp-in-string "-[0-9]+" "" issue-id))
            (project-components (jiralib-get-components project)))

        ;; Lets fire off a worklog update async with the main issue
        ;; update, why not?  This is better to fire first, because it
        ;; doesn't auto-refresh any areas, while the end of the main
        ;; update does a callback that reloads the worklog entries (so,
        ;; we hope that wont occur until after this successfully syncs
        ;; up).  Only do this sync if the user defcustom defines it as such.
        (when org-jira-worklog-sync-p
          (org-jira-update-worklogs-from-org-clocks))

        ;; Send the update to jira
        (let ((update-fields
              (list (cons
                      'components
                      (or (org-jira-build-components-list
                          project-components
                          org-issue-components) []))
                    (cons 'labels (split-string org-issue-labels ",\\s *"))
                    (cons 'priority (org-jira-get-id-name-alist org-issue-priority
                                                        (jiralib-get-priorities)))
                    (cons 'description org-issue-description)
                    (cons 'assignee (list (cons 'name (jiralib-get-user-account-id project org-issue-assignee))))
                    (cons 'summary (org-jira-strip-priority-tags (org-jira-get-issue-val-from-org 'summary)))
                    (cons 'issuetype `((id . ,org-issue-type-id)
        (name . ,org-issue-type))))))


          ;; If we enable duedate sync and we have a deadline present
          (when (and org-jira-deadline-duedate-sync-p
                    (org-jira-get-issue-val-from-org 'deadline))
            (setq update-fields
                  (append update-fields
                          (list (cons 'duedate (org-jira-get-issue-val-from-org 'deadline))))))

          ;; TODO: We need some way to handle things like assignee setting
          ;; and refreshing the proper issue in the proper buffer/filename.
          (jiralib-update-issue
          issue-id
          update-fields
          ;; This callback occurs on success
          (org-jira-with-callback
            (message (format "Issue '%s' updated!" issue-id))
            (jiralib-get-issue
              issue-id
              (org-jira-with-callback
                (org-jira-log "Update get issue for refresh callback hit.")
                (org-jira-refresh-issue)
                (save-buffer)
                (evil-exit-hybrid-state))))
          ))
        ))
    )

  (defun org-jira--render-issues-from-issue-list (Issues)
    "Add the issues from ISSUES list into the org file(s).

ISSUES is a list of `org-jira-sdk-issue' records."
    ;; FIXME: Some type of loading error - the first async callback does not know about
    ;; the issues existing as a class, so we may need to instantiate here if we have none.
    (when (eq 0 (->> Issues (cl-remove-if-not #'org-jira-sdk-isa-issue?) length))
      (setq Issues (org-jira-sdk-create-issues-from-data-list Issues)))

    ;; First off, we never ever want to run on non-issues, so check our types early.
    (setq Issues (cl-remove-if-not #'org-jira-sdk-isa-issue? Issues))
    (org-jira-log (format "About to render %d issues." (length Issues)))

    ;; If we have any left, we map over them.
    (let ((org-jira-download-comments nil)
          (org-jira-worklog-sync-p nil))
      (mapc 'org-jira--render-issue Issues))

    ;; Prior text: "Oh, are you the culprit?" - Not sure if this caused an issue at some point.
    ;; We want to ensure we fix broken org narrowing though, by doing org-show-all and then org-cycle.
    (switch-to-buffer (org-jira--get-project-buffer (-last-item Issues)))
    (save-buffer)
    (goto-char (point-min))
    (org-shifttab 2)
    (evil-exit-hybrid-state)))

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
