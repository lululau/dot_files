(defun get-git-link (remote start end)
  (interactive (let* ((remote (if current-prefix-arg
                                  (git-link--read-remote)
                                (git-link--remote)))
                      (region (git-link--get-region)))
                 (list remote (car region) (cadr region))))
  (let* ((remote-host (git-link--remote-host remote))
	 (filename    (git-link--relative-filename))
	 (branch      (git-link--branch))
	 (commit      (git-link--commit))
	 (handler     (cadr (assoc remote-host git-link-remote-alist))))

    (cond ((null filename)
	   (message "Not in a git repository with a working tree"))
	  ((null remote-host)
	   (message "Remote '%s' is unknown or contains an unsupported URL" remote))
	  ((not (functionp handler))
	   (message "No handler for %s" remote-host))
	  ((funcall handler
		     remote-host
		     (git-link--remote-dir remote)
		     filename
		     (if (or (git-link--using-git-timemachine) git-link-use-commit)
			 nil
		       branch)
		     commit
		     start
		     end)))))

(defun get-local-file-link ()
  (let (cpltxt txt link)
    (buffer-file-name (buffer-base-buffer))
    (setq cpltxt (concat "file:"
                         (abbreviate-file-name
                          (buffer-file-name (buffer-base-buffer)))))
    (setq txt (if (org-region-active-p)
                  (buffer-substring (region-beginning) (region-end))
                (buffer-substring (point-at-bol) (point-at-eol))))
    (when (string-match "\\S-" txt)
      (setq cpltxt
            (concat cpltxt "::" (org-link-escape (org-make-org-heading-search-string txt)))
            desc "NONE"))
    cpltxt))

(defun copy-org-links-at-point ()
  (interactive)
  (require 'git-link)
  (kill-new (format "[[%s][Local Source]]  [[%s][Github Source]]" (get-local-file-link) (call-interactively 'get-git-link))))
