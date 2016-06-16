;; mu4e config
;; Set up some common mu4e variables
(setq mu4e-maildir "~/Maildir"
      mu4e-trash-folder "/Deleted Messages"
      mu4e-refile-folder "/Archive"
      mu4e-sent-folder "/Sent Messages"
      mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 300
      mu4e-compose-signature-auto-include nil
      mu4e-html2text-command 'mu4e-shr2text
      mu4e-view-show-images t
      mu4e-view-show-addresses t)

;; Mail directory shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i) ("/Sent Messages" . ?s) ("/Deleted Messages" . ?d)
        ("/Archive" . ?a)))

;; Bookmarks
(setq mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        ("date:today..now" "Today's messages" ?t)
        ("date:7d..now" "Last 7 days" ?w)
        ("mime:image/*" "Messages with images" ?p)
        (,(mapconcat 'identity
                     (mapcar
                      (lambda (maildir)
                        (concat "maildir:" (car maildir)))
                      mu4e-maildir-shortcuts) " OR ")
         "All inboxes" ?i)))

;; SMTP
(setq user-full-name "刘向")
(setq user-mail-address "liuxiang@ktjr.com")
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-server "smtp.exmail.qq.com")
(setq smtpmail-smtp-service 465)

(with-eval-after-load 'mu4e-alert
  ;; Enable Desktop notifications
  (mu4e-alert-set-default-style 'notifier))

(with-eval-after-load 'mu4e
  (require 'mu4e-contrib))

(with-eval-after-load 'helm-mu

  (defun helm-mu ()
    "Search for emails.  If started in mu4e-headers-view, the
current query will be used to initialize the search.  Otherwise
`helm-mu-default-search-string' will be used."
    (interactive)
    (let* ((query (if (eq major-mode 'mu4e-headers-mode)
                      (mu4e-last-query)
                    helm-mu-default-search-string))
           ;; Do not append space it there is already trailing space or query is
           ;; empty
           (input (if (not (or (string-match-p " $" query)
                               (string= "" query)))
                      (concat query " ")
                    query)))

      ;; If there is an existing helm action buffer kill it, otherwise it interferes
      ;; with the action for this source. This will happen if helm-mu is called as
      ;; an action from some other source
      (when (get-buffer helm-action-buffer)
        (kill-buffer helm-action-buffer))

      (helm :sources 'helm-source-mu
            :buffer "*helm mu*"
            :keymap helm-mu-map
            :input input
            :candidate-number-limit 500))))

(defun mu4e-toggle-org-mode ()
  (interactive)
  (cond
   ((eq major-mode 'mu4e-view-mode) (mu4e-org-mode))
   ((eq major-mode 'mu4e-org-mode) (mu4e-view-mode))
   ((eq major-mode 'mu4e-compose-mode) (org-mu4e-compose-org-mode))
   ((eq major-mode 'org-mu4e-compose-org-mode) (mu4e-compose-mode))))

(defun open-message-with-mail-app ()
  (interactive)
  (let* ((msg-path (plist-get (mu4e-message-at-point) :path))
         (temp (make-temp-file "mu4e-message-" nil ".eml")))
    (shell-command-to-string (format "cp '%s' '%s'; open '%s' -a Mail; /bin/rm '%s'" msg-path temp temp temp))))

(with-eval-after-load 'mu4e-headers
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-headers-mode
    "fo" 'open-message-with-mail-app))

(with-eval-after-load 'mu4e-view
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-view-mode
    "to" 'mu4e-toggle-org-mode
    "fo" 'open-message-with-mail-app))

(with-eval-after-load 'mu4e-utils
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-org-mode
    "to" 'mu4e-toggle-org-mode
    "fo" 'open-message-with-mail-app))

(with-eval-after-load 'mu4e-compose
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode "a" 'mml-attach-file)
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode "to" 'mu4e-toggle-org-mode))

(with-eval-after-load 'org-mu4e
  (setq org-mu4e-convert-to-html t)
  (spacemacs/set-leader-keys-for-major-mode 'org-mu4e-compose-org-mode "a" 'mml-attach-file)
  (spacemacs/set-leader-keys-for-major-mode 'org-mu4e-compose-org-mode "to" 'mu4e-toggle-org-mode)
  (defun org~mu4e-mime-convert-to-html ()
    "Convert the current body to html."
    (unless (fboundp 'org-export-string-as)
      (mu4e-error "require function 'org-export-string-as not found."))
    (let* ((begin
            (save-excursion
              (goto-char (point-min))
              (search-forward mail-header-separator)))
           (end (point-max))
           (raw-body (buffer-substring begin end))
           (tmp-file (make-temp-name (expand-file-name "mail"
                                                       temporary-file-directory)))
           (org-export-skip-text-before-1st-heading nil)
           (org-export-htmlize-output-type 'inline-css)
           (org-export-preserve-breaks t)
           (org-export-with-LaTeX-fragments
            (if (executable-find "dvipng") 'dvipng
              (mu4e-message "Cannot find dvipng, ignore inline LaTeX") nil))
           (html-and-images
            (org~mu4e-mime-replace-images
             (org-export-string-as raw-body 'html nil)
             tmp-file))
           (html-images (cdr html-and-images))
           (html (car html-and-images)))
      (delete-region begin end)
      (save-excursion
        (goto-char begin)
        (newline)
        (insert (org~mu4e-mime-multipart
                 raw-body html (mapconcat 'identity html-images "\n")))))))
