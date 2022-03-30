(with-eval-after-load 'alert
  (defun alert-notifier-notify (info)
    (if alert-notifier-command
        (let ((args
               (list "-title"   (alert-encode-string (plist-get info :title))
                     "-appIcon" (or (plist-get info :icon) alert-notifier-default-icon)
                     "-sender" "org.gnu.emacs"
                     "-message" (alert-encode-string (plist-get info :message)))))
          (apply #'call-process alert-notifier-command nil nil nil args))
      (alert-message-notify info))))
