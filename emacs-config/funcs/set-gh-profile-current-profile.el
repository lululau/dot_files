;;;###autoload
(defun lx/set-gh-profile-current-profile ()
  (-each gh-profile-alist #'(lambda (e)
                              (let ((regexp (plist-get (cdr e) :remote-regexp))
                                    (profile (car e))
                                    (remote-url (magit-get "remote" (magit-get-remote) "url")))
                                (if (and regexp (string-match-p regexp remote-url))
                                    (setq-local gh-profile-current-profile profile))))))

