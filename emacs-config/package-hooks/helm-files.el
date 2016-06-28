(spacemacs|use-package-add-hook helm-files
  :post-config
  (defun helm-substitute-in-filename (fname)
    (cond ((and ffap-url-regexp
                (string-match-p ffap-url-regexp fname))
           fname)
          ((and (file-remote-p fname)
                helm-substitute-in-filename-stay-on-remote)
           (let ((sub (substitute-in-file-name fname)))
             (if (file-directory-p sub)
                 sub (replace-regexp-in-string "/\\'" "" sub))))
          (t
           (with-temp-buffer
             (insert fname)
             (goto-char (point-min))
             (skip-chars-forward "/") ;; Avoid infloop in UNC paths Issue #424
             (if (re-search-forward "~/\\|//\\|/[[:alpha:]]:/" nil t)
                 (let ((match (match-string 0)))
                   (goto-char (if (or (string= match "//")
                                      (string-match-p "/[[:alpha:]]:/" match))
                                  (1+ (match-beginning 0))
                                (match-beginning 0)))
                   (buffer-substring-no-properties (point) (point-at-eol)))
               fname))))))
