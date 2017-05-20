(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map (kbd "RET") #'(lambda () (interactive) (condition-case nil (call-interactively 'org-open-at-point) (error (evil-insert-newline-below)))))
  (defun lx/get-babel-src ()
    (let* ((info (org-babel-get-src-block-info nil (org-element-context)))
           (lang (nth 0 info))
           (params (nth 2 info))
           (body
            (let ((coderef (nth 6 info))
                  (expand
                   (if (org-babel-noweb-p params :eval)
                       (org-babel-expand-noweb-references info)
                     (nth 1 info))))
              (if (not coderef) expand
                (replace-regexp-in-string
                 (org-src-coderef-regexp coderef) "" expand nil nil 1)))))
      (org-babel-expand-body:generic
       body params (funcall (intern (format "org-babel-variable-assignments:%s" lang)) params)))))
