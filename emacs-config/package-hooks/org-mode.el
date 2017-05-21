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
       body params (funcall (intern (format "org-babel-variable-assignments:%s" lang)) params))))

  (defun lx/org-table-recalculate-multi-formulas ()
    (interactive)
    (save-excursion
      (goto-char (org-table-end))
      (while (string-match "^[[:blank:]]*#\\+TBLFM:" (thing-at-point 'line t))
        (org-table-calc-current-TBLFM)
        (forward-line))))

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "tR" #'lx/org-table-recalculate-multi-formulas)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "os" #'org-babel-execute-subtree)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ob" #'org-babel-execute-buffer))
