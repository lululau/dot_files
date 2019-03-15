(with-eval-after-load 'org
  (define-key org-mode-map [M-tab] 'spacemacs/alternate-buffer)
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

  (defun lx/yank-babel-src ()
    (interactive)
    (kill-new (lx/get-babel-src)))

  (defun lx/org-table-recalculate-multi-formulas ()
    (interactive)
    (save-excursion
      (goto-char (org-table-end))
      (while (string-match "^[[:blank:]]*#\\+TBLFM:" (thing-at-point 'line t))
        (org-table-calc-current-TBLFM)
        (forward-line))))

  (defun org-refresh-inline-images (&optional include-linked)
    (interactive "P")
    (org-remove-inline-images)
    (org-display-inline-images include-linked))

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "DI" #'lx/download-org-images)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "tR" #'lx/org-table-recalculate-multi-formulas)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "oy" #'lx/yank-babel-src)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "os" #'org-babel-execute-subtree)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ob" #'org-babel-execute-buffer)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "sy" #'org-copy-subtree)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "bk" #'org-babel-remove-result-one-or-many)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ttt" #'org-table-transpose-table-at-point)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ic" 'org-cycle-list-bullet)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ik" 'org-move-item-up)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ij" 'org-move-item-down))

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ti" #'org-refresh-inline-images)

(eval-after-load "org"
  '(orgit-link-set-parameters "orgit"
                              :store    'orgit-status-store
                              :follow   'orgit-status-open
                              :export   'orgit-status-export
                              :complete 'orgit-status-complete-link))
(with-eval-after-load 'org-protocol
  (add-to-list 'org-protocol-protocol-alist
               '("capture-html"
                 :protocol "capture-html"
                 :function org-protocol-capture-html--with-pandoc
                 :kill-client t)))

(with-eval-after-load 'evil-org
  (evil-define-key 'motion evil-org-mode-map (kbd "C-i") 'org-cycle))
