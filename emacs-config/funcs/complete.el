(defun lx/company-backend-usable-p (backend)
  "Return non-nil if BACKEND is a keyword or a defined company function."
  (or (keywordp backend)
      (not (symbolp backend))
      (fboundp backend)))

;;;###autoload
(defun lx/reset-lsp-company-backends ()
  (interactive)
  (let ((preferred (append '(company-files company-capf)
                           (when (fboundp 'company-tabnine)
                             '(company-tabnine))
                           '(:with company-yasnippet)))
        backends)
    (dolist (backend company-backends backends)
      (if (consp backend)
          (let ((kept (seq-filter
                       (lambda (it)
                         (and (lx/company-backend-usable-p it)
                              (not (seq-contains-p preferred it))))
                       backend)))
            (when kept
              (push (append kept '(:with company-yasnippet)) backends)))
        (when (and (lx/company-backend-usable-p backend)
                   (not (seq-contains-p preferred backend)))
          (push (list backend :with 'company-yasnippet) backends))))
    (setq-local company-backends (cons preferred backends))))
