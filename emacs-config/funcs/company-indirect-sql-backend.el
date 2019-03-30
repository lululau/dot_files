(defun company-indirect-sql-backend (command &optional arg &rest ignored)
  "dabbrev-like `company-mode' backend for code.
The backend looks for all symbols in the current buffer that aren't in
comments or strings."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-indirect-sql-backend))
    (prefix (and (eq 'sql-mode major-mode)
                 (bound-and-true-p org-src--beg-marker)
                 (company-grab-symbol)))
    (candidates (let ((case-fold-search company-dabbrev-code-ignore-case))
                  (with-current-buffer (marker-buffer org-src--beg-marker)
                    (company-dabbrev--search
                    (company-dabbrev-code--make-regexp arg)
                    company-dabbrev-code-time-limit
                    (pcase company-dabbrev-code-other-buffers
                      (`t (list major-mode))
                      (`code company-dabbrev-code-modes)
                      (`all `all))
                    (not company-dabbrev-code-everywhere)))))
    (ignore-case company-dabbrev-code-ignore-case)
    (duplicates t)))
