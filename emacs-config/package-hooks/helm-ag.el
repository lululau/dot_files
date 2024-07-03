(with-eval-after-load 'helm-ag
  (defun helm-ag--show-result-p (options has-query)
    "Not documented, OPTIONS, HAS-QUERY."
    (or has-query
        (cl-loop for opt in options
                 thereis (string-prefix-p "--files" opt)))))
