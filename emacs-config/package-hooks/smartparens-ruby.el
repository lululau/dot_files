(with-eval-after-load 'smartparens-ruby
  (defun sp-ruby-pre-pipe-handler (id action context)
    (when (equal action 'insert)
      (save-excursion
        (search-backward id)
        (just-one-space)))))
