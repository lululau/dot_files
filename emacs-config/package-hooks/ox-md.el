(with-eval-after-load 'ox-md
  (defun org-md-example-block (example-block _contents info)
    (let* ((example-string (org-export-format-code-default example-block info)))
      (if (string= "---\n" (substring example-string 0 4))
          example-string
        (replace-regexp-in-string "^" "    "
         (org-remove-indentation example-string))))))
