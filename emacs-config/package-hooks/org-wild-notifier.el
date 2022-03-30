(with-eval-after-load 'org-wild-notifier
  (defun org-wild-notifier--get-hh-mm-from-org-time-string (time-string)
    "Convert given org time-string TIME-STRING into string with 'hh:mm' format."
    (if (>= (length time-string) 20)
        (substring time-string 14 19)
      "00:00")))
