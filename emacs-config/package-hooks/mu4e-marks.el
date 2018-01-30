(with-eval-after-load 'mu4e-mark
  (setcdr (plist-get (cdr (assoc 'delete mu4e-marks)) :char) "×"))
