(with-eval-after-load 'mu4e-mark
  (setcdr (plist-get (cdr (assoc 'delete mu4e-marks)) :char) "×"))

(with-eval-after-load 'mu4e-main
  (define-key mu4e-main-mode-map (kbd "s-R") 'lx/force-update-mu4e))
