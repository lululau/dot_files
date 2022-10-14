(with-eval-after-load 'mu4e-mark
  (setcdr (plist-get (cdr (assoc 'delete mu4e-marks)) :char) "×"))

(with-eval-after-load 'mu4e-main
  (define-key mu4e-main-mode-map (kbd "s-R") 'lx/force-update-mu4e))

(defvar mu4e-view-highlights nil)

(with-eval-after-load 'mu4e-view
  (defun mu4e-view-mode-auto-highlight (&rest _)
    (mapc (lambda (highlight)
            (let ((regexp (car highlight))
                  (face (cdr highlight)))
              (highlight-lines-matching-regexp regexp face)))
          mu4e-view-highlights))
  (advice-add 'mu4e~view-render-buffer :after #'mu4e-view-mode-auto-highlight))
