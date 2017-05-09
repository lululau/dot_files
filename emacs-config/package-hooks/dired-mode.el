(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "n") nil)
  (unless (or (display-graphic-p) (lx/system-is-linux))
      (defun dired-delete-file (file &optional recursive trash)
        (call-process "trash" nil nil nil file))))

(with-eval-after-load 'dired-x
  (define-key dired-mode-map (kbd "N") nil))
