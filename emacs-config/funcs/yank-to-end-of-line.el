;;;###autoload
(defun yank-to-end-of-line ()
  (interactive "")
  (evil-yank-line (point) (line-end-position)))

