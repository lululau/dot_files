;;;###autoload
(defun turn-off-sp-on-large-file ()
  (interactive)
  (when (< 1200 (line-number-at-pos (buffer-size))) (turn-off-smartparens-mode) (turn-off-show-smartparens-mode))
  )

