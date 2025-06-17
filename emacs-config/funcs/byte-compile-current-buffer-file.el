;;;###autoload
(defun byte-compile-current-buffer-file ()
  (interactive)
  (byte-compile-file (buffer-file-name)))

