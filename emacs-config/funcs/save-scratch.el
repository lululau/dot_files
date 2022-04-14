(defun lx/save-scratch ()
  (interactive)
  (let* ((now (format-time-string "%Y%m%d-%H%M%S" (current-time)))
         (file (format "~/Documents/materials/scratches/scratch-%s.txt" now))
         (current-buffer (current-buffer))
         (buffer (find-file file)))
    (with-current-buffer current-buffer
      (copy-to-buffer buffer (beginning-of-buffer) (end-of-buffer)))
    (save-buffer)))
