(defun lx/save-scratch ()
  (interactive)
  (let* ((now (format-time-string "%Y%m%d-%H%M%S" (current-time)))
         (file (format "/Users/liuxiang/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/scratches/scratch-%s.txt" now))
         (buffer (find-file file)))
    (with-current-buffer "*scratch*"
      (copy-to-buffer buffer (beginning-of-buffer) (end-of-buffer)))
    (save-buffer)))
