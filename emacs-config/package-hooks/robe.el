(with-eval-after-load 'robe
  (defun robe-find-file (file &optional pop-to-buffer)
    (let ((file (and file (s-chomp file))))
      (unless (file-exists-p file)
        (error "'%s' does not exist" file))
      (if pop-to-buffer
          (pop-to-buffer (find-file-noselect file))
        (xref-push-marker-stack)
        (find-file file))
      (run-hooks 'robe-find-file-hook))))
