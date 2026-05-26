;; -*- lexical-binding: t; -*-

(with-eval-after-load 'orgit

  (defun orgit-status-store () nil)

  (defun orgit-log-store () nil)

  (defun orgit-rev-store ()
    (cond-let ((eq major-mode 'magit-revision-mode)
               (orgit-rev-store-1 magit-buffer-revision))
              ([_(derived-mode-p 'magit-mode)]
               [revs (or (magit-region-values 'commit)
                         (when-let ((c (magit-commit-at-point))) (list c)))]
               (mapc #'orgit-rev-store-1 revs)
               t))))
