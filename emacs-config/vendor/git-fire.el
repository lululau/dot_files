;; -*- lexical-binding: t; -*-
(require 'magit)

(defun git-fire ()
  "Stage all changes, commit as fire commit, then reset hard.
Working tree becomes clean.  Recover via git reflog."
  (interactive)
  (magit-run-git "add" ".")
  (magit-run-git "commit" "-m"
                  (format "GIT ON FIRE: %s" (format-time-string "%F %T")))
  (let ((rev (magit-rev-parse "HEAD")))
    (magit-run-git "reset" "--hard" "HEAD^")
    (message "Fire commit saved: %s" (substring rev 0 7))))

(provide 'git-fire)
