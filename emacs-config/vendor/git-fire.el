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

(defun git-fire-and-push ()
  "Create onfire branch, commit all changes, and push to remote."
  (interactive)
  (let* ((branch (or (magit-get-current-branch)
                     (user-error "Not on any branch")))
         (ts (format-time-string "%Y%m%d%H%M%S"))
         (fire-branch (format "onfire/%s/%s" branch ts))
         (remote (or (magit-get (format "branch.%s.pushRemote" branch))
                     (magit-get (format "branch.%s.remote" branch))
                     "origin")))
    (magit-run-git "checkout" "-b" fire-branch)
    (magit-run-git "add" ".")
    (magit-run-git "commit" "-m"
                    (format "On Fire. Branch: %s, Time: %s"
                            branch (format-time-string "%F %T")))
    (magit-run-git "push" "-u" remote fire-branch)
    (message "Fire pushed to %s/%s" remote fire-branch)))

(provide 'git-fire)
