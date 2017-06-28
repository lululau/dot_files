(with-eval-after-load 'ensime-sbt
  (defun ensime-sbt-switch ()
    (interactive)
    (split-window-right-and-focus)
    (call-interactively 'ensime-sbt)))
