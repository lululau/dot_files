(with-eval-after-load 'ob-tmux
  (defun ob-tmux--window-alive-p (ob-session)
    "Check if WINDOW exists in tmux session.

If no window is specified in OB-SESSION, returns 't."
    (let* ((window (ob-tmux--window ob-session))
           (target (ob-tmux--target ob-session))
           (output (ob-tmux--execute-string ob-session
                                            "list-panes"
                                            "-F 'yes_exists'"
                                            "-t" (concat "'" target "'"))))
      (cond (window
             (string-match-p "^yes_exists\n" output))
            ((null window)
             't)))))
