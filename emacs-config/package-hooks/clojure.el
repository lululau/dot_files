(with-eval-after-load 'clojure-mode
  (defun lx/clojure-ctrl-j ()
    (interactive)
    (copilot-clear-overlay)
    (call-interactively 'cider-eval-defun-to-comment)
    (call-interactively 'next-line)
    (call-interactively 'electric-newline-and-maybe-indent)
    (copilot-clear-overlay))

  (define-key clojure-mode-map (kbd "C-j") 'lx/clojure-ctrl-j))
