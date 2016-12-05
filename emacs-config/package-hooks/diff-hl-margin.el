(with-eval-after-load 'diff-hl-margin
  (define-minor-mode diff-hl-margin-minor-mode
    "Toggle displaying `diff-hl-mode' highlights on the margin locally.
You probably shouldn't use this function directly."
    :lighter ""
    (let ((width-var (intern (format "%s-margin-width" diff-hl-side))))
      (if diff-hl-margin-minor-mode
          (progn
            (set (make-local-variable 'diff-hl-margin-old-highlight-function)
                 diff-hl-highlight-function)
            (set (make-local-variable 'diff-hl-highlight-function)
                 'diff-hl-highlight-on-margin)
            (set width-var 1))
        (if (bound-and-true-p diff-hl-margin-old-highlight-function)
            (setq diff-hl-highlight-function diff-hl-margin-old-highlight-function
                  diff-hl-margin-old-highlight-function nil))
        (set width-var 0)))
    (dolist (win (get-buffer-window-list))
      (set-window-buffer win (current-buffer)))))
