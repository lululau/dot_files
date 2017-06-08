(with-eval-after-load 'clean-aindent-mode
  (defun clean-aindent--bsunindent(arg)
    "Unindents.
Bound to `M-backspace' key. Searches lines backward, finds the one that
is indented less than the current one. Unindents current line to
align with that smaller indentation"
    (interactive "p")
    (if (eq major-mode 'term-mode)
        (term-send-raw-meta)
      (if (not (clean-aindent--inside-indentp))
          (kill-word (- arg))  ;; Original "C-backspace" key function
        ;; else: cursor is inside indent space, do unindent
        (let*
            ((ln (clean-aindent--line-point))
             (c (current-indentation))
             (n (clean-aindent--find-u-indent c))  ;; compute new indent
             (s (+ ln n)))  ;; start of region to delete
          (if (not (= s c))
              (progn
                ;; (message "new unindent %d" n)
                ;; Delete characters between s to c
                (clean-aindent--goto-column c)
                (backward-delete-char-untabify (- c n)))))))))
