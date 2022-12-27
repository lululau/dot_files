(with-eval-after-load 'inf-ruby

  (defun lx/last-pry-proc ()
    (or (get-buffer-process (--first (-contains? '(inf-ruby-mode term-mode) (with-current-buffer it major-mode)) (buffer-list)))
        (error "No current process.")))

  (defun ruby-send-region (start end &optional print)
    "Send the current region to the inferior Ruby process."
    (interactive "r\nP")
    (let (term (file (or buffer-file-name (buffer-name))) line)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char start)
          (setq line (+ start (forward-line (- start)) 1))
          (goto-char start)
          (while (progn
                   (setq term (apply 'format ruby-send-terminator (random) (current-time)))
                   (re-search-forward (concat "^" (regexp-quote term) "$") end t)))))
      ;; compilation-parse-errors parses from second line.
      (save-excursion
        (let ((m (process-mark (inf-ruby-proc))))
          (set-buffer (marker-buffer m))
          (goto-char m)
          (insert ruby-eval-separator "\n")
          (set-marker m (point))))
      (comint-send-region (inf-ruby-proc) start end)
      (when print (ruby-print-result))))

  (setq inf-ruby-prompt-format (format "\\(^ARQL@[^ ]* ❯\\)\\|%s" inf-ruby-prompt-format))
  (setq inf-ruby-first-prompt-pattern (format "\\(^ARQL@[^ ]* ❯\\)\\|%s" inf-ruby-first-prompt-pattern))
  (setq inf-ruby-prompt-pattern (format "\\(^ARQL@[^ ]* ❯\\)\\|%s" inf-ruby-prompt-pattern))
  )
