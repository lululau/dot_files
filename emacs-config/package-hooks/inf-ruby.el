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

  (defun lx/ruby-send-line ()
    (interactive)
    (ruby-send-region (line-beginning-position) (line-end-position))
    (comint-send-string (inf-ruby-proc) "\n"))

  (defun lx/ruby-send-line-and-go ()
    (interactive)
    (lx/ruby-send-line)
    (ruby-switch-to-inf t))

  (defun lx/ruby-send-reload ()
    (interactive)
    (comint-send-string (inf-ruby-proc) "reload!\n")
    (ruby-switch-to-inf t))

  (defun lx/ruby-send-paragraph ()
    (interactive)
    (let ((start (save-excursion
                   (backward-paragraph)
                   (point)))
          (end (save-excursion
                 (forward-paragraph)
                 (point))))
      (ruby-send-region start end)))

  (defun lx/ruby-send-paragraph-and-go ()
    (interactive)
    (lx/ruby-send-paragraph)
    (ruby-switch-to-inf t))

  (defun lx/ruby-send-babel-block ()
    (interactive)
    (comint-send-string (inf-ruby-proc) (concat (lx/get-babel-src) "\n")))

  (defun lx/ruby-send-babel-block-and-go ()
    (interactive)
    (lx/ruby-send-babel-block)
    (ruby-switch-to-inf t))

  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
    "sa" 'lx/ruby-send-reload
    "sl" 'lx/ruby-send-line
    "sL" 'lx/ruby-send-line-and-go
    "sp" 'lx/ruby-send-paragraph
    "sP" 'lx/ruby-send-paragraph-and-go)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "sp" 'lx/ruby-send-babel-block
    "sP" 'lx/ruby-send-babel-block-and-go)
  )
