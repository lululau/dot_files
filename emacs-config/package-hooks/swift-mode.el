(spacemacs|use-package-add-hook swift-mode
  :post-config
  (defun swift-mode:send-line ()
    "Send the line to the Swift REPL process."
    (interactive)
    (swift-mode:send-region (line-beginning-position) (line-end-position)))

  (defun swift-mode:send-paragraph ()
    "Send the paragraph to the Swift REPL process."
    (interactive)
    (let ((start (save-excursion
                   (backward-paragraph)
                   (point)))
          (end (save-excursion
                 (forward-paragraph)
                 (point))))
      (swift-mode:send-region start end)))

  (spacemacs/set-leader-keys-for-major-mode 'swift-mode
    "sS" 'swift-mode:run-repl      ; run or switch to an existing swift repl
    "ss" 'swift-mode:run-repl
    "sb" 'swift-mode:send-buffer
    "sp" 'swift-mode:send-paragraph
    "sl" 'swift-mode:send-line
    "sr" 'swift-mode:send-region)
  (define-key swift-mode-map (kbd "M-j") 'nil))
