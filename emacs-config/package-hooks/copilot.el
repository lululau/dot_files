(with-eval-after-load 'copilot

  (defun copilot--generate-doc ()
    "Generate doc param for completion request."
    (list :source (concat (buffer-substring-no-properties (point-min) (point-max)) "\n")
          :tabSize tab-width
          :indentSize tab-width
          :insertSpaces (if indent-tabs-mode :false t)
          :path (copilot--buffer-file-path)
          :relativePath (copilot--buffer-file-name)
          :languageId (s-chop-suffix "-mode" (symbol-name major-mode))
          :position (list :line (1- (line-number-at-pos))
                          :character (length (buffer-substring-no-properties (point-at-bol) (point))))))

  (defun copilot-complete ()
    "Complete at the current point."
    (interactive)
    (copilot-clear-overlay)
    (setq copilot--completion-cache nil)
    (setq copilot--completion-idx 0)
    (when t
      (copilot--get-completion
       (lambda (result)
         (copilot--log "[INFO] Completion: %S" result)
         (let* ((completions (alist-get 'completions result))
                (completion (if (seq-empty-p completions) (progn (message "No copilot completion.") nil) (seq-elt completions 0))))
           (copilot--show-completion completion))))))

  (defun copilot--buffer-file-path ()
    (or (buffer-file-name) ""))

  (defun copilot--buffer-file-name ()
    (let ((buffer-file-name (buffer-file-name)))
      (if buffer-file-name
          (file-name-nondirectory buffer-file-name)
        "")))

  (defun copilot-complete-if-insert-state ()
    (interactive)
    (when (not (seq-contains-p '(copilot-complete copilot-next-completion copilot-previous-completion) this-command))
      (copilot-clear-overlay)
      (when (and (evil-insert-state-p) (not (seq-contains-p '(vterm-mode) major-mode)))
        (copilot-complete))))

  (defun copilot-toggle-auto-copilot ()
    (interactive)
    (if (bound-and-true-p copilot--auto-copilot-on-p)
        (progn (remove-hook 'post-command-hook 'copilot-complete-if-insert-state)
               (setq copilot--auto-copilot-on-p nil)
               (message "Auto Copilot off!"))
      (add-hook 'post-command-hook 'copilot-complete-if-insert-state)
      (setq copilot--auto-copilot-on-p t)
      (message "Auto Copilot on!")))

  (add-hook 'post-command-hook #'copilot-complete-if-insert-state)
  (add-hook 'evil-insert-state-exit-hook #'copilot-clear-overlay)
  (add-hook 'evil-hybrid-state-exit-hook #'copilot-clear-overlay)
  (setq copilot--auto-copilot-on-p t))
