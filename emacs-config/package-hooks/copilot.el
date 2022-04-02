(with-eval-after-load 'copilot

  (defun copilot--generate-doc ()
    "Generate doc param for completion request."
    (let ((history nil))
      (if (eq 'pry-vterm-mode major-mode)
          (setq history (tail-f "~/.pry_history" 5000)))
      (if (eq 'zsh-vterm-mode major-mode)
          (setq history (tail-f "~/.zsh_history" 5000 "tail -n %d -f %s | zsh-histfile-unmetafy")))
      (list :source (copilot--get-source history)
            :tabSize tab-width
            :indentSize tab-width
            :insertSpaces (if indent-tabs-mode :false t)
            :path (copilot--buffer-file-path)
            :relativePath (copilot--buffer-file-name)
            :languageId (copilot--get-language-id)
            :position (copilot--get-position history))))

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

  (defun copilot-accept-completion ()
    "Accept completion. Return t if there is a completion."
    (interactive)
    (when copilot--overlay
      (let ((completion (overlay-get copilot--overlay 'completion))
            (start (overlay-get copilot--overlay 'start)))
        (copilot-clear-overlay)
        (if (seq-contains-p '(pry-vterm-mode zsh-vterm-mode) major-mode)
            (vterm-send-string completion)
          (delete-region start (line-end-position))
          (insert completion))
        t)))

  (defun copilot--show-completion (completion)
    "Show COMPLETION."
    (when completion
      (let* ((text (alist-get 'text completion))
             (range (alist-get 'range completion))
             (start (alist-get 'start range))
             (start-line (alist-get 'line start))
             (start-char (alist-get 'character start)))
        (if (seq-contains-p '(pry-vterm-mode zsh-vterm-mode) major-mode)
            (copilot-display-overlay-completion text (1- (line-number-at-pos)) 0)
          (copilot-display-overlay-completion text start-line start-char)))))

  (defun copilot-display-overlay-completion (completion line col)
    "Show COMPLETION in overlay at LINE and COL. For Copilot, COL is always 0."
    (copilot-clear-overlay)
    (save-excursion
      (when (not (seq-contains-p '(pry-vterm-mode zsh-vterm-mode) major-mode))
        (widen)
        (goto-char (point-min))
        (if (= (line-end-position line) (1- (point-max)))
                                        ; special case if the last line is empty
            (progn
              (goto-char (point-max))
              (newline)
              (forward-char -1))
          (forward-line line)
          (forward-char col)))

                                        ; remove common prefix
      (let* ((cur-line (copilot--get-current-line))
             (common-prefix-len (length (s-shared-start completion cur-line))))
        (setq completion (substring completion common-prefix-len))
        (when (not (seq-contains-p '(pry-vterm-mode zsh-vterm-mode) major-mode))
          (forward-char common-prefix-len)))

      (unless (s-blank? completion)
        (let* ((ov (make-overlay (point) (point-at-eol) nil t t))
               (p-completion (propertize completion 'face 'all-the-icons-dyellow))
               (display (substring p-completion 0 1))
               (after-string (substring p-completion 1)))
          (overlay-put ov 'completion completion)
          (overlay-put ov 'start (point))
          (if (equal (overlay-start ov) (overlay-end ov))
              (progn
                (put-text-property 0 1 'cursor t p-completion)
                (overlay-put ov 'after-string p-completion))
            (overlay-put ov 'display display)
            (overlay-put ov 'after-string after-string))
          (setq copilot--overlay ov)))))

  (defun copilot--get-current-line ()
    (if (eq 'pry-vterm-mode major-mode)
        (pry-vterm-get-current-line)
      (if (eq 'zsh-vterm-mode major-mode)
          (zsh-vterm-get-current-line)
        (s-chop-suffix "\n" (thing-at-point 'line)))))

  (defun copilot--get-source (&optional history)
    (let ((source (if (eq 'pry-vterm-mode major-mode)
                      (concat (s-join "\n" history) "\n" (pry-vterm-get-current-line))
                    (if (eq 'zsh-vterm-mode major-mode)
                        (concat (mapconcat (lambda (it)
                                     (let ((s it))
                                       (if (s-starts-with? ": " s)
                                           (substring s 15)
                                         s))) history "\n") "\n" (zsh-vterm-get-current-line)) ;; TODO add `mv file ./' to source, which file is each file of current directory
                      (buffer-substring-no-properties (point-min) (point-max))))))
        (concat source "\n")))

  (defun copilot--get-language-id ()
    (if (eq 'pry-vterm-mode major-mode)
        "ruby"
      (if (eq 'zsh-vterm-mode major-mode)
          "zsh"
          (s-chop-suffix "-mode" (symbol-name major-mode)))))

  (defun copilot--get-position (&optional history)
    (if (eq 'pry-vterm-mode major-mode)
        (list :line (length history) :character (length (pry-vterm-get-current-line)))
      (if (eq 'zsh-vterm-mode major-mode)
          (list :line (length history) :character (length (zsh-vterm-get-current-line)))
      (list :line (1- (line-number-at-pos))
            :character (length (buffer-substring-no-properties (point-at-bol) (point)))))))

  (defun copilot--buffer-file-path ()
    (or (buffer-file-name) ""))

  (defun copilot--buffer-file-name ()
    (let ((buffer-file-name (buffer-file-name)))
      (if buffer-file-name
          (file-name-nondirectory buffer-file-name)
        "")))

  (defun copilot-toggle-auto-copilot ()
    (interactive)
    (if (bound-and-true-p copilot--auto-copilot-on-p)
        (progn (remove-hook 'post-command-hook 'copilot-complete-if-insert-state)
               (setq copilot--auto-copilot-on-p nil)
               (message "Auto Copilot off!"))
      (add-hook 'post-command-hook 'copilot-complete-if-insert-state)
      (setq copilot--auto-copilot-on-p t)
      (message "Auto Copilot on!")))

  (add-hook 'evil-insert-state-exit-hook #'copilot-clear-overlay)
  (add-hook 'evil-hybrid-state-exit-hook #'copilot-clear-overlay))

(defun copilot-complete-if-insert-state ()
  (interactive)
  (when (not (seq-contains-p '(copilot-complete copilot-next-completion copilot-previous-completion lx/keyboard-quit) this-command))
    (copilot-clear-overlay)
    (when (and (evil-insert-state-p) (not (seq-contains-p '(vterm-mode pry-vterm-mode zsh-vterm-mode) major-mode)))
      (copilot-complete))))

(add-hook 'post-command-hook #'copilot-complete-if-insert-state)
(setq copilot--auto-copilot-on-p t)
