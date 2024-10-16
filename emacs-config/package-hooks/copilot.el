;;;  -*- lexical-binding: t -*-
(defvar copilot-auto-copilot-inhibit-commands '(copilot-complete
                                                copilot-next-completion
                                                copilot-previous-completion
                                                lx/keyboard-quit
                                                delete-char
                                                backward-delete-char-untabify
                                                copilot-accept-or-org-cycel))

;; (defun vterm-get-current-input ()
;;   (interactive)
;;   (let* ((begin (vterm--get-beginning-of-line))
;;          (point (vterm--get-cursor-point))
;;          (input (buffer-substring-no-properties begin point))
;;          (input (replace-regexp-in-string "^.*│" "" input))
;;          (input (replace-regexp-in-string "^❯ *" "" input)))
;;     input))

(with-eval-after-load 'copilot
  (defun copilot--generate-doc ()
    "Generate doc param for completion request."
    ;; Begin Add
    (let* ((hist nil)
           (history nil)
           (code-line-found nil))
      (if (eq 'pry-vterm-mode major-mode)
          (setq hist (tail-f "~/.pry_history" 500)))
      (if (seq-contains-p '(zsh-vterm-mode ssh-zsh-vterm-mode) major-mode)
          (setq hist '("" "# Every line begins with # is the comment of the following line, and every line only has one line comment" "#!/bin/zsh")
                             ))
      (if (bound-and-true-p git-commit-mode)
          (setq history (lx/git-commit-get-diff-lines)))

      (mapc (lambda (line) (setq history (append (list line) history))) (reverse hist))

      ;; End Add

      ;; (list :source (concat (buffer-substring-no-properties (point-min) (point-max)) "\n")
      (list :source (copilot--get-source history) ;; Mod
            :tabSize (copilot--infer-indentation-offset)
            :indentSize (copilot--infer-indentation-offset)
            :insertSpaces (if indent-tabs-mode :json-false t)
            ;; :path (buffer-file-name)
            :path (copilot--buffer-file-path) ;; Mod
            :relativePath (copilot--get-relative-path)
            :uri (copilot--get-uri)
            ;; :languageId (s-chop-suffix "-mode" (symbol-name major-mode))
            :languageId (copilot--get-language-id) ;; Mod
            ;; :position (list :line (1- (line-number-at-pos))
            ;;                 :character (length (buffer-substring-no-properties (point-at-bol) (point))))))
            :position (copilot--get-position history)))) ;; Mod

  (defun copilot-accept-completion (&optional transform-fn)
    "Accept completion. Return t if there is a completion. Use TRANSFORM-FN to transform completion if provided."
    (interactive)
    (when (copilot--overlay-visible)
      (let* ((completion (overlay-get copilot--overlay 'completion))
             (start (overlay-get copilot--overlay 'start))
             (uuid (overlay-get copilot--overlay 'uuid))
             (t-completion (funcall (or transform-fn #'identity) completion)))
        (copilot--async-request 'notifyAccepted (list :uuid uuid))
        (copilot-clear-overlay)
        ;; (delete-region start (line-end-position))
        ;; (insert t-completion)
        (cond ((eq 'pry-vterm-mode major-mode)
               (vterm-send-string (concat t-completion " ")))
              ((seq-contains-p '(zsh-vterm-mode ssh-zsh-vterm-mode) major-mode)
               (vterm-send-return)
               (vterm-send-string (concat t-completion " ")))
              (t
               (delete-region start (line-end-position))
               (insert t-completion)))
                                        ; trigger completion again if not fully accepted
        (unless (equal completion t-completion)
          (copilot-complete))
        t)))

  (defun copilot--show-completion (completion)
    "Show COMPLETION."
    (copilot--dbind (:text :uuid :range (:start (:line :character))) completion
                    (cond ;; Mod
                     ((seq-contains-p '(pry-vterm-mode) major-mode)
                      (copilot-display-overlay-completion text uuid (1- (line-number-at-pos)) 0 (point)))
                     ((seq-contains-p '(zsh-vterm-mode ssh-zsh-vterm-mode) major-mode)
                      (copilot-display-overlay-completion text uuid (1- (line-number-at-pos)) 0 (save-excursion (let ((line-pos (zsh-vterm-get-current-line-beginning))) (forward-line 1) (forward-char line-pos)) (point))))
                     ((bound-and-true-p git-commit-mode)
                      (copilot-display-overlay-completion text uuid (- line (lx/git-commit-get-diff-line-nums)) character (point)))
                     (t
                      (copilot-display-overlay-completion text uuid line character (point))))))

  (defun copilot-display-overlay-completion (completion uuid line col user-pos)
    "Show COMPLETION in overlay at LINE and COL. For Copilot, COL is always 0.
USER-POS is the cursor position (for verification only)."
    (copilot-clear-overlay)

    (save-excursion
      (cond  ;; Mod
       ((seq-contains-p '(pry-vterm-mode) major-mode) t)
       ((seq-contains-p '(zsh-vterm-mode ssh-zsh-vterm-mode) major-mode) (let ((line-pos (zsh-vterm-get-current-line-beginning))) (forward-line 1) (forward-char line-pos)))
       (t (progn
            (widen)
            (goto-char (point-min))
            (if (= (line-end-position line) (1- (point-max)))
                                        ; special case if the last line is empty
                (progn
                  (goto-char (point-max))
                  (newline)
                  (forward-char -1))
              (forward-line line)
              (forward-char col)))))

                                        ; remove common prefix
      (let* ((cur-line (copilot--get-current-line)) ;; Mod
             (common-prefix-len (length (s-shared-start completion cur-line))))
        (setq completion (substring completion common-prefix-len))
        (when (not (seq-contains-p '(pry-vterm-mode zsh-vterm-mode ssh-zsh-vterm-mode) major-mode)) ;; Mod
          (forward-char common-prefix-len)))

      (when (and (s-present-p completion)
                 (or (= (point) user-pos) ; up-to-date completion
                     (and (< (point) user-pos) ; special case for removing indentation
                          (s-blank-p (s-trim (buffer-substring-no-properties (point) user-pos))))))
        (let* ((p-completion (propertize completion 'face 'all-the-icons-cyan-alt))
               (ov (if (not (overlayp copilot--overlay))
                       (make-overlay (point) (point-at-eol) nil nil t)
                     (move-overlay copilot--overlay (point) (point-at-eol))
                     copilot--overlay)))
          (if (= (overlay-start ov) (overlay-end ov)) ; end of line
              (progn
                (setq copilot--real-posn (cons (point) (posn-at-point)))
                (put-text-property 0 1 'cursor t p-completion)
                (overlay-put ov 'after-string p-completion))
            (overlay-put ov 'display (substring p-completion 0 1))
            (overlay-put ov 'after-string (substring p-completion 1)))
          (overlay-put ov 'completion completion)
          (overlay-put ov 'start (point))
          (overlay-put ov 'uuid uuid)
          (overlay-put ov 'keymap copilot-completion-map)
          (setq copilot--overlay ov)
          (copilot--async-request 'notifyShown (list :uuid uuid))))))

  (defun copilot-accept-completion-by-word (n-word)
    "Accept first N-WORD words of completion."
    (interactive "p")
    (setq n-word (or n-word 1))
    (copilot-accept-completion (lambda (completion)
                                 (let* ((blank-regexp '(any blank punct "\r" "\n"))
                                        (separator-regexp (rx-to-string
                                                           `(seq
                                                             (not ,blank-regexp)
                                                             (1+ ,blank-regexp))))
                                        (words (s-split-up-to separator-regexp completion n-word))
                                        (remain (if (<= (length words) n-word)
                                                    ""
                                                  (cl-first (last words))))
                                        (length (- (length completion) (length remain)))
                                        (prefix (substring completion 0 length)))
                                   (s-trim-right prefix)))))

  (defun copilot-accept-completion-by-line (n-line)
    "Accept first N-LINE lines of completion."
    (interactive "p")
    (setq n-line (or n-line 1))
    (copilot-accept-completion (lambda (completion)
                                 (let* ((lines (s-split-up-to (rx anychar (? "\r") "\n") completion n-line))
                                        (remain (if (<= (length lines) n-line)
                                                    ""
                                                  (cl-first (last lines))))
                                        (length (- (length completion) (length remain)))
                                        (prefix (substring completion 0 length)))
                                   (s-chomp prefix)))))

  ;; Begin Add

  (defun copilot--get-current-line ()
    (cond ((eq 'pry-vterm-mode major-mode)
           (pry-vterm-get-current-line))
          ((seq-contains-p '(zsh-vterm-mode ssh-zsh-vterm-mode) major-mode) "")
          (t
           (s-chop-suffix "\n" (or (thing-at-point 'line) "")))))

  (defun copilot--get-source (&optional history)
    (let ((source (cond ((eq 'pry-vterm-mode major-mode)
                         (concat (s-join "\n" history) "\n" (pry-vterm-get-current-line)))
                        ((seq-contains-p '(zsh-vterm-mode ssh-zsh-vterm-mode) major-mode)
                         (concat (s-join "\n" history) "\n" (zsh-vterm-get-current-line) "\n"))
                        ((bound-and-true-p git-commit-mode)
                         (concat (s-join "\n" history) "\n" (lx/git-commit-get-message)))
                        (t
                         (buffer-substring-no-properties (point-min) (point-max))))))
      (concat source "\n")))


  (defun copilot--get-language-id ()
    (cond ((eq 'pry-vterm-mode major-mode) "ruby")
          ((seq-contains-p '(zsh-vterm-mode ssh-zsh-vterm-mode) major-mode) "zsh")
          ((bound-and-true-p git-commit-mode) "git")
          (t (s-chop-suffix "-mode" (symbol-name major-mode)))))

  (defun copilot--get-position (&optional history)
    (cond ((eq 'pry-vterm-mode major-mode)
           (list :line (length history) :character (length (pry-vterm-get-current-line))))
          ((seq-contains-p '(zsh-vterm-mode ssh-zsh-vterm-mode) major-mode)
           (list :line (1+ (length history)) :character 0))
          ((bound-and-true-p git-commit-mode)
           (list :line (1- (+ (length history) (line-number-at-pos))) :character (- (point) (point-at-bol))))
          (t
           (list :line (1- (line-number-at-pos))
                 :character (- (point) (point-at-bol))))))

  (defun copilot--buffer-file-path ()
    (or (buffer-file-name) ""))

  (defun copilot-toggle-auto-copilot ()
    (interactive)
    (if (bound-and-true-p copilot--auto-copilot-on-p)
        (progn (remove-hook 'post-command-hook 'copilot-complete-if-insert-state)
               (setq copilot--auto-copilot-on-p nil)
               (message "Auto Copilot off!"))
      (add-hook 'post-command-hook 'copilot-complete-if-insert-state)
      (setq copilot--auto-copilot-on-p t)
      (message "Auto Copilot on!")))

  (unless copilot--connection
    (copilot--start-agent))

  (add-hook 'evil-insert-state-exit-hook #'copilot-clear-overlay)
  (add-hook 'evil-hybrid-state-exit-hook #'copilot-clear-overlay))

(defun copilot-complete-if-insert-state ()
  (interactive)
  (when (not (seq-contains-p copilot-auto-copilot-inhibit-commands this-command))
    (copilot-clear-overlay)
    (when (and (evil-insert-state-p)
               (not (seq-contains-p '(vterm-mode pry-vterm-mode zsh-vterm-mode ssh-zsh-vterm-mode) major-mode)))
               ;; (not (bound-and-true-p git-commit-mode)))
      (copilot-complete))))

(if (bound-and-true-p copilot--auto-copilot-on-p)
    (add-hook 'post-command-hook 'copilot-complete-if-insert-state))

;; End Add
