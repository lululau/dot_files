;; -*- lexical-binding: t; -*-
(require 'run-in-ghostel)

(defun ghostel-agent--agent-buffer-name-p (name)
  "Return non-nil if NAME looks like an agent Ghostel buffer from `ghostel-agent-run-*`."
  (string-match-p "\\`\\*ghostel-\\(?:claude\\|opencode\\|cursor\\|agy\\)\\[" name))

(defun ghostel-agent--agent-buffer-p (buffer)
  "Return non-nil if BUFFER is a live Ghostel agent shell."
  (and (buffer-live-p buffer)
       (ghostel-agent--agent-buffer-name-p (buffer-name buffer))
       (with-current-buffer buffer (derived-mode-p 'ghostel-mode))))

(defun ghostel-agent--first-visible-agent-buffer ()
  "Return the first Ghostel agent buffer displayed in some window on a visible frame.

Walk `(frame-list)' order; for each frame use `(window-list FRAME nomini)',
so minibuffer-only windows are skipped.

Do not use `(window-list nil nomini t)': third argument is FRAME on some Emacs
releases, passing `windowp t`-style errors when given `t'."
  (catch 'ghostel-agent--found
    (dolist (frm (frame-list))
      (when (frame-visible-p frm)
        (dolist (win (window-list frm 'nomini))
          (let ((buf (window-buffer win)))
            (when (ghostel-agent--agent-buffer-p buf)
              (throw 'ghostel-agent--found buf))))))))

(defun ghostel-agent-run-claude ()
  "Start a Claude agent shell in Ghostel."
  (interactive)
  (lx/run-in-projectile-ghostel "claude --dangerously-skip-permissions" "*ghostel-claude[%p]*"))

(defun ghostel-agent-run-opencode()
  "Start an Opencode agent shell in Ghostel."
  (interactive)
  (lx/run-in-projectile-ghostel "opencode" "*ghostel-opencode[%p]*"))

(defun ghostel-agent-run-cursor ()
  "Start a Cursor agent shell in Ghostel."
  (interactive)
  (lx/run-in-projectile-ghostel "cursor-agent --force" "*ghostel-cursor[%p]*"))

(defun ghostel-agent-run-antigravity ()
  "Start an Antigravity agent shell in Ghostel."
  (interactive)
  (lx/run-in-projectile-ghostel "agy --dangerously-skip-permissions" "*ghostel-agy[%p]*"))

(defalias 'ghostel-agent-run-agy #'ghostel-agent-run-antigravity
  "Compatibility alias bound in `global-set-key.el'.")

(defun ghostel-agent-send ()
  "Send the current file or selection to a visible Ghostel agent shell.
If the region is not active, sends @ABSOLUTE_FILE_PATH.
Otherwise sends \"ABSOLUTE_FILE_PATH 中的第 M-N 行:\" plus a newline and the region text."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  (let ((agent-buffer (ghostel-agent--first-visible-agent-buffer)))
    (unless agent-buffer
      (user-error "No visible Ghostel agent buffer; show claude/opencode/cursor/agy Ghostel first"))
    (let* ((abs-path (expand-file-name (buffer-file-name)))
           (str
            (if (use-region-p)
                (let* ((beg (region-beginning))
                       (end (region-end))
                       (lo (min beg end))
                       (hi (max beg end))
                       (hi-line-pos (max lo (1- hi)))
                       (m (save-excursion (goto-char lo) (line-number-at-pos)))
                       (n (save-excursion (goto-char hi-line-pos) (line-number-at-pos)))
                       (selection (buffer-substring-no-properties lo hi)))
                  (concat abs-path " 中的第 "
                          (number-to-string m) "-" (number-to-string n)
                          " 行:\n" selection "\n"))
              (concat "@" abs-path "\n"))))
      (with-current-buffer agent-buffer
        (ghostel-send-string str)))))

(provide 'ghostel-agent)
