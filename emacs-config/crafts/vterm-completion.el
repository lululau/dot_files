(defvar helm-vterm-completion-last-buffer nil)

;; (setq vterm-completion-script-dir (file-name-directory load-file-name))
(setq vterm-completion-script-dir (file-name-directory (buffer-file-name)))

(defun helm-vterm-completion-candidates ()
  (with-current-buffer helm-vterm-completion-last-buffer
  (let* ((current-line (buffer-substring-no-properties (line-beginning-position) (point)))
         (word-at-point (car (last (s-split-words current-line))))
         (WORD-at-point (car (last (split-string current-line "\\s-+" t))))
         (vterm-content (buffer-substring-no-properties (point-min) (point-max)))
         (vterm-content (if (not (string-prefix-p "*tmux" (buffer-name)))
                           vterm-content
                         (concat vterm-content (shell-command-to-string (format "%s/capture-all-tmux-panes.sh" vterm-completion-script-dir)))))
         (vterm-lines (mapcar (lambda (it) (replace-regexp-in-string  "^[^❯➜\\]]*\\(❯\\|\\]#\\|\\]\\$\\|➜\\) " "" (s-trim it))) (split-string vterm-content "[\n│]" t)))
         (vterm-WORDS (mapcan (lambda (line) (split-string line "\\s-+" t)) vterm-lines))
         (vterm-words (mapcan (lambda (WORD) (s-split-words WORD)) vterm-WORDS))
         (candidates (append vterm-lines vterm-WORDS vterm-words))
         (candidates (let (result)(dolist (candidate candidates result)
                                    (when (string-prefix-p word-at-point candidate)
                                      (setq result (cons (cons candidate (cons candidate word-at-point)) result)))
                                    (when (string-prefix-p WORD-at-point candidate)
                                      (setq result (cons (cons candidate (cons candidate WORD-at-point)) result)))
                                    (let ((idx (s-index-of word-at-point candidate)))
                                      (when idx
                                        (let ((sub-candidate (substring candidate idx)))
                                        (setq result (cons (cons sub-candidate (cons sub-candidate word-at-point)) result)))))))))

    (seq-uniq (seq-sort (lambda (a b) (string< (car a) (car b))) candidates)))))

(defun helm-vterm-completion-insert (selected)
  (with-current-buffer helm-vterm-completion-last-buffer
  (let* ((candidate (car selected))
         (word-at-point (cdr selected))
         (current-line (buffer-substring-no-properties (line-beginning-position) (point)))
        (to-insert (substring candidate (length word-at-point))))
    (vterm-send-string to-insert)
    (setq helm-vterm-completion-last-buffer nil))))

(defclass helm-vterm-completion-source (helm-source-sync)
  ((candidates :initform 'helm-vterm-completion-candidates)
   (action :initform '(("Insert completion" . helm-vterm-completion-insert)))
   (keymap :initform helm-map)))

(setq helm-vterm-completion-list
      (helm-make-source "vterm completion" 'helm-vterm-completion-source))

(defun vterm-completion ()
  (interactive)
  (setq helm-vterm-completion-last-buffer (current-buffer))
  (helm-other-buffer '(helm-vterm-completion-list) "*helm-vterm-completion*"))

(provide 'vterm-completion)
