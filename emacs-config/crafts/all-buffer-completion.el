(defvar helm-all-buffer-completion-last-buffer nil)

(setq helm-all-buffer-completion-script-dir (file-name-directory (or load-file-name (buffer-file-name))))

(defun helm-all-buffer-completion--all-emacs-buffer-lines (word)
  (mapcan (lambda (buf)
            (if (not (string-prefix-p "*tmux" (buffer-name buf)))
                (with-current-buffer buf
                  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                         (lines (s-lines content)))
                    (seq-filter (lambda (line) (s-contains-p word line)) lines)))))
          (buffer-list)))

(defun helm-all-buffer-completion--all-tmux-buffer-lines (word)
  (s-lines (shell-command-to-string
            (format "%s/capture-all-tmux-panes.sh '%s'" helm-all-buffer-completion-script-dir word))))

(defun helm-all-buffer-completion--all-lines (word)
  (append (helm-all-buffer-completion--all-emacs-buffer-lines word)
          (helm-all-buffer-completion--all-tmux-buffer-lines word)))

(defun helm-all-buffer-completion-candidates ()
  (with-current-buffer helm-all-buffer-completion-last-buffer
    (let* ((current-line (buffer-substring-no-properties (line-beginning-position) (point)))
           (word-at-point (car (last (s-split-words current-line)))))
      (when word-at-point
        (let* ((WORD-at-point (car (last (split-string current-line "\\s-+" t))))
               (lines (helm-all-buffer-completion--all-lines word-at-point))
               (WORDS (mapcan (lambda (line) (split-string line "\\s-+" t)) lines))
               (words (mapcan (lambda (WORD) (s-split-words WORD)) WORDS))
               (candidates (append lines WORDS words))
               (candidates
                (let (result)
                  (dolist (candidate candidates result)
                    (when (string-prefix-p word-at-point candidate)
                      (setq result (cons (cons candidate (cons candidate word-at-point)) result)))
                    (when (string-prefix-p WORD-at-point candidate)
                      (setq result (cons (cons candidate (cons candidate WORD-at-point)) result)))
                    (let ((idx (s-index-of word-at-point candidate)))
                      (when idx
                        (let ((sub-candidate (substring candidate idx)))
                          (setq result
                                (cons (cons sub-candidate (cons sub-candidate word-at-point)) result)))))))))
          (seq-uniq (seq-sort (lambda (a b) (< (length (car a)) (length (car b)))) candidates)))))))

(defun helm-all-buffer-completion-insert (selected)
  (with-current-buffer helm-all-buffer-completion-last-buffer
    (let* ((insert-func (plist-get (symbol-plist major-mode) 'insert-function))
           (insert-func (or insert-func 'insert))
           (candidate (car selected))
           (word-at-point (cdr selected))
           (current-line (buffer-substring-no-properties (line-beginning-position) (point)))
           (to-insert (substring candidate (length word-at-point))))
      (funcall insert-func to-insert)
      (setq helm-all-buffer-completion-last-buffer nil))))

(defclass helm-all-buffer-completion-source (helm-source-sync)
  ((candidates :initform 'helm-all-buffer-completion-candidates)
   (action :initform '(("Insert completion" . helm-all-buffer-completion-insert)))
   (keymap :initform helm-map)))

(setq helm-all-buffer-completion-list
      (helm-make-source "All Buffer (Emacs and Tmux buffers) Completion" 'helm-all-buffer-completion-source))

(defun all-buffer-completion ()
  (interactive)
  (setq helm-all-buffer-completion-last-buffer (current-buffer))
  (helm-other-buffer '(helm-all-buffer-completion-list) "*helm-all-buffer-completion*"))

(provide 'all-buffer-completion)
