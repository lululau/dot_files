(defvar helm-current-buffer-completion-last-buffer nil)

(setq helm-current-buffer-completion-script-dir (file-name-directory (or load-file-name (buffer-file-name))))

(defun helm-current-buffer-completion--all-lines (word)
  (if (string-prefix-p "*tmux" (buffer-name))
      (s-lines (shell-command-to-string
                (format "%s/capture-current-tmux-window-panes.sh '%s'" helm-current-buffer-completion-script-dir word)))
    (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
           (lines (s-lines content)))
      (seq-filter (lambda (line) (s-contains-p word line)) lines))))

(defun helm-current-buffer-completion-candidates ()
  (with-current-buffer helm-current-buffer-completion-last-buffer
    (let* ((current-line (buffer-substring-no-properties (line-beginning-position) (point)))
           (word-at-point (car (last (s-split-words current-line)))))
      (when word-at-point
        (let* ((WORD-at-point (car (last (split-string current-line "\\s-+" t))))
               (lines (helm-current-buffer-completion--all-lines word-at-point))
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

(defun helm-current-buffer-completion--insert-function ()
  "Resolve the `insert-function' for the current major mode.
Walks the `derived-mode-parent' chain so that modes deriving from a
mode carrying an `insert-function' plist entry (e.g. ghostel modes
deriving from `ghostel-mode') inherit it.  Falls back to `insert'."
  (let ((mode major-mode) fn)
    (while (and mode (not fn))
      (setq fn (plist-get (symbol-plist mode) 'insert-function))
      (setq mode (get mode 'derived-mode-parent)))
    (or fn 'insert)))

(defun helm-current-buffer-completion-insert (selected)
  (with-current-buffer helm-current-buffer-completion-last-buffer
    (let* ((insert-func (helm-current-buffer-completion--insert-function))
           (candidate (car selected))
           (word-at-point (cdr selected))
           (current-line (buffer-substring-no-properties (line-beginning-position) (point)))
           (to-insert (substring candidate (length word-at-point))))
      (funcall insert-func to-insert)
      (setq helm-current-buffer-completion-last-buffer nil))))

(defclass helm-current-buffer-completion-source (helm-source-sync)
  ((candidates :initform 'helm-current-buffer-completion-candidates)
   (action :initform '(("Insert completion" . helm-current-buffer-completion-insert)))
   (keymap :initform helm-map)))

(setq helm-current-buffer-completion-list
      (helm-make-source "All Buffer (Emacs and Tmux buffers) Completion" 'helm-current-buffer-completion-source))

(defun current-buffer-completion ()
  (interactive)
  (setq helm-current-buffer-completion-last-buffer (current-buffer))
  (helm-other-buffer '(helm-current-buffer-completion-list) "*helm-current-buffer-completion*"))

(provide 'current-buffer-completion)
