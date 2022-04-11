(defvar helm-vterm-completion-last-buffer nil)

(defun helm-vterm-completion-candidates ()
  (with-current-buffer helm-vterm-completion-last-buffer
  (let* ((current-line (buffer-substring-no-properties (line-beginning-position) (point)))
         (word-at-point (car (last (s-split-words current-line))))
         (vterm-content (buffer-substring-no-properties (point-min) (point-max)))
         (vterm-lines (mapcar 's-trim (split-string vterm-content "[\n│]" t)))
         (vterm-WORDS (mapcan (lambda (line) (split-string line "\\s-+" t)) vterm-lines))
         (vterm-words (mapcan (lambda (line) (s-split-words line)) vterm-lines))
         (candidates (append vterm-lines vterm-WORDS vterm-words))
         (candidates (seq-filter (lambda (candidate) (string-prefix-p word-at-point candidate)) candidates)))
    (sort candidates #'string<)
    (seq-uniq candidates))))

(defun helm-vterm-completion-insert (candidate)
  (with-current-buffer helm-vterm-completion-last-buffer
  (let* ((current-line (buffer-substring-no-properties (line-beginning-position) (point)))
         (word-at-point (car (last (s-split-words current-line))))
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
