(evil-define-text-object evil-same-match-lines (count &optional beg end type)
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (regexp (read-regexp "Regexp: "))
         (beg (or beg (point)))
         (end (or end (point))))
    (save-excursion
      (catch :buf-beg
        (while (string-match regexp line)
          (setq beg (line-beginning-position))
          (when (= (line-beginning-position) (point-min))
            (throw :buf-beg nil))
          (forward-line -1)
          (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          )))
    (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
    (save-excursion
      (catch :buf-end
        (while (string-match regexp line)
          (setq end (line-end-position))
          (when (= (line-end-position) (point-max))
            (throw :buf-end nil))
          (forward-line 1)
          (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          )))
    (list beg end)))


(define-key evil-outer-text-objects-map "r" 'evil-same-match-lines)
(define-key evil-inner-text-objects-map "r" 'evil-same-match-lines)
