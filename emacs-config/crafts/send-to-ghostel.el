;;;###autoload
(defun lx/ghostel-send-line ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (with-current-buffer ghostel-buffer
            (ghostel-send-string str))))))

;;;###autoload
(defun lx/ghostel-send-line-and-go ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (with-current-buffer ghostel-buffer
            (ghostel-send-string str))
          (select-window (get-buffer-window ghostel-buffer))))))

;;;###autoload
(defun lx/ghostel-send-paragraph ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))) "")))
          (with-current-buffer ghostel-buffer
            (ghostel-paste-string str)
            (ghostel-send-key "return"))))))


;;;###autoload
(defun lx/ghostel-send-paragraph-and-go ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))) "")))
          (with-current-buffer ghostel-buffer
            (ghostel-paste-string str)
            (ghostel-send-key "return"))
          (select-window (get-buffer-window ghostel-buffer))))))


;;;###autoload
(defun lx/ghostel-send-region ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (region-beginning) (region-end)) "")))
          (with-current-buffer ghostel-buffer
            (ghostel-paste-string str)
            (ghostel-send-key "return"))))))


;;;###autoload
(defun lx/ghostel-send-region-and-go ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (region-beginning) (region-end)) "")))
          (with-current-buffer ghostel-buffer
            (ghostel-paste-string str)
            (ghostel-send-key "return"))
          (select-window (get-buffer-window ghostel-buffer))))))


;;;###autoload
(defun lx/ghostel-send-babel-block ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (lx/get-babel-src) "\n")))
          (with-current-buffer ghostel-buffer
            (ghostel-paste-string str)
            (ghostel-send-key "return"))))))


;;;###autoload
(defun lx/ghostel-send-babel-block-and-go ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (lx/get-babel-src) "\n")))
          (with-current-buffer ghostel-buffer
            (ghostel-paste-string str)
            (ghostel-send-key "return"))
          (select-window (get-buffer-window ghostel-buffer))))))

;;;###autoload
(defun lx/find-ghostel-buffer ()
  (let* ((window-buffers (mapcar #'window-buffer (window-list))))
    (or
      (--find (with-current-buffer it (eq major-mode 'pry-ghostel-mode)) window-buffers)
      (--find (with-current-buffer it (derived-mode-p 'ghostel-mode)) window-buffers))))
