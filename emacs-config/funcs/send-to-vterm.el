;;;###autoload
(defun lx/vterm-send-line ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str nil))))))

;;;###autoload
(defun lx/vterm-send-line-and-go ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str nil))
          (select-window (get-buffer-window vterm-buffer))))))

;;;###autoload
(defun lx/vterm-send-paragraph ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))) "")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))))))


;;;###autoload
(defun lx/vterm-send-paragraph-and-go ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))) "")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))
          (select-window (get-buffer-window vterm-buffer))))))


;;;###autoload
(defun lx/vterm-send-region ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (region-beginning) (region-end)) "")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))))))


;;;###autoload
(defun lx/vterm-send-region-and-go ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (region-beginning) (region-end)) "")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))
          (select-window (get-buffer-window vterm-buffer))))))


;;;###autoload
(defun lx/vterm-send-babel-block ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (lx/get-babel-src) "\n")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))))))


;;;###autoload
(defun lx/vterm-send-babel-block-and-go ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (lx/get-babel-src) "\n")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))
          (select-window (get-buffer-window vterm-buffer))))))

;;;###autoload
(defun lx/find-vterm-buffer ()
  (let* ((window-buffers (mapcar #'window-buffer (window-list))))
    (or
      (--find (with-current-buffer it (eq major-mode 'pry-vterm-mode)) window-buffers)
      (--find (with-current-buffer it (derived-mode-p 'vterm-mode)) window-buffers))))

