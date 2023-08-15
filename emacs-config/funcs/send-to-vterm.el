(defun lx/vterm-send-line ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str nil))))))

(defun lx/vterm-send-line-and-go ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str nil))
          (select-window (get-buffer-window vterm-buffer))))))

(defun lx/vterm-send-paragraph ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))) "")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))))))


(defun lx/vterm-send-paragraph-and-go ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))) "")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))
          (select-window (get-buffer-window vterm-buffer))))))


(defun lx/vterm-send-region ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (region-beginning) (region-end)) "")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))))))


(defun lx/vterm-send-region-and-go ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (buffer-substring (region-beginning) (region-end)) "")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))
          (select-window (get-buffer-window vterm-buffer))))))


(defun lx/vterm-send-babel-block ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (lx/get-babel-src) "\n")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))))))


(defun lx/vterm-send-babel-block-and-go ()
  (interactive)
  (let ((vterm-buffer (lx/find-vterm-buffer)))
    (if vterm-buffer
        (let ((str (concat (lx/get-babel-src) "\n")))
          (with-current-buffer vterm-buffer
            (vterm-send-string str t)
            (vterm-send-return))
          (select-window (get-buffer-window vterm-buffer))))))

(defun lx/find-vterm-buffer ()
  (let* ((window-buffers (mapcar #'window-buffer (window-list))))
    (or
      (--find (with-current-buffer it (eq major-mode 'pry-vterm-mode)) window-buffers)
      (--find (with-current-buffer it (derived-mode-p 'vterm-mode)) window-buffers))))

(defvar send-to-vterm-mode-keymap (make-sparse-keymap))

(define-minor-mode send-to-vterm-mode
  "Send to vterm mode"
  :lighter " Send to vterm"
  :keymap send-to-vterm-mode-keymap
  :group 'send-to-vterm-mode

  (spacemacs/set-leader-keys-for-minor-mode 'send-to-vterm-mode
    "Sl" 'lx/vterm-send-line
    "SL" 'lx/vterm-send-line-and-go
    "Sr" 'lx/vterm-send-region
    "SR" 'lx/vterm-send-region-and-go
    "Sp" 'lx/vterm-send-paragraph
    "SP" 'lx/vterm-send-paragraph-and-go
    "Sb" 'lx/vterm-send-babel-block
    "SB" 'lx/vterm-send-babel-block-and-go))

