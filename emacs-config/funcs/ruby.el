(defun binding-pry-filter (text)
  (if (string-match "^ => [0-9]+:" text) (pop-to-buffer (current-buffer))))

(defun enh-ruby-toggle-block ()
  (interactive)
  (let ((start (point)) beg end)
    (end-of-line)
    (unless
        (if (and (re-search-backward "\\(?:[^#]\\)\\({\\)\\|\\(\\_<do\\_>\\)")
                 (progn
                   (goto-char (or (match-beginning 1) (match-beginning 2)))
                   (setq beg (point))
                   (save-match-data (enh-ruby-forward-sexp))
                   (setq end (point))
                   (> end start)))
            (if (match-beginning 1)
                (ruby-brace-to-do-end beg end)
              (ruby-do-end-to-brace beg end)))
      (goto-char start))))

(defun current-line-has-pry-breakpoint-p ()
  (string-match-p "binding\\.pry" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun delete-pry-breakpoints ()
  (save-excursion
    (goto-char (point-min))
    (while (/= (point) (point-max))
      (if (current-line-has-pry-breakpoint-p) (kill-whole-line) (forward-line)))))

(defun toggle-pry-breakpoint ()
  (interactive)
  (let ((buf-changed (buffer-modified-p)) (saved-evil-state evil-state))
    (if (current-line-has-pry-breakpoint-p)
        (kill-whole-line)
      (evil-open-above 0)
      (insert "require 'pry-byebug'; binding.pry;"))

    (unless buf-changed (save-buffer))
    (call-interactively (intern (concat "evil-" (symbol-name saved-evil-state) "-state")))))

(defun cleanup-pry-breakpoints ()
  (interactive)
  (let ((buf-changed (buffer-modified-p)) (saved-evil-state evil-state))
    (delete-pry-breakpoints)
    (unless buf-changed (save-buffer))
    (call-interactively (intern (concat "evil-" (symbol-name saved-evil-state) "-state")))))

(defun lx/jump-to-code-at-point ()
  (interactive)
  (let((str (ffap-string-at-point)) path line)
    (if (string-match "^\\(.+\\):\\([0-9]+\\).*$" str)
        (progn (setq path (match-string 1 str))
               (setq line (string-to-number (match-string 2 str))))
      (setq path str)
      (setq line 0))
    (org-open-file path t line)))


(defun lx/ruby-send-line ()
  (interactive)
  (if (lx/find-inf-buffer)
      (progn
        (ruby-send-region (line-beginning-position) (line-end-position))
        (comint-send-string (inf-ruby-proc) "\n"))
    (let ((pry-buffer (lx/find-pry-vterm-buffer)))
      (if pry-buffer
          (let ((str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
            (with-current-buffer pry-buffer
              (vterm-send-string str nil)))))))

(defun lx/ruby-send-line-and-go ()
  (interactive)
  (if (lx/find-inf-buffer)
      (progn
        (ruby-send-region (line-beginning-position) (line-end-position))
        (comint-send-string (inf-ruby-proc) "\n")
        (ruby-switch-to-inf t))
    (let ((pry-buffer (lx/find-pry-vterm-buffer)))
      (if pry-buffer
          (let ((str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
            (with-current-buffer pry-buffer
              (vterm-send-string str nil))
            (select-window (get-buffer-window pry-buffer)))))))

(defun lx/ruby-send-reload ()
  (interactive)
  (if (lx/find-inf-buffer)
      (progn
        (comint-send-string (inf-ruby-proc) "reload!\n")
        (ruby-switch-to-inf t))
    (let ((pry-buffer (lx/find-pry-vterm-buffer)))
      (if pry-buffer
          (let ((str "reload!\n"))
            (with-current-buffer pry-buffer
              (vterm-send-string str nil))
            (select-window (get-buffer-window pry-buffer)))))))


(defun lx/ruby-send-paragraph ()
  (interactive)
  (if (lx/find-inf-buffer)
      (progn
        (ruby-send-region (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))))
    (let ((pry-buffer (lx/find-pry-vterm-buffer)))
      (if pry-buffer
          (let ((str (concat (buffer-substring (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))) "")))
            (with-current-buffer pry-buffer
              (vterm-send-string str t)
              (vterm-send-return)))))))


(defun lx/ruby-send-paragraph-and-go ()
  (interactive)
  (if (lx/find-inf-buffer)
      (progn
        (ruby-send-region (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point)))
        (ruby-switch-to-inf t))
    (let ((pry-buffer (lx/find-pry-vterm-buffer)))
      (if pry-buffer
          (let ((str (concat (buffer-substring (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))) "")))
            (with-current-buffer pry-buffer
              (vterm-send-string str t)
              (vterm-send-return))
            (select-window (get-buffer-window pry-buffer)))))))


(defun lx/ruby-send-region ()
  (interactive)
  (if (lx/find-inf-buffer)
      (progn
        (ruby-send-region (region-beginning) (region-end)))
    (let ((pry-buffer (lx/find-pry-vterm-buffer)))
      (if pry-buffer
          (let ((str (concat (buffer-substring (region-beginning) (region-end)) "")))
            (with-current-buffer pry-buffer
              (vterm-send-string str t)
              (vterm-send-return)))))))


(defun lx/ruby-send-region-and-go ()
  (interactive)
  (if (lx/find-inf-buffer)
      (progn
        (ruby-send-region (region-beginning) (region-end))
        (ruby-switch-to-inf t))
    (let ((pry-buffer (lx/find-pry-vterm-buffer)))
      (if pry-buffer
          (let ((str (concat (buffer-substring (region-beginning) (region-end)) "")))
            (with-current-buffer pry-buffer
              (vterm-send-string str t)
              (vterm-send-return))
            (select-window (get-buffer-window pry-buffer)))))))


(defun lx/ruby-send-babel-block ()
  (interactive)
  (if (lx/find-inf-buffer)
      (progn
        (comint-send-string (inf-ruby-proc) (concat (lx/get-babel-src) "\n")))
    (let ((pry-buffer (lx/find-pry-vterm-buffer)))
      (if pry-buffer
          (let ((str (concat (lx/get-babel-src) "\n")))
            (with-current-buffer pry-buffer
              (vterm-send-string str t)
              (vterm-send-return)))))))


(defun lx/ruby-send-babel-block-and-go ()
  (interactive)
  (if (lx/find-inf-buffer)
      (progn
        (comint-send-string (inf-ruby-proc) (concat (lx/get-babel-src) "\n"))
        (ruby-switch-to-inf t))
    (let ((pry-buffer (lx/find-pry-vterm-buffer)))
      (if pry-buffer
          (let ((str (concat (lx/get-babel-src) "\n")))
            (with-current-buffer pry-buffer
              (vterm-send-string str t)
              (vterm-send-return))
            (select-window (get-buffer-window pry-buffer)))))))

(defun lx/find-inf-buffer ()
  (let* ((window-buffers (mapcar #'window-buffer (window-list))))
    (--find (with-current-buffer it (eq major-mode 'inf-ruby-mode)) window-buffers)))


(defun lx/find-pry-vterm-buffer ()
  (let* ((window-buffers (mapcar #'window-buffer (window-list))))
    (--find (with-current-buffer it (eq major-mode 'pry-vterm-mode)) window-buffers)))
