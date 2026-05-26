;; -*- lexical-binding: t; -*-
;;;###autoload
(defun lx/set-monospaced-font (english chinese english-retina-size chinese-retina-size english-normal-size chinese-normal-size)
  (setq lx/set-monospaced-font/current-font-size (list english-retina-size chinese-retina-size))
  (when window-system
    (dolist (monitor-attrs (display-monitor-attributes-list))
      ;; (let* ((scale-factor (cdr (assoc 'backing-scale-factor monitor-attrs)))
      (let* ((scale-factor 2)
             (english-size (if (> scale-factor 1) english-retina-size english-normal-size))
             (chinese-size (if (> scale-factor 1) chinese-retina-size chinese-normal-size))
             (frames (cdr (assoc 'frames monitor-attrs))))
        (dolist (frame frames)
          (set-face-attribute 'default frame :font
                              (format   "%s:pixelsize=%d"  english english-size))
          (dolist (charset '(kana han symbol cjk-misc bopomofo))
            (set-fontset-font (frame-parameter frame 'font) charset
                              (font-spec :family chinese :size chinese-size) frame)))))))

;;;###autoload
(defun lx/set-monospaced-font/increase-font-size ()
  (interactive)
  (when (not (equal lx/set-monospaced-font/current-font-size lx/set-monospaced-font/max-font-size))
    (let* ((curr lx/set-monospaced-font/current-font-size)
           (idx (-elem-index lx/set-monospaced-font/current-font-size lx/set-monospaced-font/font-size-options))
           (next-idx (1+ idx))
           (next (nth next-idx lx/set-monospaced-font/font-size-options)))
      (apply 'lx/set-monospaced-font (append lx/set-monospaced-font/font-names next next)))))


;;;###autoload
(defun lx/set-monospaced-font/decrease-font-size ()
  (interactive)
  (when (not (equal lx/set-monospaced-font/current-font-size lx/set-monospaced-font/min-font-size))
    (let* ((curr lx/set-monospaced-font/current-font-size)
           (idx (-elem-index lx/set-monospaced-font/current-font-size lx/set-monospaced-font/font-size-options))
           (next-idx (1- idx))
           (next (nth next-idx lx/set-monospaced-font/font-size-options)))
      (apply 'lx/set-monospaced-font (append lx/set-monospaced-font/font-names next next)))))
