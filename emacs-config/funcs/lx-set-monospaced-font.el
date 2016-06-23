;; Set the monospaced font size when mixed Chinese and English words, and if you use a computer with a retina display and a external display.
(defun lx/set-monospaced-font (english chinese english-retina-size chinese-retina-size english-normal-size chinese-normal-size)
  (when window-system
    (dolist (monitor-attrs (display-monitor-attributes-list))
      (let* ((scale-factor (cdr (assoc 'backing-scale-factor monitor-attrs)))
             (english-size (if (> scale-factor 1) english-retina-size english-normal-size))
             (chinese-size (if (> scale-factor 1) chinese-retina-size chinese-normal-size))
            (frames (cdr (assoc 'frames monitor-attrs))))
        (dolist (frame frames)
          (set-face-attribute 'default frame :font
                              (format   "%s:pixelsize=%d"  english english-size))
          (dolist (charset '(kana han symbol cjk-misc bopomofo))
            (set-fontset-font (frame-parameter frame 'font) charset
                              (font-spec :family chinese :size chinese-size) frame)))))))
