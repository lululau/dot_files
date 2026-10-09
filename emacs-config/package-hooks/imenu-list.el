;; -*- lexical-binding: t; -*-

(spacemacs|use-package-add-hook imenu-list
  :post-config
  ;; Make imenu-list mode-line height match spaceline by prepending a transparent
  ;; XPM spacer image whose height equals `powerline-height'.
  ;; NOTE: Must prepend (not append) because the *Ilist* window is narrow;
  ;; when mode-line content fills the window width, a trailing spacer gets
  ;; truncated and no longer affects the mode-line height.
  (add-hook 'imenu-list-major-mode-hook
            (lambda ()
              (when (and (boundp 'powerline-height)
                         powerline-height
                         mode-line-format)
                (let* ((h powerline-height)
                       (pixel-rows (mapconcat (lambda (_) " ")
                                              (make-list h nil) "\",\""))
                       (xpm-data (format "/* XPM */ static char * s[] = {\"1 %d 1 1\",\"  c None\",\"%s\"};"
                                         h pixel-rows))
                       (spacer (propertize
                                " " 'display
                                (list 'image :type 'xpm :data xpm-data :ascent 'center))))
                  (setq-local mode-line-format
                              (cons spacer mode-line-format)))))
            :append))
