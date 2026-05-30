;; -*- lexical-binding: t; -*-

(spacemacs|use-package-add-hook neotree
  :post-config
  (define-key neotree-mode-map (kbd "s-t") #'(lambda () (interactive) (select-window-by-number 1) (split-window-right-and-focus)))
  (define-key neotree-mode-map (kbd "s-T") #'(lambda () (interactive) (select-window-by-number 1) (split-window-below-and-focus)))
  (define-key neotree-mode-map (kbd "s-n") #'(lambda () (interactive) (select-window-by-number 1) (switch-to-buffer-other-window (generate-new-buffer "*Untitled*")) (undo-tree-mode)))
  (define-key neotree-mode-map (kbd "s-N") #'(lambda () (interactive) (select-window-by-number 1) (switch-to-buffer (generate-new-buffer "*Untitled*")) (undo-tree-mode)))
  (define-key neotree-mode-map [tab] #'(lambda () (interactive) (neo-buffer--toggle-expand (neo-buffer--get-filename-current-line)) (neo-buffer--refresh t)))

  ;; Make neotree mode-line height match spaceline by appending a transparent
  ;; XPM spacer image whose height equals `powerline-height'.
  (add-hook 'neotree-mode-hook
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
                              (append mode-line-format (list spacer))))))))
