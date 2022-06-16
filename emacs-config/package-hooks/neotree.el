(spacemacs|use-package-add-hook neotree
  :post-config
  (define-key neotree-mode-map (kbd "s-t") #'(lambda () (interactive) (select-window-by-number 1) (split-window-right-and-focus)))
  (define-key neotree-mode-map (kbd "s-T") #'(lambda () (interactive) (select-window-by-number 1) (split-window-below-and-focus)))
  (define-key neotree-mode-map (kbd "s-n") #'(lambda () (interactive) (select-window-by-number 1) (switch-to-buffer-other-window (generate-new-buffer "*Untitled*")) (undo-tree-mode)))
  (define-key neotree-mode-map (kbd "s-N") #'(lambda () (interactive) (select-window-by-number 1) (switch-to-buffer (generate-new-buffer "*Untitled*")) (undo-tree-mode)))
  (define-key neotree-mode-map [tab] #'(lambda () (interactive) (neo-buffer--toggle-expand (neo-buffer--get-filename-current-line)) (neo-buffer--refresh t))))
