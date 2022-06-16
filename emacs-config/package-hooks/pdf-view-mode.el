(with-eval-after-load 'pdf-view-mode
  (evil-define-key 'motion pdf-view-mode-map
    (kbd "S-m") 'pdf-annot-add-highlight-markup-annotation
    (kbd "s-m") 'pdf-annot-add-highlight-markup-annotation))
