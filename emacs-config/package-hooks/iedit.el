(with-eval-after-load 'iedit
  (add-hook 'iedit-mode-hook #'(lambda ()
                                 (setq-local ggtags-mode-local-var ggtags-mode)
                                 (if ggtags-mode-local-var (ggtags-mode -1))))
  (add-hook 'iedit-mode-end-hook #'(lambda ()
                                     (if ggtags-mode-local-var (ggtags-mode)))))
