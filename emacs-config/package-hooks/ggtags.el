(with-eval-after-load 'ggtags
  (define-key ggtags-mode-map (kbd "M-]") nil)
  (define-key ggtags-mode-map (kbd "M-[") nil))


(spacemacs|use-package-add-hook ggtags
  :post-config
  (define-key ggtags-mode-map (kbd "M-.") nil))
