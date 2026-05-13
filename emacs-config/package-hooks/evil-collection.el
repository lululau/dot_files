;; -*- lexical-binding: t; -*-

(defun lx/override-dired-subtree-kbd (&rest args)
  (with-eval-after-load 'dired-subtree
    (evil-collection-define-key 'normal 'dired-mode-map
      (kbd "TAB") 'dired-subtree-toggle
      (kbd "<backtab>") 'dired-subtree-cycle
      (kbd "M-j") 'lx/window-down-fallback-to-switch-frame
      (kbd "M-k") 'lx/window-up-fallback-to-switch-frame)))

(with-eval-after-load 'evil-collection-dired
  (evil-collection-define-key 'motion 'dired-mode-map
    (kbd "TAB") 'dired-subtree-toggle
    (kbd "<backtab>") 'dired-subtree-cycle)

  (advice-add 'evil-collection-dired-setup :after #'lx/override-dired-subtree-kbd))


(with-eval-after-load 'evil-collection-magit
  (evil-define-key 'visual magit-hunk-section-map (kbd "s") 'magit-stage)
  (evil-define-key 'normal magit-mode-map (kbd "S-SPC") nil)
  (evil-define-key 'visual magit-mode-map (kbd "S-SPC") nil)
  (evil-define-key 'motion magit-mode-map (kbd "S-SPC") nil)
  (evil-define-key 'emacs magit-mode-map (kbd "S-SPC") nil)
  (evil-define-key 'hybrid magit-mode-map (kbd "S-SPC") nil))

;; evil-collection-vterm block removed — ghostel uses evil-ghostel.el instead
