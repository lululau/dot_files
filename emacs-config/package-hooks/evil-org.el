;; -*- lexical-binding: t; -*-

(spacemacs|use-package-add-hook evil-org
  :post-config
  (progn
    (mapc (lambda (state)
            (evil-define-key state evil-org-mode-map
              (kbd "M-l") nil
              (kbd "M-h") nil
              (kbd "M-k") nil
              (kbd "M-j") nil
              (kbd "M-L") nil
              (kbd "M-H") nil
              (kbd "M-K") nil
              (kbd "M-J") nil
              ))
          '(normal insert))
    (define-key org-mode-map (kbd "M-h") nil)
    (unless (display-graphic-p)
      (evil-define-key 'normal evil-org-mode-map
        (kbd "C-RET") (evil-org-define-eol-command
                       org-insert-heading-respect-content)
        (kbd "M-S-RET") (evil-org-define-eol-command
                       org-insert-todo-heading)))
    ))

(with-eval-after-load 'evil-org
  ;; evil-org 20261025 起只在 defcustom :set 里装键位，Spacemacs 预先 setq 了
  ;; evil-org-key-theme 导致 :set 不会触发；须在下方清除 M-hjkl 之前调用
  (evil-org-set-key-theme)
  (let-alist evil-org-movement-bindings
    (let ((state (if evil-org-use-additional-insert
                     '(normal visual insert)
                   '(normal visual))))
      (evil-define-key state 'evil-org-mode
        (kbd (concat "M-" .left)) nil
        (kbd (concat "M-" .right)) nil
        (kbd (concat "M-" .up)) nil
        (kbd (concat "M-" .down)) nil))))
