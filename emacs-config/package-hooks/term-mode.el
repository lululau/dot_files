(with-eval-after-load 'term
  (define-key term-raw-map
    (kbd (if (display-graphic-p) "<S-return>" "S-RET")) #'(lambda ()
                           (interactive)
                           (shell-pop--cd-to-cwd
                            (with-current-buffer (get-buffer shell-pop-last-buffer) (projectile-project-root)))))

  (define-key term-raw-map
    (kbd (if (display-graphic-p) "<s-return>" "s-RET")) #'(lambda ()
                                                            (interactive)
                                                            (let* ((buffer (get-buffer shell-pop-last-buffer))
                                                                   (buffer-file-name (buffer-file-name buffer)))
                                                              (if (and (not buffer-file-name) (eq 'dired-mode (with-current-buffer buffer major-mode)))
                                                                  (setq buffer-file-directory (with-current-buffer buffer dired-directory))
                                                                (setq buffer-file-directory (file-name-directory buffer-file-name)))
                                                              (shell-pop--cd-to-cwd buffer-file-directory))))

  (define-key term-raw-map (kbd "<s-left>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "frame\n")))
  (define-key term-raw-map (kbd "<s-up>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "up\n")))
  (define-key term-raw-map (kbd "<s-down>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "down\n")))
  (define-key term-raw-map (kbd "<f6>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "s\n")))
  (define-key term-raw-map (kbd "<f7>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "f\n")))
  (define-key term-raw-map (kbd "<f8>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "c\n")))
  (define-key term-raw-map (kbd "<f9>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "n\n")))
  (define-key term-raw-map (kbd "M-DEL") #'term-send-raw-meta)
  (define-key term-raw-map (kbd "M-D") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "exit-program\n")))

  (let ((map (lookup-key term-raw-map "\e")))
    (define-key map "h" #'evil-window-left)
    (define-key map "l" #'evil-window-right)
    (define-key map "j" #'lx/window-down-fallback-to-switch-frame)
    (define-key map "k" #'lx/window-up-fallback-to-switch-frame)
    (define-key map "H" #'evil-window-move-far-left)
    (define-key map "J" #'evil-window-move-very-bottom)
    (define-key map "K" #'evil-window-move-very-top)
    (define-key map "L" #'evil-window-move-far-right)))
