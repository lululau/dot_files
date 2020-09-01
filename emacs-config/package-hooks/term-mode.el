(with-eval-after-load 'vterm
  (define-key vterm-mode-map
    (kbd (if (display-graphic-p) "<S-return>" "S-RET")) #'(lambda ()
                           (interactive)
                           (shell-pop--cd-to-cwd
                            (with-current-buffer (get-buffer shell-pop-last-buffer) (projectile-project-root)))))

  (define-key vterm-mode-map
    (kbd (if (display-graphic-p) "<s-return>" "s-RET")) #'(lambda ()
                                                            (interactive)
                                                            (let* ((buffer (get-buffer shell-pop-last-buffer))
                                                                   (buffer-file-name (buffer-file-name buffer)))
                                                              (if buffer-file-name
                                                                  (setq buffer-file-directory (file-name-directory buffer-file-name))
                                                                (if (eq 'dired-mode (with-current-buffer buffer major-mode))
                                                                    (setq buffer-file-directory (with-current-buffer buffer dired-directory))
                                                                  (setq buffer-file-directory (with-current-buffer buffer (projectile-project-root)))
                                                                  ))
                                                              (shell-pop--cd-to-cwd buffer-file-directory))))

  (define-key vterm-mode-map (kbd "<s-left>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "frame\n")))
  (define-key vterm-mode-map (kbd "<s-up>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "up\n")))
  (define-key vterm-mode-map (kbd "<s-down>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "down\n")))
  (define-key vterm-mode-map (kbd "<f6>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "s\n")))
  (define-key vterm-mode-map (kbd "<f7>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "f\n")))
  (define-key vterm-mode-map (kbd "<f8>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "c\n")))
  (define-key vterm-mode-map (kbd "<f9>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "n\n")))
  (define-key vterm-mode-map (kbd "M-DEL") #'term-send-raw-meta)
  (define-key vterm-mode-map (kbd "M-D") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "exit-program\n")))

  (let ((map (lookup-key vterm-mode-map "\e")))
    (define-key map "h" #'evil-window-left)
    (define-key map "l" #'evil-window-right)
    (define-key map "j" #'lx/window-down-fallback-to-switch-frame)
    (define-key map "k" #'lx/window-up-fallback-to-switch-frame)
    (define-key map "H" #'evil-window-move-far-left)
    (define-key map "J" #'evil-window-move-very-bottom)
    (define-key map "K" #'evil-window-move-very-top)
    (define-key map "L" #'evil-window-move-far-right)))
