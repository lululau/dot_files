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
  ;; (define-key vterm-mode-map (kbd "C-S-l") #'vterm-send-C-l)
  ;; (define-key vterm-mode-map (kbd "C-l") #'recenter-top-bottom)
  (define-key vterm-mode-map (kbd "C-l") #'(lambda () (interactive) (let ((inhibit-read-only t)) (insert (s-repeat (count-screen-lines (window-start) (point)) "\n")) (vterm-send-C-l))))
  (define-key vterm-mode-map (kbd "C-z") #'vterm-send-C-z)
  (define-key vterm-mode-map (kbd "M-p") #'vterm-send-M-p)
  (define-key vterm-mode-map (kbd "s-r r") #'lx/run-in-vterm/rerun)
  (evil-define-key 'hybrid vterm-mode-map (kbd "C-z") #'vterm-send-C-z)
  (evil-define-key 'hybrid vterm-mode-map (kbd "<escape>") #'vterm-send-escape)
  (define-key vterm-mode-map (kbd "s-<backspace>") #'vterm-send-C-u)
  (define-key vterm-mode-map (kbd "M-D") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "exit-program\n")))
  (define-key vterm-mode-map (kbd "s-w") #'delete-window-or-bury-buffer)
  (define-key vterm-mode-map (kbd "<f12>") nil)

  (let ((map (lookup-key vterm-mode-map "\e")))
    ;; (define-key map "h" #'evil-window-left)
    ;; (define-key map "l" #'evil-window-right)
    ;; (define-key map "j" #'lx/window-down-fallback-to-switch-frame)
    (define-key map "k" #'lx/window-up-fallback-to-switch-frame)
    (define-key map "H" #'evil-window-move-far-left)
    (define-key map "J" #'evil-window-move-very-bottom)
    (define-key map "K" #'evil-window-move-very-top)
    (define-key map "L" #'evil-window-move-far-right))

  (defun vterm-dnd-copy-path (uri)
    (let* ((uri (url-unhex-string uri))
           (uri (string-as-multibyte uri))
           (parsed (url-generic-parse-url uri))
           (path (car (url-path-and-query parsed)))
           (path (concat "'" path "'")))
      (vterm-send-string path)))

  (defun vterm-dnd-fallback (uri action)
    (let ((dnd-protocol-alist
           (rassq-delete-all
            'vterm-dnd
            (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri)))

  (defun vterm-dnd (uri action)
    (cond ((eq 'vterm-mode major-mode)
           (condition-case nil
               (vterm-dnd-copy-path uri)
             (error
              (vterm-dnd-fallback uri action))))
          ;; redirect to someone else
          (t
           (vterm-dnd-fallback uri action))))

  (defun vterm-dnd-enable ()
    (unless (eq (cdr (assoc "^file:///" dnd-protocol-alist))
                'vterm-dnd)
      (setq dnd-protocol-alist
            `(("^file:///" . vterm-dnd)
              ,@dnd-protocol-alist))))

  (defun vterm-dnd-disable ()
    "Disable vterm-dnd."
    (rassq-delete-all 'vterm-dnd dnd-protocol-alist))

  (vterm-dnd-enable))

(spacemacs|use-package-add-hook vterm
  :post-config
  (define-key vterm-mode-map (kbd "M-p") #'vterm-send-M-p))
