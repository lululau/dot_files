;; -*- lexical-binding: t; -*-

(with-eval-after-load 'ghostel
  (require 'shell-pop)

  (defcustom ghostel-kill-buffer-on-normal-exit t
    "Kill buffer on normal exit (finished status)."
    :type 'boolean
    :group 'ghostel)

  (add-hook 'ghostel-exit-functions
    (lambda (buf event)
      (when (and ghostel-kill-buffer-on-normal-exit (buffer-live-p buf)
                 (string= "finished\n" event))
        (kill-buffer buf))))

  (defun ghostel-enter-hybrid-state-decently ()
    (interactive)
    (evil-hybrid-state)
    (ghostel-send-key "space")
    (ghostel-send-key "backspace"))


  (define-key ghostel-mode-map
    (kbd (if (display-graphic-p) "<S-return>" "S-RET")) #'(lambda ()
                                                            (interactive)
                                                            (let ((shell-pop-internal-mode "zsh-ghostel"))
                                                              (shell-pop--cd-to-cwd
                                                               (with-current-buffer (get-buffer zsh-ghostel-last-buffer)
                                                                 (let ((remote-host (lx/get-remote-buffer-host)))
                                                                   (if remote-host
                                                                       (replace-regexp-in-string (message "^/\\(scp\\|ssh\\):%s:" remote-host) "" default-directory)
                                                                     (or (projectile-project-root) default-directory))))))))

  (define-key ghostel-mode-map
    (kbd (if (display-graphic-p) "<s-return>" "s-RET")) #'(lambda ()
                                                            (interactive)
                                                            (let* ((shell-pop-internal-mode "zsh-ghostel")
                                                                   (buffer (get-buffer zsh-ghostel-last-buffer))
                                                                   (buffer-file-name (buffer-file-name buffer)))
                                                              (shell-pop--cd-to-cwd
                                                               (with-current-buffer buffer
                                                                 (let ((remote-host (lx/get-remote-buffer-host)))
                                                                   (if remote-host
                                                                       (replace-regexp-in-string (message "^/\\(scp\\|ssh\\):%s:" remote-host) "" default-directory)
                                                                     (if buffer-file-name
                                                                         (setq buffer-file-directory (file-name-directory buffer-file-name))
                                                                       (if (eq 'dired-mode (with-current-buffer buffer major-mode))
                                                                           (setq buffer-file-directory (with-current-buffer buffer dired-directory))
                                                                         (setq buffer-file-directory (with-current-buffer buffer (projectile-project-root)))))
                                                                    buffer-file-directory)))))))

  (define-key ghostel-mode-map (kbd "<s-left>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "frame\n")))
  (define-key ghostel-mode-map (kbd "<s-up>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "up\n")))
  (define-key ghostel-mode-map (kbd "<s-down>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "down\n")))
  (define-key ghostel-mode-map (kbd "<f6>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "s\n")))
  (define-key ghostel-mode-map (kbd "<f7>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "f\n")))
  (define-key ghostel-mode-map (kbd "<f8>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "c\n")))
  (define-key ghostel-mode-map (kbd "<f9>") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "n\n")))
  (define-key ghostel-mode-map (kbd "M-DEL") #'term-send-raw-meta)
  (define-key ghostel-mode-map (kbd "C-z") #'ghostel-send-C-z)
  (define-key ghostel-mode-map (kbd "M-p") #'(lambda () (interactive) (ghostel-send-key "p" '(meta))))
  (define-key ghostel-mode-map (kbd "s-r r") #'lx/run-in-ghostel/rerun)
  (evil-define-key 'hybrid ghostel-mode-map (kbd "C-z") #'ghostel-send-C-z)
  (evil-define-key 'hybrid ghostel-mode-map (kbd "<escape>") #'(lambda () (interactive) (ghostel-send-key "escape")))
  (evil-define-key 'motion ghostel-mode-map (kbd "s-q") #'ghostel-enter-hybrid-state-decently)
  (evil-define-key 'hybrid ghostel-mode-map (kbd "s-k") #'(lambda () (interactive) (dotimes (i 10) (ghostel-send-string "k"))))
  (evil-define-key 'hybrid ghostel-mode-map (kbd "s-j") #'(lambda () (interactive) (dotimes (i 10) (ghostel-send-string "j"))))
  (define-key ghostel-mode-map (kbd "s-<backspace>") #'(lambda () (interactive) (ghostel-send-key "u" '(control))))
  (define-key ghostel-mode-map (kbd "M-D") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "exit-program\n")))
  (define-key ghostel-mode-map (kbd "s-w") #'delete-window-or-bury-buffer)
  (define-key ghostel-mode-map (kbd "<f12>") nil)
  (define-key ghostel-mode-map (kbd "C-c C-c") #'ghostel--self-insert)
  (define-key ghostel-mode-map (kbd "C-x C-c") #'(lambda () (interactive) (ghostel-send-key "x" '(control)) (ghostel-send-key "c" '(control))))
  (define-key ghostel-mode-map (kbd "C-x C-g") #'(lambda () (interactive) (ghostel-send-key "x" '(control)) (ghostel-send-key "g" '(control))))
  (define-key ghostel-mode-map (kbd "C-x C-e") #'(lambda () (interactive) (ghostel-send-key "x" '(control)) (ghostel-send-key "e" '(control))))
  (define-key ghostel-mode-map (kbd "C-c e") #'(lambda () (interactive) (ghostel-send-key "x" '(control)) (ghostel-send-key "e" '(control))))
  (define-key ghostel-mode-map (kbd "C-x C-k") #'(lambda () (interactive) (ghostel-send-key "x" '(control)) (ghostel-send-key "k" '(control))))
  (define-key ghostel-mode-map (kbd "C-x C-s") #'(lambda () (interactive) (ghostel-send-key "x" '(control)) (ghostel-send-key "s" '(control))))
  (define-key ghostel-mode-map (kbd "C-x C-f") #'(lambda () (interactive) (ghostel-send-key "x" '(control)) (ghostel-send-key "f" '(control))))
  (define-key ghostel-mode-map (kbd "C-x C-b") #'(lambda () (interactive) (ghostel-send-key "x" '(control)) (ghostel-send-key "b" '(control))))
  (define-key ghostel-mode-map (kbd "C-x b") #'(lambda () (interactive) (ghostel-send-key "x" '(control)) (ghostel-send-key "b")))
  (define-key ghostel-mode-map (kbd "C-x k") #'(lambda () (interactive) (ghostel-send-key "x" '(control)) (ghostel-send-key "k")))
  (define-key ghostel-mode-map (kbd "C-x s") #'(lambda () (interactive) (ghostel-send-key "x" '(control)) (ghostel-send-key "s")))
  (define-key ghostel-mode-map (kbd "M-:") #'eval-expression)
  (define-key ghostel-mode-map (kbd "<M-return>") #'(lambda () (interactive) (process-send-string ghostel--process "\e\C-m")))
  (define-key ghostel-mode-map (kbd "C-h") #'(lambda () (interactive) (ghostel-send-key "h" '(control))))
  (define-key ghostel-mode-map (kbd "M-/") 'current-buffer-completion)

  (let ((map (lookup-key ghostel-mode-map "\e")))
    (define-key map "k" #'lx/window-up-fallback-to-switch-frame)
    (define-key map "H" #'evil-window-move-far-left)
    (define-key map "J" #'evil-window-move-very-bottom)
    (define-key map "K" #'evil-window-move-very-top)
    (define-key map "L" #'evil-window-move-far-right))

  (defun ghostel-dnd-copy-path (uri)
    (let* ((uri (url-unhex-string uri))
           (uri (string-as-multibyte uri))
           (parsed (url-generic-parse-url uri))
           (path (car (url-path-and-query parsed)))
           (path (concat "'" path "'")))
      (ghostel-send-string path)))

  (defun ghostel-dnd-fallback (uri action)
    (let ((dnd-protocol-alist
           (rassq-delete-all
            'ghostel-dnd
            (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri)))

  (defun ghostel-dnd (uri action)
    (cond ((derived-mode-p 'ghostel-mode)
           (condition-case nil
               (ghostel-dnd-copy-path uri)
             (error
              (ghostel-dnd-fallback uri action))))
          ;; redirect to someone else
          (t
           (ghostel-dnd-fallback uri action))))

  (defun ghostel-dnd-enable ()
    (unless (eq (cdr (assoc "^file:///" dnd-protocol-alist))
                'ghostel-dnd)
      (setq dnd-protocol-alist
            `(("^file:///" . ghostel-dnd)
              ,@dnd-protocol-alist))))

  (defun ghostel-dnd-disable ()
    "Disable ghostel-dnd."
    (rassq-delete-all 'ghostel-dnd dnd-protocol-alist))

  (ghostel-dnd-enable)

  (rvm-activate-corresponding-ruby))

(spacemacs|use-package-add-hook ghostel
  :post-config
  (define-key ghostel-mode-map (kbd "M-p") #'(lambda () (interactive) (ghostel-send-key "p" '(meta))))
  (define-key ghostel-mode-map (kbd "M-/") 'current-buffer-completion))
