;; -*- lexical-binding: t; -*-

;; Defer the whole ghostel-mode customization until BOTH:
;;   (a) ghostel itself is loaded, and
;;   (b) evil is loaded (we use `evil-define-key' etc.), and
;;   (c) shell-pop is loadable (used by ghostel-cd-to-{cwd,file-dir}).
;;
;; Reason: when ghostel is force-activated during a fresh install/update
;; (Spacemacs runs `package--load-files-for-activation' on the freshly
;; built package), this hook would otherwise fire synchronously -- before
;; evil's / shell-pop's elpa directories are even on `load-path' -- and
;; throw things like (void-function evil-define-key) or
;; (file-missing ... shell-pop). We instead apply the configuration once,
;; from whichever of these fires first:
;;   * `with-eval-after-load' chain ghostel -> evil, or
;;   * `emacs-startup-hook' (after Spacemacs user-config finishes).

(defvar lx/ghostel-mode--applied nil
  "Non-nil once the ghostel-mode customizations below have been applied.")

(defun lx/ghostel-mode--apply ()
  "Apply ghostel-mode customizations when all deps are ready."
  (when (and (not lx/ghostel-mode--applied)
             (featurep 'ghostel)
             (featurep 'evil))
    (setq lx/ghostel-mode--applied t)
    (require 'shell-pop nil 'noerror)
    (lx/ghostel-mode--configure)))

(with-eval-after-load 'ghostel
  (with-eval-after-load 'evil
    (lx/ghostel-mode--apply)))

(add-hook 'emacs-startup-hook #'lx/ghostel-mode--apply)

(defun lx/ghostel-mode--configure ()
  "The actual ghostel-mode configuration body (deferred)."

  (defun ghostel-send-escape-key () (interactive) (ghostel-send-key "escape"))
  (defun ghostel-send-10-up () (interactive) (dotimes (i 10) (ghostel-send-string "k")))
  (defun ghostel-send-10-down () (interactive) (dotimes (i 10) (ghostel-send-string "j")))
  (defun ghostel-send-ctrl-u () (interactive) (ghostel-send-key "u" "ctrl"))
  (defun ghostel-send-exit-program () (interactive) (ghostel-send-string "exit-program\n"))
  (defun ghostel-send-meta-return () (interactive) (ghostel-send-string "\e\C-m"))
  (defun ghostel-send-meta-p () (interactive) (ghostel-send-key "p" "meta"))
  (defun ghostel-send-meta-period () (interactive) (ghostel-send-string "\e."))
  (defun ghostel-send-ctrl-h () (interactive) (ghostel-send-key "h" "ctrl"))
  (defun ghostel-send-C-x-C-c () (interactive) (ghostel-send-ctrl-x "c" t))
  (defun ghostel-send-C-x-C-g () (interactive) (ghostel-send-ctrl-x "g" t))
  (defun ghostel-send-C-x-C-e () (interactive) (ghostel-send-ctrl-x "e" t))
  (defun ghostel-send-C-x-C-k () (interactive) (ghostel-send-ctrl-x "k" t))
  (defun ghostel-send-C-x-C-s () (interactive) (ghostel-send-ctrl-x "s" t))
  (defun ghostel-send-C-x-C-f () (interactive) (ghostel-send-ctrl-x "f" t))
  (defun ghostel-send-C-x-C-b () (interactive) (ghostel-send-ctrl-x "b" t))
  (defun ghostel-send-C-x-b () (interactive) (ghostel-send-ctrl-x "b"))
  (defun ghostel-send-C-x-k () (interactive) (ghostel-send-ctrl-x "k"))
  (defun ghostel-send-C-x-s () (interactive) (ghostel-send-ctrl-x "s"))

  (defun ghostel-send-frame () (interactive) (ghostel-send-cmd "frame\n"))
  (defun ghostel-send-up () (interactive) (ghostel-send-cmd "up\n"))
  (defun ghostel-send-down () (interactive) (ghostel-send-cmd "down\n"))
  (defun ghostel-send-step () (interactive) (ghostel-send-cmd "s\n"))
  (defun ghostel-send-finish () (interactive) (ghostel-send-cmd "f\n"))
  (defun ghostel-send-continue () (interactive) (ghostel-send-cmd "c\n"))
  (defun ghostel-send-next () (interactive) (ghostel-send-cmd "n\n"))

  (defun ghostel-enter-hybrid-state-decently ()
    (interactive)
    (evil-hybrid-state)
    (ghostel-send-string " ")
    (ghostel-send-key "backspace"))

  (defun ghostel-cd-to-cwd ()
    "cd to current working directory"
    (interactive)
    (let ((shell-pop-internal-mode "zsh-ghostel"))
      (shell-pop--cd-to-cwd
       (with-current-buffer (get-buffer zsh-ghostel-last-buffer)
         (let ((remote-host (lx/get-remote-buffer-host)))
           (if remote-host
               (replace-regexp-in-string (message "^/\\(scp\\|ssh\\):%s:" remote-host) "" default-directory)
             (or (projectile-project-root) default-directory)))))))

  (defun ghostel-cd-to-file-dir ()
    "cd to file directory of last buffer"
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
             buffer-file-directory))))))

  (defun ghostel-send-cmd (str)
    "Send string command to ghostel terminal"
    (interactive) (ghostel-send-string str))

  (defun ghostel-send-ctrl-x (key &optional ctrl)
    "Send C-x followed by KEY to ghostel. If CTRL is non-nil, send C-KEY."
    (ghostel-send-key "x" "ctrl")
    (ghostel-send-key key (when ctrl "ctrl")))


  (defcustom ghostel-kill-buffer-on-normal-exit t
    "Kill buffer on normal exit (finished status)."
    :type 'boolean
    :group 'ghostel)

  ;; Replace the built-in sentinel with two-level kill logic (mirrors the
  ;; vterm--sentinel pattern).  When `ghostel-kill-buffer-on-normal-exit' is
  ;; t, only kill on normal ("finished") exits so that abnormal exits are
  ;; kept for inspection.  When nil, fall through to
  ;; `ghostel-kill-buffer-on-exit'.  A force-full-redraw is added before
  ;; timer cancellation so the terminal state is rendered to the Emacs
  ;; buffer when kept alive (the original sentinel only flushes without
  ;; rendering, so the last pending chunk would be invisible).
  (defun ghostel--sentinel (process event)
    "Process sentinel: clean up when shell exits.
PROCESS is the shell process, EVENT describes the state change."
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when ghostel--term
            (ghostel--flush-pending-output))
          (setq-local ghostel-full-redraw t)
          (setq ghostel--force-next-redraw t)
          (ghostel--delayed-redraw (current-buffer))
          (when ghostel--redraw-timer
            (cancel-timer ghostel--redraw-timer)
            (setq ghostel--redraw-timer nil))
          (when ghostel--input-timer
            (cancel-timer ghostel--input-timer)
            (setq ghostel--input-timer nil))
          (when ghostel--plain-link-detection-timer
            (cancel-timer ghostel--plain-link-detection-timer)
            (setq ghostel--plain-link-detection-begin nil
                  ghostel--plain-link-detection-end nil))
          (ghostel--cancel-password-confirm-timer)
          (ghostel--spinner-stop)
          (remove-hook 'pre-redisplay-functions #'ghostel--fake-cursor-update t)
          (ghostel--fake-cursor-clear)
          (run-hook-with-args 'ghostel-exit-functions buf event)
          (when (buffer-live-p buf)
            (if (and ghostel-kill-buffer-on-normal-exit
                     (string= "finished\n" event))
                (kill-buffer buf)
              (if ghostel-kill-buffer-on-exit
                  (kill-buffer buf)
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert "\n[Process exited]\n")))))))))

  (defun ghostel-enter-insert-state-decently ()
    (interactive)
    (evil-insert-state)
    (ghostel-send-key "space")
    (ghostel-send-key "backspace"))

  (define-key ghostel-mode-map (kbd (if (display-graphic-p) "<S-return>" "S-RET")) #'ghostel-cd-to-cwd)
  (define-key ghostel-mode-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) #'ghostel-cd-to-file-dir)
  (define-key ghostel-mode-map (kbd "<s-left>") #'ghostel-send-frame)
  (define-key ghostel-mode-map (kbd "<s-up>") #'ghostel-send-up)
  (define-key ghostel-mode-map (kbd "<s-down>") #'ghostel-send-down)
  (define-key ghostel-mode-map (kbd "<f6>") #'ghostel-send-step)
  (define-key ghostel-mode-map (kbd "<f7>") #'ghostel-send-finish)
  (define-key ghostel-mode-map (kbd "<f8>") #'ghostel-send-continue)
  (define-key ghostel-mode-map (kbd "<f9>") #'ghostel-send-next)
  (define-key ghostel-mode-map (kbd "C-z") #'ghostel-send-C-z)
  (define-key ghostel-mode-map (kbd "M-p") #'ghostel-send-meta-p)
  (define-key ghostel-mode-map (kbd "s-r r") #'lx/run-in-ghostel/rerun)
  (evil-define-key 'motion ghostel-mode-map (kbd "s-q") #'ghostel-enter-insert-state-decently)
  (define-key ghostel-mode-map (kbd "s-<backspace>") #'ghostel-send-ctrl-u)
  (define-key ghostel-mode-map (kbd "M-D") #'ghostel-send-exit-program)
  (define-key ghostel-mode-map (kbd "s-w") #'delete-window-or-bury-buffer)
  (define-key ghostel-mode-map (kbd "<f12>") nil)
  (define-key ghostel-mode-map (kbd "C-c C-c") #'ghostel--self-insert)
  (define-key ghostel-mode-map (kbd "C-x C-c") #'ghostel-send-C-x-C-c)
  (define-key ghostel-mode-map (kbd "C-x C-g") #'ghostel-send-C-x-C-g)
  (define-key ghostel-mode-map (kbd "C-x C-e") #'ghostel-send-C-x-C-e)
  (define-key ghostel-mode-map (kbd "C-c e") #'ghostel-send-C-x-C-e)
  (define-key ghostel-mode-map (kbd "C-x C-k") #'ghostel-send-C-x-C-k)
  (define-key ghostel-mode-map (kbd "C-x C-s") #'ghostel-send-C-x-C-s)
  (define-key ghostel-mode-map (kbd "C-x C-f") #'ghostel-send-C-x-C-f)
  (define-key ghostel-mode-map (kbd "C-x C-b") #'ghostel-send-C-x-C-b)
  (define-key ghostel-mode-map (kbd "C-x b") #'ghostel-send-C-x-b)
  (define-key ghostel-mode-map (kbd "C-x k") #'ghostel-send-C-x-k)
  (define-key ghostel-mode-map (kbd "C-x s") #'ghostel-send-C-x-s)
  (define-key ghostel-mode-map (kbd "M-:") #'eval-expression)
  (define-key ghostel-mode-map (kbd "<M-return>") #'ghostel-send-meta-return)
  (define-key ghostel-mode-map (kbd "C-h") #'ghostel-send-ctrl-h)
  (define-key ghostel-mode-map (kbd "M-/") 'current-buffer-completion)
  (define-key ghostel-mode-map (kbd "M-.") #'ghostel-send-meta-period)
  (define-key ghostel-mode-map (kbd "<escape>") #'ghostel-send-escape-key)

  (let ((map (lookup-key ghostel-mode-map "\e")))
    (define-key map "k" #'lx/window-up-fallback-to-switch-frame)
    (define-key map "H" #'evil-window-move-far-left)
    (define-key map "J" #'evil-window-move-very-bottom)
    (define-key map "K" #'evil-window-move-very-top)
    (define-key map "L" #'evil-window-move-far-right))

  (define-key ghostel-semi-char-mode-map (kbd (if (display-graphic-p) "<S-return>" "S-RET")) #'ghostel-cd-to-cwd)
  (define-key ghostel-semi-char-mode-map (kbd (if (display-graphic-p) "<s-return>" "s-RET")) #'ghostel-cd-to-file-dir)
  (define-key ghostel-semi-char-mode-map (kbd "<s-left>") #'ghostel-send-frame)
  (define-key ghostel-semi-char-mode-map (kbd "<s-up>") #'ghostel-send-up)
  (define-key ghostel-semi-char-mode-map (kbd "<s-down>") #'ghostel-send-down)
  (define-key ghostel-semi-char-mode-map (kbd "<f6>") #'ghostel-send-step)
  (define-key ghostel-semi-char-mode-map (kbd "<f7>") #'ghostel-send-finish)
  (define-key ghostel-semi-char-mode-map (kbd "<f8>") #'ghostel-send-continue)
  (define-key ghostel-semi-char-mode-map (kbd "<f9>") #'ghostel-send-next)
  (define-key ghostel-semi-char-mode-map (kbd "C-z") #'ghostel-send-C-z)
  (define-key ghostel-semi-char-mode-map (kbd "s-<backspace>") #'ghostel-send-ctrl-u)
  (define-key ghostel-semi-char-mode-map (kbd "M-D") #'ghostel-send-exit-program)
  (define-key ghostel-semi-char-mode-map (kbd "s-w") #'delete-window-or-bury-buffer)
  (define-key ghostel-semi-char-mode-map (kbd "<f12>") nil)
  (define-key ghostel-semi-char-mode-map (kbd "C-x C-c") #'ghostel-send-C-x-C-c)
  (define-key ghostel-semi-char-mode-map (kbd "C-x C-g") #'ghostel-send-C-x-C-g)
  (define-key ghostel-semi-char-mode-map (kbd "C-x C-e") #'ghostel-send-C-x-C-e)
  (define-key ghostel-semi-char-mode-map (kbd "C-c e") #'ghostel-send-C-x-C-e)
  (define-key ghostel-semi-char-mode-map (kbd "C-x C-k") #'ghostel-send-C-x-C-k)
  (define-key ghostel-semi-char-mode-map (kbd "C-x C-s") #'ghostel-send-C-x-C-s)
  (define-key ghostel-semi-char-mode-map (kbd "C-x C-f") #'ghostel-send-C-x-C-f)
  (define-key ghostel-semi-char-mode-map (kbd "C-x C-b") #'ghostel-send-C-x-C-b)
  (define-key ghostel-semi-char-mode-map (kbd "C-x b") #'ghostel-send-C-x-b)
  (define-key ghostel-semi-char-mode-map (kbd "C-x k") #'ghostel-send-C-x-k)
  (define-key ghostel-semi-char-mode-map (kbd "C-x s") #'ghostel-send-C-x-s)
  (define-key ghostel-semi-char-mode-map (kbd "M-:") #'eval-expression)
  (define-key ghostel-semi-char-mode-map (kbd "<M-return>") #'ghostel-send-meta-return)
  (define-key ghostel-semi-char-mode-map (kbd "C-h") #'ghostel-send-ctrl-h)
  (define-key ghostel-semi-char-mode-map (kbd "M-/") 'current-buffer-completion)
  (define-key ghostel-semi-char-mode-map (kbd "M-.") #'ghostel-send-meta-period)
  (define-key ghostel-semi-char-mode-map (kbd "<escape>") #'ghostel-send-escape-key)

  (evil-define-key 'hybrid ghostel-semi-char-mode-map (kbd "C-z") #'ghostel-send-C-z)
  (evil-define-key 'hybrid ghostel-mode-map (kbd "<escape>") #'ghostel-send-escape-key)
  (evil-define-key 'hybrid ghostel-semi-char-mode-map (kbd "<escape>") #'ghostel-send-escape-key)
  (evil-define-key 'motion ghostel-semi-char-mode-map (kbd "s-q") #'ghostel-enter-hybrid-state-decently)
  (evil-define-key 'hybrid ghostel-semi-char-mode-map (kbd "s-k") #'ghostel-send-10-up)
  (evil-define-key 'hybrid ghostel-semi-char-mode-map (kbd "s-j") #'ghostel-send-10-down)


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

  ;; (rvm-activate-corresponding-ruby)

  )

(spacemacs|use-package-add-hook ghostel
  :post-config
  (define-key ghostel-mode-map (kbd "M-p") #'ghostel-send-meta-p)
  (define-key ghostel-mode-map (kbd "M-/") 'current-buffer-completion))
