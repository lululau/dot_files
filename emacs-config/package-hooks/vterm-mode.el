(with-eval-after-load 'vterm
  (require 'shell-pop)
;;   (defvar-local vterm--undecoded-bytes nil)
;;   (defun vterm--filter (process input)
;;     "I/O Event.  Feeds PROCESS's INPUT to the virtual terminal.
;; Then triggers a redraw from the module."
;;     (let ((inhibit-redisplay t)
;;           (inhibit-read-only t)
;;           (buf (process-buffer process))
;;           (decoded-str))
;;       (when (buffer-live-p buf)
;;         (with-current-buffer buf
;;           ;; Borrowed from term.el
;;           ;;
;;           ;; Avoid garbling of certain multibyte characters by decoding the string
;;           ;; before counting characters.  See,
;;           ;; https://github.com/akermu/emacs-libvterm/issues/394, and the
;;           ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=1006 (for term.el).
;;           (when vterm--undecoded-bytes
;;             (setq input (concat vterm--undecoded-bytes input))
;;             (setq vterm--undecoded-bytes nil))
;;           (setq decoded-str
;;                 (decode-coding-string input locale-coding-system t))
;;           (let ((partial 0)
;;                 (count (length decoded-str)))
;;             (while (and (< partial count)
;;                         (eq (char-charset (aref decoded-str
;;                                                 (- count 1 partial)))
;;                             'eight-bit))
;;               (cl-incf partial))
;;             (when (> count partial 0)
;;               (setq vterm--undecoded-bytes
;;                     (substring decoded-str (- partial)))
;;               (setq decoded-str
;;                     (substring decoded-str 0 (- partial)))))
;;           (vterm--write-input vterm--term decoded-str)
;;           (vterm--update vterm--term)))))

  (define-key vterm-mode-map
    (kbd (if (display-graphic-p) "<S-return>" "S-RET")) #'(lambda ()
                                                            (interactive)
                                                            (let ((shell-pop-internal-mode "zsh-vterm"))
                                                              (shell-pop--cd-to-cwd
                                                               (with-current-buffer (get-buffer zsh-vterm-last-buffer) (projectile-project-root))))))

  (define-key vterm-mode-map
    (kbd (if (display-graphic-p) "<s-return>" "s-RET")) #'(lambda ()
                                                            (interactive)
                                                            (let* ((shell-pop-internal-mode "zsh-vterm")
                                                                   (buffer (get-buffer zsh-vterm-last-buffer))
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
  ;; (define-key vterm-mode-map (kbd "C-l") #'(lambda () (interactive) (let ((inhibit-read-only t)) (insert (s-repeat (count-screen-lines (window-start) (point)) "\n")) (vterm-send-C-l))))
  (define-key vterm-mode-map (kbd "C-z") #'vterm-send-C-z)
  (define-key vterm-mode-map (kbd "M-p") #'vterm-send-M-p)
  (define-key vterm-mode-map (kbd "s-r r") #'lx/run-in-vterm/rerun)
  (evil-define-key 'hybrid vterm-mode-map (kbd "C-z") #'vterm-send-C-z)
  (evil-define-key 'hybrid vterm-mode-map (kbd "<escape>") #'vterm-send-escape)
  (define-key vterm-mode-map (kbd "s-<backspace>") #'vterm-send-C-u)
  (define-key vterm-mode-map (kbd "M-D") #'(lambda () (interactive) (comint-send-string (get-buffer-process (current-buffer)) "exit-program\n")))
  (define-key vterm-mode-map (kbd "s-w") #'delete-window-or-bury-buffer)
  (define-key vterm-mode-map (kbd "<f12>") nil)
  (define-key vterm-mode-map (kbd "C-x C-c") #'(lambda () (interactive) (vterm-send-key "x" nil nil t) (vterm-send-key "c" nil nil t)))
  (define-key vterm-mode-map (kbd "C-x C-e") #'(lambda () (interactive) (vterm-send-key "x" nil nil t) (vterm-send-key "e" nil nil t)))
  (define-key vterm-mode-map (kbd "C-c e") #'(lambda () (interactive) (vterm-send-key "x" nil nil t) (vterm-send-key "e" nil nil t)))
  (define-key vterm-mode-map (kbd "C-x C-k") #'(lambda () (interactive) (vterm-send-key "x" nil nil t) (vterm-send-key "k" nil nil t)))
  (define-key vterm-mode-map (kbd "C-x C-s") #'(lambda () (interactive) (vterm-send-key "x" nil nil t) (vterm-send-key "s" nil nil t)))
  (define-key vterm-mode-map (kbd "C-x C-f") #'(lambda () (interactive) (vterm-send-key "x" nil nil t) (vterm-send-key "f" nil nil t)))
  (define-key vterm-mode-map (kbd "C-x C-b") #'(lambda () (interactive) (vterm-send-key "x" nil nil t) (vterm-send-key "b" nil nil t)))
  (define-key vterm-mode-map (kbd "C-x b") #'(lambda () (interactive) (vterm-send-key "x" nil nil t) (vterm-send-key "b" nil nil nil)))
  (define-key vterm-mode-map (kbd "C-x k") #'(lambda () (interactive) (vterm-send-key "x" nil nil t) (vterm-send-key "k" nil nil nil)))
  (define-key vterm-mode-map (kbd "C-x s") #'(lambda () (interactive) (vterm-send-key "x" nil nil t) (vterm-send-key "s" nil nil nil)))
  (define-key vterm-mode-map (kbd "<M-return>") #'(lambda () (interactive) (process-send-string vterm--process "\e\C-m")))
  (define-key vterm-mode-map (kbd "C-h") 'vterm-send-C-h)
  (define-key vterm-mode-map (kbd "M-/") 'all-buffer-completion)

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
    (cond ((derived-mode-p 'vterm-mode)
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

  (vterm-dnd-enable)

  (rvm-activate-corresponding-ruby))

(spacemacs|use-package-add-hook vterm
  :post-config
  (define-key vterm-mode-map (kbd "M-p") #'vterm-send-M-p)
  (define-key vterm-mode-map (kbd "M-/") 'all-buffer-completion))
