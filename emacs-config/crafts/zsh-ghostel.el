(require 'ghostel)

(defvar zsh-ghostel-zsh-buffer-index 0)

(defvar zsh-ghostel-last-buffer nil)

(defvar zsh-ghostel-prompt-regexp "^.*\\(❯\\|\\]#\\|\\]\\$\\|➜\\) ")

(defvar zsh-ghostel-prompt-has-previous-regexp "^.*\\(❯\\|➜\\) ")

(defun lx/run-in-zsh-ghostel (command buffer-name &optional directory window-type)
  (interactive)
  (let* ((buffer-name (or buffer-name (message "zsh %d" (cl-incf zsh-ghostel-zsh-buffer-index))))
         (buffer (get-buffer buffer-name))
         (window-type (or window-type 'default)))
    (set (intern (format "%s-command" buffer-name)) (list command buffer-name directory window-type))
    (set (intern (format "%s-process-environment" buffer-name)) process-environment)
    (set (intern (format "%s-kill-buffer-on-exit" buffer-name)) (bound-and-true-p ghostel-kill-buffer-on-exit))
    (if buffer
        (if (equal buffer (current-buffer))
            (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
                (bury-buffer)
              (delete-window))
          (setq zsh-ghostel-last-buffer (current-buffer))
          (pcase window-type
            ('split (if (get-buffer-window buffer)
                        (select-window (get-buffer-window buffer))
             (pop-to-buffer buffer 'display-buffer-pop-up-window)))
            ('popup (if (get-buffer-window buffer)
                        (select-window (get-buffer-window buffer))
             (select-window (shell-pop-split-window)) (switch-to-buffer buffer)))
            (_ (switch-to-buffer buffer))))

      (let* ((default-directory (or directory user-home-directory))
             (ghostel-shell command))
        (pcase window-type
          ('split (split-window-right-and-focus))
          ('popup (select-window (shell-pop-split-window))))

        (setq zsh-ghostel-last-buffer (current-buffer))
        (zsh-ghostel buffer-name)))))

(defun zsh-ghostel (&optional arg)
  "Create an interactive Ghostel buffer.
Start a new Ghostel session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x ghostel RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Ghostel sessions is determined by the
value of `ghostel-buffer-name'."
  (interactive "P")
  (zsh-ghostel--internal #'pop-to-buffer-same-window arg))

(defun zsh-ghostel--prepare-buffer (buffer &optional identity)
  "Put BUFFER into `ghostel-mode' and record its terminal identity.
IDENTITY, if given, is stored as `ghostel--buffer-identity' so the
buffer can be found again after title-tracking renames it."
  (with-current-buffer buffer
    (unless (derived-mode-p 'zsh-ghostel-mode)
      (zsh-ghostel-mode)
      (setq ghostel--managed-buffer-name (buffer-name))
      (setq ghostel--buffer-identity (or identity (buffer-name))))))

(defun zsh-ghostel--internal (pop-to-buf-fun &optional arg)
  (ghostel--load-module t)
  (let* ((fresh (and arg (not (numberp arg))))
         (identity (cond (fresh nil)
                         ((numberp arg)
                          (format "%s<%d>" ghostel-buffer-name arg))
                         (t ghostel-buffer-name)))
         (buffer (if fresh
                     (generate-new-buffer ghostel-buffer-name)
                   (or (ghostel--find-buffer-by-identity identity)
                       (get-buffer-create identity)))))
    (unless (with-current-buffer buffer (derived-mode-p 'zsh-ghostel-mode))
      (zsh-ghostel--prepare-buffer buffer identity))
    (pop-to-buffer buffer (append display-buffer--same-window-action
                                  '((category . comint))))
    (ghostel--init-buffer buffer identity)
    buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zsh-ghostel-previous-cli-output ()
  (if (string-match-p zsh-ghostel-prompt-has-previous-regexp (thing-at-point 'line))
      (evil-previous-line))
  (evil-previous-line)
  (evil-visual-line)
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute (concat "?" zsh-ghostel-prompt-regexp)))
  (evil-next-line)
  (beginning-of-line))

(defun zsh-ghostel-previous-cli-command ()
  (evil-previous-line)
  (beginning-of-line)
  (string-match zsh-ghostel-prompt-regexp (thing-at-point 'line))
  (forward-char (match-end 0))
  (evil-visual-char)
  (evil-end-of-line)
  (evil-backward-WORD-end))

(defun zsh-ghostel-previous-cli ()
  (interactive)
  (if (eq evil-state 'visual) (evil-exit-visual-state))
  (evil-normal-state)
  (if (string-match-p zsh-ghostel-prompt-regexp (thing-at-point 'line))
      (zsh-ghostel-previous-cli-output)
    (zsh-ghostel-previous-cli-command)))

(defun zsh-ghostel-next-cli-output ()
  (evil-next-line)
  (evil-visual-line)
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute (concat "/" zsh-ghostel-prompt-regexp)))
  (if (string-match-p zsh-ghostel-prompt-has-previous-regexp (thing-at-point 'line))
      (evil-previous-line))
  (evil-previous-line)
  (beginning-of-line))

(defun zsh-ghostel-next-cli-command ()
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute (concat "/" zsh-ghostel-prompt-regexp)))
  (beginning-of-line)
  (string-match zsh-ghostel-prompt-regexp (thing-at-point 'line))
  (forward-char (match-end 0))
  (evil-visual-char)
  (evil-end-of-line)
  (evil-backward-WORD-end))

(defun zsh-ghostel-next-cli ()
  (interactive)
  (if (eq evil-state 'visual) (evil-exit-visual-state))
  (evil-normal-state)
  (if (string-match-p zsh-ghostel-prompt-regexp (thing-at-point 'line))
      (zsh-ghostel-next-cli-output)
    (zsh-ghostel-next-cli-command)))

(evil-define-operator evil-yank-for-zsh-ghostel (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((evil-was-yanked-without-register
         (and evil-was-yanked-without-register (not register))))
    (cond
     ((and (fboundp 'cua--global-mark-active)
           (fboundp 'cua-copy-region-to-global-mark)
           (cua--global-mark-active))
      (cua-copy-region-to-global-mark beg end))
     ((eq type 'block)
      (evil-yank-rectangle beg end register yank-handler))
     ((memq type '(line screen-line))
      (evil-yank-lines beg end register yank-handler))
     (t
      (evil-yank-characters beg end register yank-handler)))
    (evil-hybrid-state)
    (ghostel-send-string "a")
    (ghostel-send-key "backspace")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zsh-ghostel-get-current-line ()
  (let* ((start (line-beginning-position))
        (end (point))
        (str (buffer-substring-no-properties start end)))
    (replace-regexp-in-string zsh-ghostel-prompt-regexp "" str)))

(defun zsh-ghostel-get-current-line-beginning ()
  (let ((idx-bar (string-match "│" (thing-at-point 'line))))
    (if idx-bar
        (if (> (- (point) (line-beginning-position)) idx-bar)
            (1+ idx-bar)
          0)
      0)))

(defun zsh-ghostel-accept-copilot-or-send-tab-to-term ()
  (interactive)
  (if (and (bound-and-true-p copilot--overlay) (copilot--overlay-visible))
      (copilot-accept-completion)
    (ghostel-send-key "tab")))

(defun zsh-ghostel-accept-copilot-or-send-shift-tab-to-term ()
  (interactive)
  (if (and (bound-and-true-p copilot--overlay) (copilot--overlay-visible))
      (copilot-accept-completion)
    (ghostel--self-insert)))

(defun zsh-ghostel-goto-tmp-dir ()
  (interactive)
  (let* ((project-root (projectile-project-root))
         (tmp-dir (concat project-root "tmp")))
    (if project-root
        (if (file-exists-p tmp-dir)
              (ghostel-send-string (concat "cd " tmp-dir "\n"))
          (message "No tmp directory found"))
      (ghostel-send-string "cd ~/tmp\n"))))

(defvar zsh-ghostel-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ghostel-semi-char-mode-map)
    (define-key map (kbd "<backtab>") #'zsh-ghostel-accept-copilot-or-send-shift-tab-to-term)
    (define-key map (kbd "<tab>") #'zsh-ghostel-accept-copilot-or-send-tab-to-term)
    (define-key map (kbd "s-C") #'zsh-ghostel-previous-cli)
    (define-key map (kbd "s-V") #'zsh-ghostel-next-cli)
    (define-key map (kbd "s-a") #'ghostel-send-C-z)
    (define-key map (kbd "s-i s-o") #'zsh-ghostel-goto-tmp-dir)
    (define-key map (kbd "M-C") #'(lambda () (interactive) (ghostel-send-key "C" "shift,meta")))
    (define-key map (kbd "M-V") #'(lambda () (interactive) (ghostel-send-key "V" "shift,meta")))
    (define-key map (kbd "M-N") #'(lambda () (interactive) (ghostel-send-key "N" "shift,meta")))
    (define-key map (kbd "M-P") #'(lambda () (interactive) (ghostel-send-key "P" "shift,meta")))

    (evil-define-key 'visual map (kbd "<return>") #'evil-yank-for-zsh-ghostel)

    (evil-define-key 'hybrid map (kbd "M-C") #'(lambda () (interactive) (ghostel-send-key "C" "shift,meta")))
    (evil-define-key 'hybrid map (kbd "M-V") #'(lambda () (interactive) (ghostel-send-key "V" "shift,meta")))
    (evil-define-key 'hybrid map (kbd "M-N") #'(lambda () (interactive) (ghostel-send-key "N" "shift,meta")))
    (evil-define-key 'hybrid map (kbd "M-P") #'(lambda () (interactive) (ghostel-send-key "P" "shift,meta")))


    (evil-define-key 'hybrid map (kbd "M-!") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "7")))
    (evil-define-key 'hybrid map (kbd "M-@") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "8")))
    (evil-define-key 'hybrid map (kbd "M-#") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "9")))
    (evil-define-key 'hybrid map (kbd "M-$") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "0")))
    (evil-define-key 'hybrid map (kbd "s-z") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "z")))
    (evil-define-key 'hybrid map (kbd "s-j") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl")))

    (evil-define-key 'hybrid map (kbd "C-M-s-!") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "1")))
    (evil-define-key 'hybrid map (kbd "C-M-s-@") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "2")))
    (evil-define-key 'hybrid map (kbd "C-M-s-#") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "3")))
    (evil-define-key 'hybrid map (kbd "C-M-s-$") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "4")))
    (evil-define-key 'hybrid map (kbd "C-M-s-%") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "5")))
    (evil-define-key 'hybrid map (kbd "C-M-s-^") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "6")))
    (evil-define-key 'hybrid map (kbd "C-M-s-|") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-key "j" "ctrl")))

    (evil-define-key 'normal map (kbd "C-M-s-!") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "1")))
    (evil-define-key 'normal map (kbd "C-M-s-@") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "2")))
    (evil-define-key 'normal map (kbd "C-M-s-#") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "3")))
    (evil-define-key 'normal map (kbd "C-M-s-$") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "4")))
    (evil-define-key 'normal map (kbd "C-M-s-%") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "5")))
    (evil-define-key 'normal map (kbd "C-M-s-^") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "6")))
    (evil-define-key 'normal map (kbd "C-M-s-|") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-key "j" "ctrl")))

    (evil-define-key 'hybrid map (kbd "s-]") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string ">")))
    (evil-define-key 'hybrid map (kbd "s-[") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "<")))
    (evil-define-key 'normal map (kbd "s-]") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string ">")))
    (evil-define-key 'normal map (kbd "s-[") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "<")))

    (evil-define-key 'hybrid map (kbd "M-h") #'(lambda () (interactive) (ghostel--self-insert)))

    (define-key map (kbd "<s-S-return>") #'(lambda () (interactive) (if (window-parent) (spacemacs/toggle-maximize-buffer) (ghostel-send-key "j" "ctrl") (ghostel-send-string "z"))))

    map))

(define-derived-mode zsh-ghostel-mode ghostel-mode "zsh"
  "Major mode for zsh ghostel buffer."
  (use-local-map zsh-ghostel-mode-map))

(defun zsh-ghostel--restore-keymap (&rest _)
  "Restore `zsh-ghostel-mode-map' after ghostel switches back to semi-char mode."
  (when (derived-mode-p 'zsh-ghostel-mode)
    (use-local-map zsh-ghostel-mode-map)))

(advice-add 'ghostel-semi-char-mode :after #'zsh-ghostel--restore-keymap)

(provide 'zsh-ghostel)
