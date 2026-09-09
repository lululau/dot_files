(require 'ghostel)
(require 'run-in-ghostel)
(require 'shell-pop)

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
             (command-parts (lx/run-in-ghostel--expand-tilde-argv
                             (split-string-and-unquote command)))
             (buffer (generate-new-buffer buffer-name)))
        (pcase window-type
          ('split (split-window-right-and-focus))
          ('popup (select-window (shell-pop-split-window))))

        (setq zsh-ghostel-last-buffer (current-buffer))
        (with-current-buffer buffer
          (zsh-ghostel-mode)
          ;; Empty managed name prevents title tracking from renaming.
          (setq ghostel--managed-buffer-name ""
                ghostel-identity `((kind . zsh-ghostel-exec)
                                   (name . ,buffer-name)
                                   (instance . 1))))
        (pop-to-buffer buffer (append display-buffer--same-window-action
                                      '((category . comint))))
        (ghostel-exec buffer (car command-parts) (cdr command-parts))))))
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
  "Put BUFFER into `zsh-ghostel-mode' and record its terminal identity.
IDENTITY, if given, is a `ghostel-identity' alist stored on the buffer
so it can be found again after title-tracking renames it."
  (with-current-buffer buffer
    (unless (derived-mode-p 'zsh-ghostel-mode)
      (zsh-ghostel-mode))
    (setq ghostel--managed-buffer-name (buffer-name))
    (when identity
      (setq ghostel-identity identity))))

(defun zsh-ghostel--internal (pop-to-buf-fun &optional arg)
  "Find or create a `zsh-ghostel' terminal, mirroring `ghostel--start'.
POP-TO-BUF-FUN is retained for call-site compatibility but display uses
the same action as stock `ghostel' so size detection sees the window.
ARG follows `ghostel' prefix conventions: number selects an instance,
string forces that buffer name, other non-nil creates a fresh instance."
  (ghostel--load-module t)
  (let* ((fresh (and arg (not (numberp arg))))
         (name (if (stringp arg) arg ghostel-buffer-name))
         (context `((kind . zsh-ghostel) (name . ,name)))
         (instance (cond ((numberp arg) arg)
                         (fresh (ghostel--next-instance context))
                         (t 1)))
         (identity `(,@context (instance . ,instance)))
         (buf-name (if (and (not (stringp arg)) (> instance 1))
                       (format "%s<%d>" name instance)
                     name))
         (display-action (append display-buffer--same-window-action
                                 '((category . comint))))
         (existing (and (not fresh)
                        (ghostel--find-buffer-by-identity identity))))
    (if existing
        (progn
          (unless (buffer-local-value 'ghostel--term existing)
            (user-error "Ghostel buffer %s has no terminal"
                        (buffer-name existing)))
          (pop-to-buffer existing display-action)
          existing)
      (let ((buffer (generate-new-buffer buf-name)))
        (condition-case err
            (progn
              (zsh-ghostel--prepare-buffer buffer identity)
              ;; Display before init so rows/cols come from the real window.
              (if (functionp pop-to-buf-fun)
                  (funcall pop-to-buf-fun buffer)
                (pop-to-buffer buffer display-action))
              (ghostel--init-buffer buffer)
              (with-current-buffer buffer
                (setq ghostel--managed-buffer-name (buffer-name)
                      ghostel--initial-name (buffer-name)
                      ghostel-identity identity)
                (ghostel--start-process)
                (ghostel--apply-initial-input-mode))
              buffer)
          ((error quit)
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (signal (car err) (cdr err))))))))
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
    (evil-insert-state)
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

(defun zsh-ghostel-send-tmux (key)
  "Send tmux prefix (C-j) followed by KEY"
  (ghostel-send-key "j" "ctrl")
  (ghostel-send-string key))

(defun zsh-ghostel-send-shift-meta-C ()
  "Send Shift+Meta+C to terminal"
  (interactive) (ghostel-send-key "C" "shift,meta"))
(defun zsh-ghostel-send-shift-meta-V ()
  "Send Shift+Meta+V to terminal"
  (interactive) (ghostel-send-key "V" "shift,meta"))
(defun zsh-ghostel-send-shift-meta-N ()
  "Send Shift+Meta+N to terminal"
  (interactive) (ghostel-send-key "N" "shift,meta"))
(defun zsh-ghostel-send-shift-meta-P ()
  "Send Shift+Meta+P to terminal"
  (interactive) (ghostel-send-key "P" "shift,meta"))
(defun zsh-ghostel-tmux-1 ()
  "Switch to tmux pane 1"
  (interactive) (zsh-ghostel-send-tmux "1"))
(defun zsh-ghostel-tmux-2 ()
  "Switch to tmux pane 2"
  (interactive) (zsh-ghostel-send-tmux "2"))
(defun zsh-ghostel-tmux-3 ()
  "Switch to tmux pane 3"
  (interactive) (zsh-ghostel-send-tmux "3"))
(defun zsh-ghostel-tmux-4 ()
  "Switch to tmux pane 4"
  (interactive) (zsh-ghostel-send-tmux "4"))
(defun zsh-ghostel-tmux-5 ()
  "Switch to tmux pane 5"
  (interactive) (zsh-ghostel-send-tmux "5"))
(defun zsh-ghostel-tmux-6 ()
  "Switch to tmux pane 6"
  (interactive) (zsh-ghostel-send-tmux "6"))
(defun zsh-ghostel-tmux-7 ()
  "Switch to tmux pane 7"
  (interactive) (zsh-ghostel-send-tmux "7"))
(defun zsh-ghostel-tmux-8 ()
  "Switch to tmux pane 8"
  (interactive) (zsh-ghostel-send-tmux "8"))
(defun zsh-ghostel-tmux-9 ()
  "Switch to tmux pane 9"
  (interactive) (zsh-ghostel-send-tmux "9"))
(defun zsh-ghostel-tmux-0 ()
  "Switch to tmux pane 0"
  (interactive) (zsh-ghostel-send-tmux "0"))
(defun zsh-ghostel-tmux-z ()
  "Toggle tmux pane zoom"
  (interactive) (zsh-ghostel-send-tmux "z"))
(defun zsh-ghostel-tmux-double-ctrl-j ()
  "Send double C-j to terminal"
  (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-key "j" "ctrl"))
(defun zsh-ghostel-tmux-> ()
  "Swap tmux pane to next"
  (interactive) (zsh-ghostel-send-tmux ">"))
(defun zsh-ghostel-tmux-< ()
  "Swap tmux pane to previous"
  (interactive) (zsh-ghostel-send-tmux "<"))
(defun zsh-ghostel-send-ctrl-j ()
  "Send C-j to terminal"
  (interactive) (ghostel-send-key "j" "ctrl"))
(defun zsh-ghostel-self-insert ()
  "Self insert via ghostel"
  (interactive) (ghostel--self-insert))
(defun zsh-ghostel-maximize-or-tmux-zoom ()
  "Maximize window or toggle tmux pane zoom"
  (interactive)
  (if (window-parent)
      (spacemacs/toggle-maximize-buffer)
    (zsh-ghostel-send-tmux "z")))

(defvar zsh-ghostel-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ghostel-semi-char-mode-map)
    (define-key map (kbd "<backtab>") #'zsh-ghostel-accept-copilot-or-send-shift-tab-to-term)
    (define-key map (kbd "<tab>") #'zsh-ghostel-accept-copilot-or-send-tab-to-term)
    (define-key map (kbd "s-C") #'zsh-ghostel-previous-cli)
    (define-key map (kbd "s-V") #'zsh-ghostel-next-cli)
    (define-key map (kbd "s-a") #'ghostel-send-C-z)
    (define-key map (kbd "s-i s-o") #'zsh-ghostel-goto-tmp-dir)
    (define-key map (kbd "M-C") #'zsh-ghostel-send-shift-meta-C)
    (define-key map (kbd "M-V") #'zsh-ghostel-send-shift-meta-V)
    (define-key map (kbd "M-N") #'zsh-ghostel-send-shift-meta-N)
    (define-key map (kbd "M-P") #'zsh-ghostel-send-shift-meta-P)

    (evil-define-key 'visual map (kbd "<return>") #'evil-yank-for-zsh-ghostel)

    (evil-define-key 'insert map (kbd "M-C") #'zsh-ghostel-send-shift-meta-C)
    (evil-define-key 'hybrid map (kbd "M-C") #'zsh-ghostel-send-shift-meta-C)
    (evil-define-key 'insert map (kbd "M-V") #'zsh-ghostel-send-shift-meta-V)
    (evil-define-key 'insert map (kbd "M-N") #'zsh-ghostel-send-shift-meta-N)
    (evil-define-key 'hybrid map (kbd "M-N") #'zsh-ghostel-send-shift-meta-N)
    (evil-define-key 'insert map (kbd "M-P") #'zsh-ghostel-send-shift-meta-P)
    (evil-define-key 'hybrid map (kbd "M-P") #'zsh-ghostel-send-shift-meta-P)

    (evil-define-key 'insert map (kbd "M-!") #'zsh-ghostel-tmux-7)
    (evil-define-key 'hybrid map (kbd "M-!") #'zsh-ghostel-tmux-7)
    (evil-define-key 'insert map (kbd "M-@") #'zsh-ghostel-tmux-8)
    (evil-define-key 'hybrid map (kbd "M-@") #'zsh-ghostel-tmux-8)
    (evil-define-key 'insert map (kbd "M-#") #'zsh-ghostel-tmux-9)
    (evil-define-key 'hybrid map (kbd "M-#") #'zsh-ghostel-tmux-9)
    (evil-define-key 'insert map (kbd "M-$") #'zsh-ghostel-tmux-0)
    (evil-define-key 'hybrid map (kbd "M-$") #'zsh-ghostel-tmux-0)
    (evil-define-key 'insert map (kbd "s-z") #'zsh-ghostel-tmux-z)
    (evil-define-key 'hybrid map (kbd "s-z") #'zsh-ghostel-tmux-z)
    (evil-define-key 'insert map (kbd "s-j") #'zsh-ghostel-send-ctrl-j)
    (evil-define-key 'hybrid map (kbd "s-j") #'zsh-ghostel-send-ctrl-j)

    (evil-define-key 'insert map (kbd "C-M-s-!") #'zsh-ghostel-tmux-1)
    (evil-define-key 'hybrid map (kbd "C-M-s-!") #'zsh-ghostel-tmux-1)
    (evil-define-key 'insert map (kbd "C-M-s-@") #'zsh-ghostel-tmux-2)
    (evil-define-key 'hybrid map (kbd "C-M-s-@") #'zsh-ghostel-tmux-2)
    (evil-define-key 'insert map (kbd "C-M-s-#") #'zsh-ghostel-tmux-3)
    (evil-define-key 'hybrid map (kbd "C-M-s-#") #'zsh-ghostel-tmux-3)
    (evil-define-key 'insert map (kbd "C-M-s-$") #'zsh-ghostel-tmux-4)
    (evil-define-key 'hybrid map (kbd "C-M-s-$") #'zsh-ghostel-tmux-4)
    (evil-define-key 'insert map (kbd "C-M-s-%") #'zsh-ghostel-tmux-5)
    (evil-define-key 'hybrid map (kbd "C-M-s-%") #'zsh-ghostel-tmux-5)
    (evil-define-key 'insert map (kbd "C-M-s-^") #'zsh-ghostel-tmux-6)
    (evil-define-key 'hybrid map (kbd "C-M-s-^") #'zsh-ghostel-tmux-6)
    (evil-define-key 'insert map (kbd "C-M-s-|") #'zsh-ghostel-tmux-double-ctrl-j)
    (evil-define-key 'hybrid map (kbd "C-M-s-|") #'zsh-ghostel-tmux-double-ctrl-j)

    (evil-define-key 'normal map (kbd "C-M-s-!") #'zsh-ghostel-tmux-1)
    (evil-define-key 'normal map (kbd "C-M-s-@") #'zsh-ghostel-tmux-2)
    (evil-define-key 'normal map (kbd "C-M-s-#") #'zsh-ghostel-tmux-3)
    (evil-define-key 'normal map (kbd "C-M-s-$") #'zsh-ghostel-tmux-4)
    (evil-define-key 'normal map (kbd "C-M-s-%") #'zsh-ghostel-tmux-5)
    (evil-define-key 'normal map (kbd "C-M-s-^") #'zsh-ghostel-tmux-6)
    (evil-define-key 'normal map (kbd "C-M-s-|") #'zsh-ghostel-tmux-double-ctrl-j)

    (evil-define-key 'insert map (kbd "s-]") #'zsh-ghostel-tmux->)
    (evil-define-key 'hybrid map (kbd "s-]") #'zsh-ghostel-tmux->)
    (evil-define-key 'insert map (kbd "s-[") #'zsh-ghostel-tmux-<)
    (evil-define-key 'hybrid map (kbd "s-[") #'zsh-ghostel-tmux-<)
    (evil-define-key 'normal map (kbd "s-]") #'zsh-ghostel-tmux->)
    (evil-define-key 'normal map (kbd "s-[") #'zsh-ghostel-tmux-<)

    (evil-define-key 'insert map (kbd "M-h") #'ghostel-send-meta-h)
    (evil-define-key 'hybrid map (kbd "M-h") #'ghostel-send-meta-h)

    ;; (define-key map (kbd "<s-S-return>") #'zsh-ghostel-maximize-or-tmux-zoom)

    map))

(define-derived-mode zsh-ghostel-mode ghostel-mode "zsh"
  "Major mode for zsh ghostel buffer."
  (use-local-map zsh-ghostel-mode-map))

(defun zsh-ghostel--restore-keymap (&rest _)
  "Restore `zsh-ghostel-mode-map' after ghostel switches back to semi-char mode."
  (when (derived-mode-p 'zsh-ghostel-mode)
    (use-local-map zsh-ghostel-mode-map)))

(advice-add 'ghostel-semi-char-mode :after #'zsh-ghostel--restore-keymap)

;; In a visual selection ghostel activates the region and flips into copy
;; (read-only) input mode, replacing the local map with `ghostel--readonly-keymap'.
;; That map lacks `s-C'/`s-V', so they fall through to `global-map'.  Overlay a
;; child keymap that keeps the CLI-navigation bindings on top of ghostel's
;; read-only map for zsh-ghostel buffers.
(defvar zsh-ghostel-readonly-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-C") #'zsh-ghostel-previous-cli)
    (define-key map (kbd "s-V") #'zsh-ghostel-next-cli)
    map)
  "Bindings overlaid on ghostel's read-only map in `zsh-ghostel-mode' buffers.")

(defun zsh-ghostel--restore-readonly-keymap (&rest _)
  "Overlay `zsh-ghostel-readonly-mode-map' after entering copy/Emacs mode."
  (when (derived-mode-p 'zsh-ghostel-mode)
    (set-keymap-parent zsh-ghostel-readonly-mode-map (ghostel--readonly-keymap))
    (use-local-map zsh-ghostel-readonly-mode-map)))

(advice-add 'ghostel--enter-readonly :after #'zsh-ghostel--restore-readonly-keymap)

(provide 'zsh-ghostel)
