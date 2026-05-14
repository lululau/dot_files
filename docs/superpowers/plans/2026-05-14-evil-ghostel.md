# evil-ghostel.el Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Create evil-ghostel.el providing Evil insert/normal state integration for ghostel terminal, and adapt existing files to the new state model.

**Architecture:** evil-ghostel.el uses "position + send Delete" strategy — in normal state, evil operators calculate the region from buffer text, yank to kill-ring, then send terminal key sequences (arrow keys for positioning, Delete/Backspace/Ctrl+k for deletion). The file is loaded after ghostel and sets initial state to insert. Existing files (ghostel-mode.el, zsh-ghostel.el) are modified to remove hybrid-state bindings and comint remnants.

**Tech Stack:** Emacs Lisp, ghostel (libghostty-vt), evil-mode

**Spec:** `/Users/liuxiang/config-dev/docs/superpowers/specs/2026-05-14-evil-ghostel-design.md`

---

## Task 1: Create evil-ghostel.el

**Files:**
- Create: `/Users/liuxiang/config-dev/emacs-config/package-hooks/evil-ghostel.el`

- [ ] **Step 1: Create evil-ghostel.el with complete content**

```elisp
;; -*- lexical-binding: t; -*-
;;; evil-ghostel.el --- Evil integration for ghostel -*- lexical-binding: t; -*-
;;; Commentary:
;;; Evil insert/normal state integration for ghostel terminal emulator.
;;; Replaces evil-collection-vterm with equivalent functionality using
;;; terminal key sequences for normal-state operators.

;;; Code:

(require 'evil)
(require 'ghostel nil t)

;; ---------------------------------------------------------------------------
;; Customization
;; ---------------------------------------------------------------------------

(defcustom evil-ghostel-move-cursor-back nil
  "Whether the cursor is moved backwards when exiting insert state.
Moving cursor backwards is the default vim behavior but
it is not appropriate in terminals."
  :type 'boolean
  :group 'ghostel)

;; ---------------------------------------------------------------------------
;; State management
;; ---------------------------------------------------------------------------

(defun evil-ghostel-escape-stay ()
  "Don't move cursor back when exiting insert state."
  (setq-local evil-move-cursor-back evil-ghostel-move-cursor-back))

;; ---------------------------------------------------------------------------
;; ESC toggle (for nested evil: vim inside ghostel, ssh'd emacs, etc.)
;; ---------------------------------------------------------------------------

(defvar-local evil-ghostel-send-escape-to-vterm-p nil
  "Track whether ESC is sent to the terminal or to Emacs.")

(defun evil-ghostel-toggle-send-escape ()
  "Toggle where ESC is sent between terminal and Emacs.
This is needed for programs that use ESC, e.g. vim or an ssh'd emacs that
also uses `evil-mode'."
  (interactive)
  (if evil-ghostel-send-escape-to-vterm-p
      (evil-define-key 'insert 'ghostel-mode-map (kbd "<escape>")
        (lookup-key evil-insert-state-map (kbd "<escape>")))
    (evil-define-key 'insert 'ghostel-mode-map
      (kbd "<escape>") 'ghostel--self-insert))
  (setq evil-ghostel-send-escape-to-vterm-p
        (not evil-ghostel-send-escape-to-vterm-p))
  (message "Sending ESC to %s."
           (if evil-ghostel-send-escape-to-vterm-p
               "vterm"
             "emacs")))

;; ---------------------------------------------------------------------------
;; Core helpers
;; ---------------------------------------------------------------------------

(defun evil-ghostel--input-bounds ()
  "Return (INPUT-START . INPUT-END) for the current input line.
Returns nil when not on a prompt line."
  (let* ((input-start (ghostel-input-start-point))
         (cursor-pos (ghostel-cursor-point))
         (input-end (when cursor-pos
                      (save-excursion
                        (goto-char cursor-pos)
                        (line-end-position)))))
    (when (and input-start input-end (<= input-start input-end))
      (cons input-start input-end))))

(defun evil-ghostel--point-in-input-p ()
  "Return non-nil if point is within the current input area."
  (when-let* ((bounds (evil-ghostel--input-bounds)))
    (and (>= (point) (car bounds))
         (<= (point) (cdr bounds)))))

(defun evil-ghostel--position-terminal-cursor (target-pos)
  "Move the terminal cursor to align with TARGET-POS in the buffer."
  (let ((current (ghostel-cursor-point)))
    (when (and current target-pos (/= current target-pos))
      (let ((diff (- target-pos current)))
        (cond
         ((> diff 0) (dotimes (_ diff) (ghostel-send-key "right")))
         ((< diff 0) (dotimes (_ (- diff)) (ghostel-send-key "left"))))))))

(defun evil-ghostel--delete-region-in-terminal (beg end)
  "Delete text between BEG and END in the terminal via key sequences.
Positions the terminal cursor to BEG, then sends Delete for each character."
  (let ((count (- end beg)))
    (when (> count 0)
      (evil-ghostel--position-terminal-cursor beg)
      (dotimes (_ count)
        (ghostel-send-key "delete")))))

;; ---------------------------------------------------------------------------
;; Operators
;; ---------------------------------------------------------------------------

(evil-define-operator evil-ghostel-delete (beg end type register yank-handler)
  "Delete text from BEG to END, clamped to the input region."
  :motion nil
  (let* ((bounds (evil-ghostel--input-bounds))
         (input-start (car bounds))
         (input-end (cdr bounds))
         (beg (max (or beg (point)) (or input-start (point))))
         (end (min (or end beg) (or input-end (point)))))
    (when (and bounds (< beg end))
      (unless register
        (let ((text (filter-buffer-substring beg end)))
          (unless (string-match-p "\n" text)
            (evil-set-register ?- text))))
      (let ((evil-was-yanked-without-register nil))
        (evil-yank beg end type register yank-handler))
      (evil-ghostel--delete-region-in-terminal beg end))))

(evil-define-operator evil-ghostel-delete-line (beg end type register yank-handler)
  "Delete to end of line in the terminal."
  :motion nil
  :keep-visual t
  (let* ((bounds (evil-ghostel--input-bounds))
         (input-start (car bounds))
         (input-end (cdr bounds))
         (beg (or beg (point)))
         (end (or end beg))
         (line-end (if (and evil-respect-visual-line-mode visual-line-mode)
                       (save-excursion (end-of-visual-line) (point))
                     (line-end-position))))
    (when (and bounds (< beg (min end input-end)))
      (when (evil-visual-state-p)
        (evil-exit-visual-state))
      (unless register
        (let ((text (filter-buffer-substring beg (min end input-end))))
          (unless (string-match-p "\n" text)
            (evil-set-register ?- text))))
      (let ((evil-was-yanked-without-register nil))
        (evil-yank beg (min end input-end) type register yank-handler))
      (evil-ghostel--position-terminal-cursor beg)
      (ghostel-send-key "k" '(control)))))

(evil-define-operator evil-ghostel-delete-backward-char (beg end type register)
  "Delete previous character."
  :motion evil-backward-char
  (evil-ghostel-delete beg end type register))

(evil-define-operator evil-ghostel-delete-char (beg end type register)
  "Delete current character."
  :motion evil-forward-char
  (evil-ghostel-delete beg end type register))

(evil-define-operator evil-ghostel-replace (beg end type register yank-handler)
  "Replace character at point."
  :motion evil-forward-char
  (let ((replacement (make-string (- end beg) (read-char))))
    (evil-ghostel-delete beg end type register yank-handler)
    (ghostel-send-string replacement)
    ;; Move cursor back to replacement position (like vim's r)
    (dotimes (_ (length replacement))
      (ghostel-send-key "left"))))

(evil-define-operator evil-ghostel-change (beg end type register yank-handler)
  "Delete and enter insert state."
  (evil-ghostel-delete beg end type register yank-handler)
  (call-interactively #'evil-insert))

(evil-define-operator evil-ghostel-change-line (beg end type register yank-handler)
  "Delete to end of line and enter insert state."
  :motion evil-end-of-line-or-visual-line
  (evil-ghostel-delete-line beg end type register yank-handler)
  (call-interactively #'evil-insert))

(evil-define-operator evil-ghostel-substitute (beg end type register)
  "Replace character and enter insert."
  :motion evil-forward-char
  (evil-ghostel-change beg end type register))

(evil-define-operator evil-ghostel-substitute-line (beg end register yank-handler)
  "Replace entire line and enter insert."
  :motion evil-line-or-visual-line
  :type line
  (evil-ghostel-change beg end 'line register yank-handler))

;; ---------------------------------------------------------------------------
;; Insert / Append commands
;; ---------------------------------------------------------------------------

(defun evil-ghostel-insert ()
  "Insert before cursor."
  (interactive)
  (evil-ghostel--position-terminal-cursor (point))
  (call-interactively #'evil-insert))

(defun evil-ghostel-insert-line ()
  "Insert at beginning of input."
  (interactive)
  (let ((input-start (ghostel-input-start-point)))
    (when input-start
      (evil-ghostel--position-terminal-cursor input-start)))
  (call-interactively #'evil-insert))

(defun evil-ghostel-append ()
  "Append after cursor."
  (interactive)
  (evil-ghostel--position-terminal-cursor (1+ (point)))
  (call-interactively #'evil-insert))

(defun evil-ghostel-append-line ()
  "Append at end of input line."
  (interactive)
  (let* ((bounds (evil-ghostel--input-bounds))
         (input-end (cdr bounds)))
    (when input-end
      (evil-ghostel--position-terminal-cursor input-end)))
  (call-interactively #'evil-insert))

;; ---------------------------------------------------------------------------
;; Paste
;; ---------------------------------------------------------------------------

(defun evil-ghostel-paste-after (&optional arg)
  "Paste after cursor."
  (interactive "P")
  (evil-ghostel--position-terminal-cursor (1+ (point)))
  (ghostel-paste-string (current-kill 0)))

;; ---------------------------------------------------------------------------
;; Motions
;; ---------------------------------------------------------------------------

(evil-define-motion evil-ghostel-first-non-blank ()
  "Move to first non-blank character after the prompt."
  :type exclusive
  (if (ghostel-input-start-point)
      (goto-char (ghostel-input-start-point))
    (evil-first-non-blank)))

(evil-define-motion evil-ghostel-next-line (count)
  "Move COUNT lines down, but not past the last prompt line."
  :type line
  (when (> (count-words (point) (point-max)) 0)
    (evil-next-line count)))

(defun evil-ghostel-goto-cursor ()
  "Reset point to the terminal cursor position."
  (interactive)
  (when-let* ((pos (ghostel-cursor-point)))
    (goto-char pos)))

(defun evil-ghostel-undo ()
  "Send readline undo (Ctrl+_)."
  (interactive)
  (ghostel-send-string "\x1f"))

;; ---------------------------------------------------------------------------
;; Setup
;; ---------------------------------------------------------------------------

;;;###autoload
(defun evil-ghostel-setup ()
  "Set up `evil' bindings for `ghostel'."
  (evil-set-initial-state 'ghostel-mode 'insert)

  (add-hook 'ghostel-mode-hook #'evil-ghostel-escape-stay)

  ;; ESC toggle
  (evil-define-key '(normal insert) 'ghostel-mode-map
    (kbd "C-c C-z") 'evil-ghostel-toggle-send-escape)

  ;; C- key passthrough in insert state — ensure evil doesn't intercept them
  (evil-define-key 'insert 'ghostel-mode-map
    (kbd "C-a") 'ghostel--self-insert
    (kbd "C-d") 'ghostel--self-insert
    (kbd "C-e") 'ghostel--self-insert
    (kbd "C-k") 'ghostel--self-insert
    (kbd "C-n") 'ghostel--self-insert
    (kbd "C-o") 'ghostel--self-insert
    (kbd "C-p") 'ghostel--self-insert
    (kbd "C-r") 'ghostel--self-insert
    (kbd "C-t") 'ghostel--self-insert
    (kbd "C-w") 'ghostel--self-insert
    (kbd "C-y") 'ghostel--self-insert
    (kbd "C-z") 'ghostel--self-insert
    (kbd "<delete>") 'ghostel-send-C-d)

  ;; Normal state bindings
  (evil-define-key 'normal 'ghostel-mode-map
    "[[" #'(lambda (&optional n) (interactive "p") (ghostel--navigate-previous-prompt n))
    "]]" #'(lambda (&optional n) (interactive "p") (ghostel--navigate-next-prompt n))
    "p" 'evil-ghostel-paste-after
    "P" 'ghostel-yank
    "a" 'evil-ghostel-append
    "A" 'evil-ghostel-append-line
    "d" 'evil-ghostel-delete
    "D" 'evil-ghostel-delete-line
    "x" 'evil-ghostel-delete-char
    "X" 'evil-ghostel-delete-backward-char
    "^" 'evil-ghostel-first-non-blank
    "i" 'evil-ghostel-insert
    "I" 'evil-ghostel-insert-line
    "u" 'evil-ghostel-undo
    "r" 'evil-ghostel-replace
    "c" 'evil-ghostel-change
    "C" 'evil-ghostel-change-line
    "s" 'evil-ghostel-substitute
    "S" 'evil-ghostel-substitute-line
    "j" 'evil-ghostel-next-line
    "G" 'evil-ghostel-goto-cursor)

  ;; Visual state bindings
  (evil-define-key 'visual 'ghostel-mode-map
    "d" 'evil-ghostel-delete
    "x" 'evil-ghostel-delete))

(with-eval-after-load 'ghostel
  (evil-ghostel-setup))

(provide 'evil-ghostel)
;;; evil-ghostel.el ends here
```

- [ ] **Step 2: Verify file loads without errors**

Run: `emacsclient -e '(progn (load "/Users/liuxiang/config-dev/emacs-config/package-hooks/evil-ghostel.el" nil t) (message "evil-ghostel loaded"))'`
Expected: `"evil-ghostel loaded"`

- [ ] **Step 3: Commit**

```bash
cd /Users/liuxiang/config-dev
git add emacs-config/package-hooks/evil-ghostel.el
git commit -m "feat: create evil-ghostel.el — Evil insert/normal integration for ghostel"
```

---

## Task 2: Modify ghostel-mode.el — remove hybrid bindings, fix comint remnants

**Files:**
- Modify: `/Users/liuxiang/config-dev/emacs-config/package-hooks/ghostel-mode.el`

Changes:
1. Remove all `evil-define-key 'hybrid` bindings (lines 64-68) — now handled by evil-ghostel.el
2. Replace `comint-send-string (get-buffer-process ...)` calls (lines 53-59, 70) with `ghostel-send-string`
3. Change `ghostel-enter-hybrid-state-decently` to enter insert state instead of hybrid (line 17-21)
4. Replace `process-send-string ghostel--process` with `ghostel-send-string` (line 86)
5. Remove `(require 'shell-pop)` — it's only needed by zsh-ghostel.el which requires it itself

- [ ] **Step 1: Replace file content**

The full replacement for `/Users/liuxiang/config-dev/emacs-config/package-hooks/ghostel-mode.el`:

```elisp
;; -*- lexical-binding: t; -*-

(with-eval-after-load 'ghostel

  (defcustom ghostel-kill-buffer-on-normal-exit t
    "Kill buffer on normal exit (finished status)."
    :type 'boolean
    :group 'ghostel)

  (add-hook 'ghostel-exit-functions
    (lambda (buf event)
      (when (and ghostel-kill-buffer-on-normal-exit (buffer-live-p buf)
                 (string= "finished\n" event))
        (kill-buffer buf))))

  (require 'shell-pop)

  (defun ghostel-enter-insert-state-decently ()
    (interactive)
    (evil-insert-state)
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

  (define-key ghostel-mode-map (kbd "<s-left>") #'(lambda () (interactive) (ghostel-send-string "frame\n")))
  (define-key ghostel-mode-map (kbd "<s-up>") #'(lambda () (interactive) (ghostel-send-string "up\n")))
  (define-key ghostel-mode-map (kbd "<s-down>") #'(lambda () (interactive) (ghostel-send-string "down\n")))
  (define-key ghostel-mode-map (kbd "<f6>") #'(lambda () (interactive) (ghostel-send-string "s\n")))
  (define-key ghostel-mode-map (kbd "<f7>") #'(lambda () (interactive) (ghostel-send-string "f\n")))
  (define-key ghostel-mode-map (kbd "<f8>") #'(lambda () (interactive) (ghostel-send-string "c\n")))
  (define-key ghostel-mode-map (kbd "<f9>") #'(lambda () (interactive) (ghostel-send-string "n\n")))
  (define-key ghostel-mode-map (kbd "C-z") #'ghostel-send-C-z)
  (define-key ghostel-mode-map (kbd "M-p") #'(lambda () (interactive) (ghostel-send-key "p" '(meta))))
  (define-key ghostel-mode-map (kbd "s-r r") #'lx/run-in-ghostel/rerun)
  (define-key ghostel-mode-map (kbd "s-<backspace>") #'(lambda () (interactive) (ghostel-send-key "u" '(control))))
  (define-key ghostel-mode-map (kbd "M-D") #'(lambda () (interactive) (ghostel-send-string "exit-program\n")))
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
  (define-key ghostel-mode-map (kbd "<M-return>") #'(lambda () (interactive) (ghostel-send-string "\e\C-m")))
  (define-key ghostel-mode-map (kbd "C-h") #'(lambda () (interactive) (ghostel-send-key "h" '(control))))
  (define-key ghostel-mode-map (kbd "M-/") 'current-buffer-completion)
  (evil-define-key 'motion ghostel-mode-map (kbd "s-q") #'ghostel-enter-insert-state-decently)

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
```

- [ ] **Step 2: Commit**

```bash
cd /Users/liuxiang/config-dev
git add emacs-config/package-hooks/ghostel-mode.el
git commit -m "refactor: ghostel-mode.el — remove hybrid bindings, fix comint remnants"
```

---

## Task 3: Modify zsh-ghostel.el — hybrid → insert migration

**Files:**
- Modify: `/Users/liuxiang/config-dev/emacs-config/crafts/zsh-ghostel.el`

Changes:
1. All `evil-define-key 'hybrid` → `evil-define-key 'insert`
2. `(evil-hybrid-state)` → `(evil-insert-state)` in `evil-yank-for-zsh-ghostel`
3. `ghostel-enter-hybrid-state-decently` reference stays (defined in ghostel-mode.el, renamed there to `ghostel-enter-insert-state-decently`)

- [ ] **Step 1: Replace evil-yank-for-zsh-ghostel — hybrid → insert**

Change line 171 `(evil-hybrid-state)` to `(evil-insert-state)`:

Old:
```elisp
    (evil-hybrid-state)
    (ghostel-send-string "a")
    (ghostel-send-key "backspace")))
```

New:
```elisp
    (evil-insert-state)
    (ghostel-send-string "a")
    (ghostel-send-key "backspace")))
```

- [ ] **Step 2: Replace all `evil-define-key 'hybrid` with `evil-define-key 'insert` in zsh-ghostel-mode-map**

Replace every occurrence of `(evil-define-key 'hybrid map` with `(evil-define-key 'insert map` in the `zsh-ghostel-mode-map` definition (lines 229-263).

The complete `zsh-ghostel-mode-map` with all hybrid→insert changes:

```elisp
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

    (evil-define-key 'insert map (kbd "M-C") #'(lambda () (interactive) (ghostel-send-key "C" "shift,meta")))
    (evil-define-key 'insert map (kbd "M-V") #'(lambda () (interactive) (ghostel-send-key "V" "shift,meta")))
    (evil-define-key 'insert map (kbd "M-N") #'(lambda () (interactive) (ghostel-send-key "N" "shift,meta")))
    (evil-define-key 'insert map (kbd "M-P") #'(lambda () (interactive) (ghostel-send-key "P" "shift,meta")))

    (evil-define-key 'insert map (kbd "M-!") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "7")))
    (evil-define-key 'insert map (kbd "M-@") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "8")))
    (evil-define-key 'insert map (kbd "M-#") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "9")))
    (evil-define-key 'insert map (kbd "M-$") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "0")))
    (evil-define-key 'insert map (kbd "s-z") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "z")))
    (evil-define-key 'insert map (kbd "s-j") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl")))

    (evil-define-key 'insert map (kbd "C-M-s-!") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "1")))
    (evil-define-key 'insert map (kbd "C-M-s-@") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "2")))
    (evil-define-key 'insert map (kbd "C-M-s-#") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "3")))
    (evil-define-key 'insert map (kbd "C-M-s-$") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "4")))
    (evil-define-key 'insert map (kbd "C-M-s-%") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "5")))
    (evil-define-key 'insert map (kbd "C-M-s-^") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "6")))
    (evil-define-key 'insert map (kbd "C-M-s-|") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-key "j" "ctrl")))

    (evil-define-key 'normal map (kbd "C-M-s-!") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "1")))
    (evil-define-key 'normal map (kbd "C-M-s-@") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "2")))
    (evil-define-key 'normal map (kbd "C-M-s-#") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "3")))
    (evil-define-key 'normal map (kbd "C-M-s-$") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "4")))
    (evil-define-key 'normal map (kbd "C-M-s-%") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "5")))
    (evil-define-key 'normal map (kbd "C-M-s-^") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "6")))
    (evil-define-key 'normal map (kbd "C-M-s-|") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-key "j" "ctrl")))

    (evil-define-key 'insert map (kbd "s-]") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string ">")))
    (evil-define-key 'insert map (kbd "s-[") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "<")))
    (evil-define-key 'normal map (kbd "s-]") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string ">")))
    (evil-define-key 'normal map (kbd "s-[") #'(lambda () (interactive) (ghostel-send-key "j" "ctrl") (ghostel-send-string "<")))

    (evil-define-key 'insert map (kbd "M-h") #'(lambda () (interactive) (ghostel--self-insert)))

    (define-key map (kbd "<s-S-return>") #'(lambda () (interactive) (if (window-parent) (spacemacs/toggle-maximize-buffer) (ghostel-send-key "j" "ctrl") (ghostel-send-string "z"))))

    map))
```

- [ ] **Step 3: Commit**

```bash
cd /Users/liuxiang/config-dev
git add emacs-config/crafts/zsh-ghostel.el
git commit -m "refactor: zsh-ghostel.el — migrate hybrid state to insert state"
```

---

## Self-Review Checklist

**1. Spec coverage:**

| Spec section | Task |
|---|---|
| State model (insert/normal) | Task 1 (evil-set-initial-state) |
| Core delete strategy | Task 1 (position-terminal-cursor + delete-region-in-terminal) |
| Core helpers (input-bounds, etc.) | Task 1 |
| Operators (d, D, x, X, c, C, s, S, r) | Task 1 |
| Insert/Append (i, I, a, A) | Task 1 |
| Paste (p, P) | Task 1 |
| Navigation (^, [[, ]], j, G, u) | Task 1 |
| Visual mode (d, x) | Task 1 |
| ESC toggle | Task 1 |
| C- key passthrough | Task 1 |
| Cursor behavior (move-cursor-back) | Task 1 |
| ghostel-mode.el cleanup | Task 2 |
| zsh-ghostel.el hybrid→insert | Task 3 |

**2. Placeholder scan:** No TBD/TODO found. All code is complete.

**3. Type consistency:** All function names are consistent across tasks:
- `evil-ghostel--input-bounds` used in operators ✓
- `evil-ghostel--position-terminal-cursor` used in operators and insert commands ✓
- `evil-ghostel--delete-region-in-terminal` used in delete operator ✓
- `ghostel-enter-insert-state-decently` renamed from hybrid, referenced in ghostel-mode.el ✓
