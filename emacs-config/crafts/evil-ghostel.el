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
      (evil-define-key 'insert ghostel-mode-map (kbd "<escape>")
        (lookup-key evil-insert-state-map (kbd "<escape>")))
    (evil-define-key 'insert ghostel-mode-map
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
  "Delete text from BEG to END, clamped to the input region.
Uses Ctrl+a + Ctrl+k fast path for whole-line deletions (dd, S, cc)."
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
      (if (and (= beg input-start) (= end input-end))
          (progn
            (ghostel-send-key "a" "ctrl")
            (ghostel-send-key "k" "ctrl"))
        (evil-ghostel--delete-region-in-terminal beg end)))))

(evil-define-operator evil-ghostel-delete-line (beg end type register yank-handler)
  "Delete to end of line in the terminal."
  :motion nil
  :keep-visual t
  (let* ((bounds (evil-ghostel--input-bounds))
         (input-start (car bounds))
         (input-end (cdr bounds))
         (beg (or beg (point)))
         (end (or end beg))
         (visual-line-mode (and evil-respect-visual-line-mode visual-line-mode))
         (line-end (if visual-line-mode
                       (save-excursion (end-of-visual-line) (point))
                     (line-end-position)))
         (effective-end (min line-end (or input-end line-end))))
    (when (and bounds (< beg effective-end))
      (when (evil-visual-state-p)
        (unless (memq type '(line screen-line block))
          (let ((range (evil-expand beg end
                                    (if visual-line-mode 'screen-line 'line))))
            (setq beg (evil-range-beginning range)
                  end (evil-range-end range)
                  type (evil-type range))))
        (evil-exit-visual-state))
      (unless register
        (let ((text (filter-buffer-substring beg effective-end)))
          (unless (string-match-p "\n" text)
            (evil-set-register ?- text))))
      (let ((evil-was-yanked-without-register nil))
        (evil-yank beg effective-end type register yank-handler))
      (evil-ghostel--position-terminal-cursor beg)
      (ghostel-send-key "k" "ctrl"))))

(evil-define-operator evil-ghostel-delete-backward-char (beg end type register yank-handler)
  "Delete previous character by sending Backspace."
  :motion evil-backward-char
  (when (evil-ghostel--point-in-input-p)
    (when (and beg end (< beg end))
      (unless register
        (let ((text (filter-buffer-substring beg end)))
          (unless (string-match-p "\n" text)
            (evil-set-register ?- text))))
      (let ((evil-was-yanked-without-register nil))
        (evil-yank beg end type register yank-handler))
      (ghostel-send-key "backspace"))))

(evil-define-operator evil-ghostel-delete-char (beg end type register yank-handler)
  "Delete current character."
  :motion evil-forward-char
  (evil-ghostel-delete beg end type register yank-handler))

(evil-define-operator evil-ghostel-replace (beg end type register yank-handler)
  "Replace character at point."
  :motion evil-forward-char
  (let ((replacement (make-string (- end beg) (read-char))))
    (evil-ghostel-delete beg end type register yank-handler)
    (ghostel-send-string replacement)
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

(evil-define-operator evil-ghostel-substitute (beg end type register yank-handler)
  "Replace character and enter insert."
  :motion evil-forward-char
  (evil-ghostel-change beg end type register yank-handler))

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
  "Paste after cursor, clamped to input region end."
  (interactive "P")
  (let* ((bounds (evil-ghostel--input-bounds))
         (target (if bounds (min (1+ (point)) (cdr bounds)) (1+ (point)))))
    (evil-ghostel--position-terminal-cursor target))
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
  (save-excursion
    (forward-line (or count 1))
    (unless (eobp)
      (evil-next-line count))))

(defun evil-ghostel-goto-cursor ()
  "Reset point to the terminal cursor position."
  (interactive)
  (when-let* ((pos (ghostel-cursor-point)))
    (goto-char pos)))

(defun evil-ghostel-undo ()
  "Send readline undo (Ctrl+_)."
  (interactive)
  (ghostel-send-string "\x1f"))

(defun evil-ghostel-previous-prompt (&optional n)
  "Navigate to Nth previous prompt."
  (interactive "p")
  (ghostel--navigate-previous-prompt n))

(defun evil-ghostel-next-prompt (&optional n)
  "Navigate to Nth next prompt."
  (interactive "p")
  (ghostel--navigate-next-prompt n))

;; ---------------------------------------------------------------------------
;; Setup
;; ---------------------------------------------------------------------------

;;;###autoload
(defun evil-ghostel-setup ()
  "Set up `evil' bindings for `ghostel'."
  (evil-set-initial-state 'ghostel-mode 'insert)

  (add-hook 'ghostel-mode-hook #'evil-ghostel-escape-stay)

  ;; ESC toggle
  (evil-define-key '(normal insert) ghostel-mode-map
    (kbd "C-c C-z") 'evil-ghostel-toggle-send-escape)

  ;; C- key passthrough in insert state
  (evil-define-key 'insert ghostel-mode-map
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
  (evil-define-key 'normal ghostel-mode-map
    "[[" #'evil-ghostel-previous-prompt
    "]]" #'evil-ghostel-next-prompt
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
    "G" 'evil-ghostel-goto-cursor)

  ;; Visual state bindings
  (evil-define-key 'visual ghostel-mode-map
    "d" 'evil-ghostel-delete
    "x" 'evil-ghostel-delete))

(with-eval-after-load 'ghostel
  (evil-ghostel-setup))

(provide 'evil-ghostel)
;;; evil-ghostel.el ends here
