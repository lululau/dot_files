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
;; Surround (ds, cs, ys) — hooks into standard evil-surround
;; ---------------------------------------------------------------------------
;; Global evil-surround-mode (from Spacemacs) provides the operator-state
;; dispatch.  We just need to:
;;   1. Register ghostel operators so the dispatch recognizes them.
;;   2. Override the actual delete/change/region functions in ghostel buffers
;;      to use terminal key sequences instead of direct buffer modifications.

(defun evil-ghostel--surround-pair (char)
  "Return (OPEN . CLOSE) for surround CHAR.
Uses `evil-surround-pairs-alist' if available, else simple pairs."
  (let ((entry (assoc char (if (boundp 'evil-surround-pairs-alist)
                               evil-surround-pairs-alist
                             nil))))
    (if entry
        (let ((val (cdr entry)))
          (if (functionp val) (cons (string char) (string char)) val))
      (cons (string char) (string char)))))

(defun evil-ghostel--find-surrounding-pair (char)
  "Find surrounding pair for CHAR around point within input bounds.
Returns (OPEN-POS . CLOSE-POS) or nil."
  (save-restriction
    (widen)
    (let* ((pair (evil-ghostel--surround-pair char))
           (open-str (car pair))
           (close-str (cdr pair))
           (same-p (string= open-str close-str))
           (bounds (evil-ghostel--input-bounds)))
      (when bounds
        (let ((input-start (car bounds))
              (input-end (cdr bounds))
              open-pos close-pos)
          (save-excursion
            (if same-p
                ;; Same-char pairs (quotes): search backward then forward
                (progn
                  (goto-char (min (1+ (point)) (1+ input-end)))
                  (setq open-pos (search-backward open-str input-start t))
                  (when open-pos
                    (goto-char (+ open-pos (length open-str)))
                    (when-let ((found (search-forward close-str (1+ input-end) t)))
                      (setq close-pos (- found (length close-str))))))
              ;; Different-char pairs: balanced nesting scan
              (let ((depth 0)
                    (pos (point)))
                ;; Backward scan for opening delimiter
                (catch 'found-open
                  (goto-char pos)
                  (cond
                   ((looking-at (regexp-quote open-str))
                    (setq open-pos pos) (throw 'found-open nil))
                   ((looking-at (regexp-quote close-str))
                    (setq depth 1)))
                  (setq pos (1- pos))
                  (while (>= pos input-start)
                    (goto-char pos)
                    (cond
                     ((looking-at (regexp-quote open-str))
                      (if (<= depth 1)
                          (progn (setq open-pos pos) (throw 'found-open nil))
                        (setq depth (1- depth))))
                     ((looking-at (regexp-quote close-str))
                      (setq depth (1+ depth))))
                    (setq pos (1- pos))))
                ;; Forward scan for matching close
                (when open-pos
                  (setq pos (+ open-pos (length open-str))
                        depth 1)
                  (catch 'found-close
                    (while (<= pos input-end)
                      (goto-char pos)
                      (cond
                       ((looking-at (regexp-quote open-str))
                        (setq depth (1+ depth)))
                       ((looking-at (regexp-quote close-str))
                        (setq depth (1- depth))
                        (when (= depth 0)
                          (setq close-pos pos)
                          (throw 'found-close nil))))
                      (setq pos (1+ pos))))))))
          (when (and open-pos close-pos)
            (cons open-pos close-pos)))))))

;; Helper: send N left/right arrow keys
(defun evil-ghostel--send-move (n)
  "Send |N| arrow keys: right if N>0, left if N<0."
  (cond
   ((> n 0) (dotimes (_ n) (ghostel-send-key "right")))
   ((< n 0) (dotimes (_ (- n)) (ghostel-send-key "left")))))

(defun evil-ghostel--surround-delete-impl (char &optional _outer _inner)
  "Terminal-aware implementation of surround delete for CHAR."
  (save-restriction
    (widen)
    (let* ((pair (evil-ghostel--surround-pair char))
           (pair-pos (evil-ghostel--find-surrounding-pair char)))
      (if (not pair-pos)
          (message "No surrounding pair found for `%c'" char)
        (let* ((open-pos (car pair-pos))
               (close-pos (cdr pair-pos))
               (open-len (length (car pair)))
               (close-len (length (cdr pair)))
               (cursor-pos (or (ghostel-cursor-point) (point))))
          ;; Track cursor manually — ghostel-cursor-point is stale after sends.
          ;; Process close first (higher pos), then open.
          (evil-ghostel--send-move (- close-pos cursor-pos))
          (dotimes (_ close-len) (ghostel-send-key "delete"))
          ;; Cursor is now at close-pos; move leftward to open-pos
          (evil-ghostel--send-move (- open-pos close-pos))
          (dotimes (_ open-len) (ghostel-send-key "delete")))))))

(defun evil-ghostel--surround-change-impl (char &optional _outer _inner)
  "Terminal-aware implementation of surround change for CHAR."
  (save-restriction
    (widen)
    (let* ((old-pair (evil-ghostel--surround-pair char))
           (pair-pos (evil-ghostel--find-surrounding-pair char)))
      (if (not pair-pos)
          (message "No surrounding pair found for `%c'" char)
        (let ((new-char (evil-surround-read-char)))
          (let* ((new-pair (evil-ghostel--surround-pair new-char))
                 (open-pos (car pair-pos))
                 (close-pos (cdr pair-pos))
                 (old-open-len (length (car old-pair)))
                 (old-close-len (length (cdr old-pair)))
                 (cursor-pos (or (ghostel-cursor-point) (point))))
            ;; Step 1: move to close, delete old close, insert new close
            (evil-ghostel--send-move (- close-pos cursor-pos))
            (dotimes (_ old-close-len) (ghostel-send-key "delete"))
            (ghostel-send-string (cdr new-pair))
            ;; Cursor is now at close-pos + new-close-len
            ;; Step 2: move to open-pos
            (let ((new-close-len (length (cdr new-pair))))
              (evil-ghostel--send-move (- open-pos (+ close-pos new-close-len))))
            ;; Delete old open, insert new open
            (dotimes (_ old-open-len) (ghostel-send-key "delete"))
            (ghostel-send-string (car new-pair))))))))

(defun evil-ghostel--surround-region-impl (beg end _type char &optional _force-new-line)
  "Terminal-aware implementation of surround region for BEG END CHAR."
  (save-restriction
    (widen)
    (when (and beg end char)
      (let* ((pair (evil-ghostel--surround-pair char))
             (cursor-pos (or (ghostel-cursor-point) (point))))
        ;; Step 1: move to end, insert close delimiter
        (evil-ghostel--send-move (- end cursor-pos))
        (ghostel-send-string (cdr pair))
        ;; Cursor is now at end + close-len; move to beg
        (let ((close-len (length (cdr pair))))
          (evil-ghostel--send-move (- beg (+ end close-len))))
        ;; Insert open delimiter
        (ghostel-send-string (car pair))))))

;; Advice functions: intercept evil-surround in ghostel buffers
(defun evil-ghostel--advice-surround-delete (orig-fn char &optional outer inner)
  "In ghostel buffers, use terminal key sequences for surround delete."
  (if (derived-mode-p 'ghostel-mode)
      (evil-ghostel--surround-delete-impl char outer inner)
    (funcall orig-fn char outer inner)))

(defun evil-ghostel--advice-surround-change (orig-fn char &optional outer inner)
  "In ghostel buffers, use terminal key sequences for surround change."
  (if (derived-mode-p 'ghostel-mode)
      (evil-ghostel--surround-change-impl char outer inner)
    (funcall orig-fn char outer inner)))

(defun evil-ghostel--advice-surround-region (orig-fn beg end type char &optional force-new-line)
  "In ghostel buffers, use terminal key sequences for surround region."
  (if (derived-mode-p 'ghostel-mode)
      (evil-ghostel--surround-region-impl beg end type char force-new-line)
    (funcall orig-fn beg end type char force-new-line)))

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
    "x" 'evil-ghostel-delete)

  ;; Register ghostel operators with evil-surround's dispatch so that
  ;; ds/cs work correctly (the standard dispatch checks this alist).
  (with-eval-after-load 'evil-surround
    (add-to-list 'evil-surround-operator-alist '(evil-ghostel-delete . delete))
    (add-to-list 'evil-surround-operator-alist '(evil-ghostel-change . change))
    ;; Advise evil-surround functions to use terminal sequences in ghostel
    (advice-add 'evil-surround-delete :around #'evil-ghostel--advice-surround-delete)
    (advice-add 'evil-surround-change :around #'evil-ghostel--advice-surround-change)
    (advice-add 'evil-surround-region :around #'evil-ghostel--advice-surround-region)))

(defcustom evil-ghostel-surround-pairs-alist
  '((?\( . ("(" . ")"))
    (?\) . ("(" . ")"))
    (?\[ . ("[" . "]"))
    (?\] . ("[" . "]"))
    (?\{ . ("{" . "}"))
    (?\} . ("{" . "}"))
    (?\" . ("\"" . "\""))
    (?\' . ("'" . "'"))
    (?` . ("`" . "`"))
    (?< . ("<" . ">"))
    (?> . ("<" . ">"))
    (?b . ("(" . ")"))
    (?B . ("{" . "}"))
    (?r . ("[" . "]"))
    (?a . ("<" . ">")))
  "Alist of trigger chars to (OPEN . CLOSE) surround pairs."
  :type '(alist :key-type character
                :value-type (cons string string))
  :group 'ghostel)

(defun evil-ghostel--surround-pair (char)
  "Return (OPEN . CLOSE) for surround CHAR."
  (let ((entry (assoc char evil-ghostel-surround-pairs-alist)))
    (if entry (cdr entry) (cons (string char) (string char)))))

(defun evil-ghostel-surround-read-char ()
  "Read a character, widening if in operator state."
  (save-restriction (widen) (read-char)))

(defun evil-ghostel--find-surrounding-pair (char)
  "Find surrounding pair for CHAR around point within input bounds.
Returns (OPEN-POS . CLOSE-POS) or nil."
  (save-restriction
    (widen)
    (let* ((pair (evil-ghostel--surround-pair char))
           (open-str (car pair))
           (close-str (cdr pair))
           (same-p (string= open-str close-str))
           (bounds (evil-ghostel--input-bounds)))
      (when bounds
        (let ((input-start (car bounds))
              (input-end (cdr bounds))
              open-pos close-pos)
          (save-excursion
            (if same-p
                ;; Same-char pairs (quotes): search backward then forward
                (progn
                  (goto-char (min (1+ (point)) (1+ input-end)))
                  (setq open-pos (search-backward open-str input-start t))
                  (when open-pos
                    (goto-char (+ open-pos (length open-str)))
                    (when-let ((found (search-forward close-str (1+ input-end) t)))
                      (setq close-pos (- found (length close-str))))))
              ;; Different-char pairs: balanced nesting scan
              (let ((depth 0)
                    (pos (point)))
                ;; Backward scan for opening delimiter
                (catch 'found-open
                  (goto-char pos)
                  (cond
                   ((looking-at (regexp-quote open-str))
                    (setq open-pos pos) (throw 'found-open nil))
                   ((looking-at (regexp-quote close-str))
                    (setq depth 1)))
                  (setq pos (1- pos))
                  (while (>= pos input-start)
                    (goto-char pos)
                    (cond
                     ((looking-at (regexp-quote open-str))
                      (if (<= depth 1)
                          (progn (setq open-pos pos) (throw 'found-open nil))
                        (setq depth (1- depth))))
                     ((looking-at (regexp-quote close-str))
                      (setq depth (1+ depth))))
                    (setq pos (1- pos))))
                ;; Forward scan for matching close
                (when open-pos
                  (setq pos (+ open-pos (length open-str))
                        depth 1)
                  (catch 'found-close
                    (while (<= pos input-end)
                      (goto-char pos)
                      (cond
                       ((looking-at (regexp-quote open-str))
                        (setq depth (1+ depth)))
                       ((looking-at (regexp-quote close-str))
                        (setq depth (1- depth))
                        (when (= depth 0)
                          (setq close-pos pos)
                          (throw 'found-close nil))))
                      (setq pos (1+ pos))))))))
          (when (and open-pos close-pos)
            (cons open-pos close-pos)))))))

;; Helper: send N left/right arrow keys
(defun evil-ghostel--send-move (n)
  "Send |N| arrow keys: right if N>0, left if N<0."
  (cond
   ((> n 0) (dotimes (_ n) (ghostel-send-key "right")))
   ((< n 0) (dotimes (_ (- n)) (ghostel-send-key "left")))))

;;;###autoload
(defun evil-ghostel-surround-delete (char)
  "Delete surrounding pair for CHAR (ds).
Reads CHAR interactively when called from a keybinding."
  (interactive (list (evil-ghostel-surround-read-char)))
  (save-restriction
    (widen)
    (let* ((pair (evil-ghostel--surround-pair char))
           (pair-pos (evil-ghostel--find-surrounding-pair char)))
      (if (not pair-pos)
          (message "No surrounding pair found for `%c'" char)
        (let* ((open-pos (car pair-pos))
               (close-pos (cdr pair-pos))
               (open-len (length (car pair)))
               (close-len (length (cdr pair)))
               (cursor-pos (or (ghostel-cursor-point) (point))))
          ;; Track cursor position manually because ghostel-cursor-point
          ;; won't update until the next terminal redraw (async).
          ;; Process close first (higher pos), then open — deleting close
          ;; doesn't shift open's position.

          ;; Step 1: move to close-pos and delete close delimiter
          (evil-ghostel--send-move (- close-pos cursor-pos))
          (dotimes (_ close-len) (ghostel-send-key "delete"))
          ;; Cursor is now at close-pos (chars after close shifted left)

          ;; Step 2: move from close-pos to open-pos (always leftward)
          (evil-ghostel--send-move (- open-pos close-pos))
          ;; Delete open delimiter
          (dotimes (_ open-len) (ghostel-send-key "delete")))))))

;;;###autoload
(defun evil-ghostel-surround-change (old-char)
  "Change surrounding pair from OLD-CHAR to new char (cs).
Reads OLD-CHAR and new char interactively."
  (interactive (list (evil-ghostel-surround-read-char)))
  (let ((new-char (evil-ghostel-surround-read-char)))
    (save-restriction
      (widen)
      (let* ((old-pair (evil-ghostel--surround-pair old-char))
             (new-pair (evil-ghostel--surround-pair new-char))
             (pair-pos (evil-ghostel--find-surrounding-pair old-char)))
        (if (not pair-pos)
            (message "No surrounding pair found for `%c'" old-char)
          (let* ((open-pos (car pair-pos))
                 (close-pos (cdr pair-pos))
                 (old-open-len (length (car old-pair)))
                 (old-close-len (length (cdr old-pair)))
                 (cursor-pos (or (ghostel-cursor-point) (point))))
            ;; Step 1: go to close-pos, delete old close, insert new close
            (evil-ghostel--send-move (- close-pos cursor-pos))
            (dotimes (_ old-close-len) (ghostel-send-key "delete"))
            (ghostel-send-string (cdr new-pair))
            ;; After: cursor is at close-pos + (length new-close)

            ;; Step 2: move to open-pos
            (let ((new-close-len (length (cdr new-pair))))
              (evil-ghostel--send-move (- open-pos (+ close-pos new-close-len))))
            ;; At open-pos: delete old open, insert new open
            (dotimes (_ old-open-len) (ghostel-send-key "delete"))
            (ghostel-send-string (car new-pair))))))))

;;;###autoload
(evil-define-operator evil-ghostel-surround-region (beg end type char)
  "Surround region BEG..END with pair for CHAR (ys, visual S)."
  (interactive
   (append (evil-operator-range t)
           (list (evil-ghostel-surround-read-char))))
  (save-restriction
    (widen)
    (when (and beg end char)
      (let* ((pair (evil-ghostel--surround-pair char))
             (cursor-pos (or (ghostel-cursor-point) (point))))
        ;; Step 1: move to end, insert close delimiter
        (evil-ghostel--send-move (- end cursor-pos))
        (ghostel-send-string (cdr pair))
        ;; After: cursor is at end + (length close). Move to beg.
        (let ((close-len (length (cdr pair))))
          (evil-ghostel--send-move (- beg (+ end close-len))))
        ;; At beg: insert open delimiter
        (ghostel-send-string (car pair))))))

;; ---------------------------------------------------------------------------
;; Surround key bindings — direct normal-state keys (ds, cs, ys)
;; ---------------------------------------------------------------------------
;; ds and cs are bound as direct normal-state key sequences to avoid
;; the fragile operator dispatch mechanism.  ys is bound as an
;; operator via evil-define-operator above.

;;;###autoload
(defun evil-ghostel-ds ()
  "Delete surround — read one char and delete the surrounding pair."
  (interactive)
  (call-interactively #'evil-ghostel-surround-delete))

;;;###autoload
(defun evil-ghostel-cs ()
  "Change surround — read old char then new char and change the pair."
  (interactive)
  (call-interactively #'evil-ghostel-surround-change))



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
    "G" 'evil-ghostel-goto-cursor
    ;; Surround: direct key bindings in normal state
    "ds" 'evil-ghostel-ds
    "cs" 'evil-ghostel-cs
    "ys" 'evil-ghostel-surround-region)

  ;; Visual state bindings
  (evil-define-key 'visual ghostel-mode-map
    "d" 'evil-ghostel-delete
    "x" 'evil-ghostel-delete
    "S" 'evil-ghostel-surround-region))

(with-eval-after-load 'ghostel
  (evil-ghostel-setup))

(provide 'evil-ghostel)
;;; evil-ghostel.el ends here
