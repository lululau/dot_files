;;; markdown-modern.el --- Modern looks for Markdown -*- lexical-binding: t; -*-

;; A port of `org-modern''s visual prettifier to `markdown-mode'.
;;
;; Features (all individually toggleable):
;;   - Tables:      pipe tables get box-drawn vertical rules and thin
;;                  horizontal separators, just like `org-modern'.
;;   - Checkboxes:  GFM `[x]'/`[ ]' are replaced with icon glyphs.
;;   - List bullets: `-', `*', `+' are replaced with typographic bullets.
;;   - Headings:    leading `#' marks are replaced with level indicators.
;;   - Horizontal rules: `---'/`***'/`___' become thin separator lines.
;;
;; Enable per buffer with `markdown-modern-mode', or everywhere with
;; `global-markdown-modern-mode'.

;;; Code:

(require 'markdown-mode)
(require 'cl-lib)

(defgroup markdown-modern nil
  "Modern looks for Markdown."
  :group 'markdown
  :prefix "markdown-modern-")

(defcustom markdown-modern-table-vertical 3
  "Width of vertical table lines in pixels.
Set to nil to disable styling of vertical lines."
  :type '(choice (const :tag "Off" nil) natnum)
  :group 'markdown-modern)

(defcustom markdown-modern-table-horizontal 0.1
  "Prettify horizontal table lines.
A number is used as the relative height of the separator row, which
collapses it into a thin overline rule.  Set to nil to disable."
  :type '(choice (const :tag "Off" nil) number)
  :group 'markdown-modern)

(defcustom markdown-modern-checkbox
  '((?X . "\xf14a")     ; [X] → nf-fa-check_square
    (?x . "\xf14a")     ; [x] → same
    (?- . "\xf0c8")     ; [-] → nf-fa-square (partial)
    (?\s . "\xf096"))   ; [ ] → nf-fa-square_o (empty)
  "Alist mapping checkbox content characters to display replacements.
Each entry is (CHAR . STRING) where CHAR is the character inside the
brackets and STRING is the replacement.  Set to nil to disable."
  :type '(choice (const :tag "Off" nil)
                 (alist :key-type character :value-type string))
  :group 'markdown-modern)

(defcustom markdown-modern-list
  '((?+ . "◦")
    (?- . "–")
    (?* . "•"))
  "Alist mapping list bullet characters to display replacements.
Set to nil to disable styling list bullets."
  :type '(choice (const :tag "Off" nil)
                 (alist :key-type character :value-type string))
  :group 'markdown-modern)

(defcustom markdown-modern-horizontal-rule t
  "Prettify horizontal rules (thematic breaks).
When non-nil, lines like `---', `***', or `___' are rendered as
thin separator lines using `:underline'.  Set to nil to disable."
  :type 'boolean
  :group 'markdown-modern)

(defcustom markdown-modern-heading
  '(?◉ ?○ ?◈ ?◇ ?✳ ?✦)
  "List of characters to replace heading `#' markers.
The Nth element replaces `#' at heading level N.  If the heading
level exceeds the list length, the last element is reused.
Set to nil to disable heading prettification."
  :type '(choice (const :tag "Off" nil)
                 (repeat character))
  :group 'markdown-modern)

(defcustom markdown-modern-heading-hide-leading t
  "When non-nil, hide leading `#' characters in headings.
Only the last `#' is replaced with the level icon; the rest are hidden.
When nil, all `#' characters are replaced with the level icon."
  :type 'boolean
  :group 'markdown-modern)

(defcustom markdown-modern-indent 2
  "Width of left indentation per heading level.
When non-nil, indent headings and body text relative to their level.
For example, if 2, a level 2 heading is indented by 2 spaces and its
body text is indented by 4 spaces.  Set to nil to disable.

Indentation is provided by a separate minor mode,
`markdown-modern-indent-mode', that mirrors the incremental,
idle-time approach used by `org-indent-mode'.  It is toggled
together with `markdown-modern-mode' according to the value of
this user option."
  :type '(choice (const :tag "Off" nil) integer)
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'markdown-modern-indent-mode)
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (when (bound-and-true-p markdown-modern-mode)
                 (markdown-modern-indent-mode
                  (if markdown-modern-indent 1 0)))))))
  :group 'markdown-modern)

(defface markdown-modern--hide '((t :inherit default))
  "Internal face used to hide separators when vertical lines are disabled.
Its foreground is kept in sync with the default background by
`markdown-modern--update-faces'."
  :group 'markdown-modern)

(defface markdown-modern-checkbox-face nil
  "Face used for checkbox icons.
You can specify a font `:family' if the default font does not
contain the checkbox glyphs (e.g., a Nerd Font)."
  :group 'markdown-modern)

(defface markdown-modern-symbol nil
  "Face used for heading icons, list bullets, and other symbols.
You can specify a font `:family' if the default font does not
render certain Unicode characters well."
  :group 'markdown-modern)

(defface markdown-modern-horizontal-rule
  '((((background light)) :underline "gray70" :extend t)
    (t :underline "#484b61" :extend t))
  "Face used for horizontal rules (thematic breaks)."
  :group 'markdown-modern)

(defvar-local markdown-modern--font-lock-keywords nil)
(defvar-local markdown-modern--checkbox-cache nil)
(defvar-local markdown-modern--heading-cache nil)
(defvar-local markdown-modern--table-sp-width 0)
(defconst markdown-modern--table-overline '(:overline t))
(defconst markdown-modern--table-sp
  '((space :width (markdown-modern--table-sp-width))
    (space :width (markdown-modern--table-sp-width))))

(defconst markdown-modern--table-row-regexp "^[ \t]*\\(|.*|\\)[ \t]*$"
  "Regexp matching a pipe-delimited Markdown table row.")

(defconst markdown-modern--table-hline-regexp "^[ \t]*|[-:]"
  "Regexp matching a Markdown table separator (header underline) row.")

(defconst markdown-modern--checkbox-regexp
  "^[ \t]*\\(?:[-*+]\\|[0-9]+[.)]\\)[ \t]+\\(\\[\\([ xX-]\\)\\]\\) "
  "Regexp matching a GFM checkbox in a list item.
Group 1 is the full `[.]' bracket expression.
Group 2 is the single character inside the brackets.")

(defun markdown-modern--checkbox ()
  "Prettify GFM checkboxes according to `markdown-modern-checkbox'."
  (let* ((beg (match-beginning 1))
         (end (match-end 1))
         (ch  (char-after (match-beginning 2)))
         (rep (cdr (assq ch markdown-modern--checkbox-cache))))
    (when rep
      (put-text-property beg end 'display rep)))
  nil)

(defun markdown-modern--make-checkbox-cache ()
  "Build display-string cache from `markdown-modern-checkbox'."
  (mapcar (pcase-lambda (`(,k . ,v))
            (let ((s (if (stringp v) (copy-sequence v) (char-to-string v))))
              (add-face-text-property 0 (length s)
                                     'markdown-modern-checkbox-face 'append s)
              (cons k s)))
          markdown-modern-checkbox))

(defun markdown-modern--symbol (str)
  "Add `markdown-modern-symbol' face to STR."
  (setq str (if (stringp str) (copy-sequence str) (char-to-string str)))
  (add-face-text-property 0 (length str) 'markdown-modern-symbol 'append str)
  str)

(defun markdown-modern--make-heading-cache ()
  "Build display-string cache from `markdown-modern-heading'."
  (cl-loop for ch in markdown-modern-heading
           collect (markdown-modern--symbol ch)))

(defun markdown-modern--heading ()
  "Prettify Markdown heading markers.
Replace leading `#' characters with level indicators from
`markdown-modern-heading'.  When `markdown-modern-heading-hide-leading'
is non-nil, only the last `#' gets the icon and the preceding ones
are hidden."
  (when (get-text-property (match-beginning 0) 'markdown-heading)
    (let* ((beg (match-beginning 1))
           (end (match-end 1))
           (level (- end beg))         ; number of # characters
           (idx (min (1- level) (1- (length markdown-modern--heading-cache))))
           (icon (nth idx markdown-modern--heading-cache)))
      (when icon
        (if markdown-modern-heading-hide-leading
            (progn
              ;; Hide all but the last #
              (when (> level 1)
                (put-text-property beg (1- end) 'invisible 'markdown-modern))
              ;; Replace the last # with the icon
              (put-text-property (1- end) end 'display icon))
          ;; Replace ALL # with a single icon
          (put-text-property beg end 'display icon)))))
  nil)

(defun markdown-modern--indent-properties ()
  "Calculate indentation properties for the matched line.
Retained for backward compatibility only.  Indentation is normally
driven incrementally by `markdown-modern-indent-mode', which is far
cheaper than running this per line through font-lock."
  (save-match-data
    (let ((is-heading (save-excursion
                        (goto-char (match-beginning 0))
                        (and (looking-at "#+ ")
                             (get-text-property (match-beginning 0) 'markdown-heading))))
          (level 0))
      (if is-heading
          (setq level (save-excursion
                        (goto-char (match-beginning 0))
                        (looking-at "\\(#+\\) ")
                        (- (match-end 1) (match-beginning 1))))
        (setq level (save-excursion
                      (goto-char (match-beginning 0))
                      (let ((found nil)
                            (lvl 0))
                        (while (and (not found)
                                    (re-search-backward "^\\(#\\{1,6\\}\\) " nil t))
                          (when (get-text-property (match-beginning 0) 'markdown-heading)
                            (setq lvl (- (match-end 1) (match-beginning 1))
                                  found t)))
                        lvl))))
      (let* ((indent-width (if is-heading
                               (* (1- level) markdown-modern-indent)
                             (* level markdown-modern-indent)))
             (prefix (if (> indent-width 0)
                         (make-string indent-width ?\s)
                       nil)))
        (list 'face nil 'line-prefix prefix 'wrap-prefix prefix)))))

(defun markdown-modern--table ()
  "Prettify the Markdown table row matched by font-lock.
Mirrors `org-modern--table', adapted to Markdown syntax where the
only cell delimiter is \"|\" and separator cells may contain \"-\"
and \":\"."
  (save-excursion
    (let* ((beg (match-beginning 0))
           (end (match-end 0))
           (tbeg (match-beginning 1))
           (tend (match-end 1)))
      ;; Only touch genuine tables, never pipes inside code blocks.
      (when (save-excursion (goto-char beg) (markdown-table-at-point-p))
        (let ((inner (progn
                       (goto-char beg)
                       (forward-line)
                       (re-search-forward markdown-modern--table-row-regexp
                                          (line-end-position) t)))
              (separator (progn
                           (goto-char beg)
                           (re-search-forward markdown-modern--table-hline-regexp
                                              end 'noerror))))
          ;; Vertical lines.
          (goto-char beg)
          (while (search-forward "|" end 'noerror)
            (let ((a (1- (point)))
                  (b (point)))
              (unless (eq (char-before a) ?\\) ; keep escaped \| untouched
                (cond
                 ((and markdown-modern-table-vertical (or (not separator) inner))
                  (add-text-properties
                   a b
                   `(display (space :width (,markdown-modern-table-vertical))
                     face (:inherit markdown-table-face :inverse-video t))))
                 ((and markdown-modern-table-horizontal separator)
                  (put-text-property
                   a b 'display
                   `(space :width (,markdown-modern-table-vertical))))
                 (t (put-text-property a b 'face 'markdown-modern--hide))))))
          ;; Horizontal separator row.
          (goto-char beg)
          (when separator
            (when (numberp markdown-modern-table-horizontal)
              (add-face-text-property tbeg tend markdown-modern--table-overline 'append)
              (add-face-text-property beg (min (1+ end) (point-max))
                                      `(:height ,markdown-modern-table-horizontal)
                                      'append))
            (while (re-search-forward "[^|]+" tend 'noerror)
              (let ((a (match-beginning 0))
                    (b (match-end 0)))
                (cl-loop for i from a below b do
                         (put-text-property
                          i (1+ i) 'display
                          (nth (logand i 1) markdown-modern--table-sp))))))))
      nil)))

(defun markdown-modern--pre-redisplay (_)
  "Compute font parameters before redisplay.
Keep the stretch-glyph width and overline color in sync with the
table font and theme, like `org-modern--pre-redisplay'."
  (let ((face-remapping-alist
         `((default markdown-table-face
            ,@(or (ensure-list (cdr (assq 'default face-remapping-alist)))
                  '(default)))
           ,@face-remapping-alist)))
    (setq markdown-modern--table-sp-width (default-font-width)))
  (setf (cadr markdown-modern--table-overline)
        (face-attribute 'markdown-table-face :foreground nil t)))

(defun markdown-modern--update-faces ()
  "Sync the internal hide face foreground with the default background."
  (set-face-attribute 'markdown-modern--hide nil
                      :foreground (face-attribute 'default :background nil t)))

(defun markdown-modern--unfontify (beg end &optional _loud)
  "Unfontify prettified elements between BEG and END.
Note: `line-prefix'/`wrap-prefix' are intentionally NOT managed by
font-lock; they are owned by `markdown-modern-indent-mode'.  Managing
them here would cause font-lock refontification to wipe the indent
prefixes with nothing to restore them."
  (let ((font-lock-extra-managed-props
         (append '(display invisible)
                 font-lock-extra-managed-props)))
    (font-lock-default-unfontify-region beg end)))

(defun markdown-modern--make-font-lock-keywords ()
  "Return font-lock keywords."
  `(;; List bullets
    ,@(when markdown-modern-list
        (cl-loop for (ch . rep) in markdown-modern-list
                 collect
                 (let ((sym (markdown-modern--symbol rep)))
                   (if (eq ch ?*)
                       ;; `*' as list bullet requires leading whitespace
                       ;; to avoid clashing with bold `**text**'.
                       `("^\\([ \t]+\\)\\(\\*\\)[ \t]" 2 '(face nil display ,sym))
                     `(,(format "^[ \t]*\\(\\%c\\)[ \t]" ch)
                       1 '(face nil display ,sym))))))
    ;; Checkboxes
    ,@(when markdown-modern-checkbox
        `((,markdown-modern--checkbox-regexp
           (1 (markdown-modern--checkbox) prepend t))))
    ;; Headings
    ,@(when markdown-modern-heading
        `(("^\\(#\\{1,6\\}\\) "
           (1 (markdown-modern--heading) prepend))))
    ;; Horizontal rules (thematic breaks): ---, ***, ___
    ,@(when markdown-modern-horizontal-rule
        '(("\\(^[ \t]*[-*_]\\{3,\\}\\)[ \t]*\r?\n"
           (1 '(face nil display " "))
           (0 '(face markdown-modern-horizontal-rule) prepend))))
    ;; NOTE: Per-line indentation used to live here as a font-lock
    ;; keyword that ran `markdown-modern--indent-properties' on every
    ;; line (and did a `re-search-backward' for each).  That was the
    ;; main source of editing latency.  Indentation is now provided
    ;; incrementally by `markdown-modern-indent-mode'; see the bottom
    ;; of this file.
    ;; Tables (must come last — expensive)
    (,markdown-modern--table-row-regexp (0 (markdown-modern--table)))))

;;;###autoload
(define-minor-mode markdown-modern-mode
  "Modern looks for Markdown (org-modern style).

Prettifies tables, checkboxes, list bullets, headings, and
horizontal rules.  Each feature is individually configurable.

When `markdown-modern-indent' is non-nil, also enables
`markdown-modern-indent-mode' for dynamic (org-indent style)
indentation relative to heading level."
  :group 'markdown-modern
  (let ((kw (markdown-modern--make-font-lock-keywords)))
    (cond
     (markdown-modern-mode
      (add-to-invisibility-spec 'markdown-modern)
      (setq markdown-modern--checkbox-cache (markdown-modern--make-checkbox-cache))
      (setq markdown-modern--heading-cache (markdown-modern--make-heading-cache))
      (setq markdown-modern--font-lock-keywords kw)
      (font-lock-add-keywords nil kw 'append)
      (setq-local font-lock-unfontify-region-function #'markdown-modern--unfontify)
      (add-hook 'pre-redisplay-functions #'markdown-modern--pre-redisplay nil 'local)
      (markdown-modern--update-faces))
     (t
      ;; Disable indent mode first so its properties are cleared by
      ;; its own teardown, not by the font-lock unfontify below.
      (when (and (boundp 'markdown-modern-indent-mode)
                 markdown-modern-indent-mode)
        (markdown-modern-indent-mode 0))
      (remove-from-invisibility-spec 'markdown-modern)
      (font-lock-remove-keywords nil markdown-modern--font-lock-keywords)
      (setq-local font-lock-unfontify-region-function #'font-lock-default-unfontify-region)
      (remove-hook 'pre-redisplay-functions #'markdown-modern--pre-redisplay 'local)))
    (with-silent-modifications
      (markdown-modern--unfontify (point-min) (point-max)))
    (font-lock-flush)
    ;; Enable indent mode LAST, after unfontify/font-lock-flush, so
    ;; its line-prefix/wrap-prefix survive (they are not managed by
    ;; font-lock and would otherwise be wiped by the unfontify above).
    (when (and markdown-modern-mode markdown-modern-indent
               (fboundp 'markdown-modern-indent-mode))
      (markdown-modern-indent-mode
       (if markdown-modern-mode 1 0)))))

(defun markdown-modern--on ()
  "Enable `markdown-modern-mode' in Markdown buffers."
  (when (derived-mode-p 'markdown-mode)
    (markdown-modern-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-markdown-modern-mode
  markdown-modern-mode markdown-modern--on
  :group 'markdown-modern)


;;; Dynamic indentation (port of `org-indent-mode' semantics)
;;
;; `markdown-modern-indent-mode' adds `line-prefix' and `wrap-prefix'
;; text properties so that body text is visually indented relative to
;; its enclosing heading level, like `org-indent-mode'.  Unlike the
;; previous font-lock based implementation, it:
;;
;;   - updates only the changed region on `after-change-functions'
;;     (full re-fontification of the current section only when a
;;     heading itself was edited);
;;   - completes the initial pass over large buffers during idle time
;;     via an "agent" timer, so opening a big file never blocks;
;;   - computes the level incrementally with a single forward scan,
;;     never `re-search-backward' from each line.
;;
;; Prefixes are cached per level in vectors, exactly like org-indent.

(defconst markdown-modern-indent--deepest-level 8
  "Maximum heading level tracked by `markdown-modern-indent-mode'.")

(defvar-local markdown-modern-indent--heading-prefixes nil
  "Vector of cached `line-prefix' strings for heading lines, by level.")

(defvar-local markdown-modern-indent--text-prefixes nil
  "Vector of cached `line-prefix' strings for body lines, by level.")

(defvar-local markdown-modern-indent--wrap-prefixes nil
  "Vector of cached `wrap-prefix' strings by level.")

(defvar markdown-modern-indent-mode nil
  "Non-nil when `markdown-modern-indent-mode' is enabled in a buffer.
Declared here so the before/after-change helpers can cheaply test it
without requiring the `define-minor-mode' form to be evaluated first.")

(defvar-local markdown-modern-indent--modified-heading-flag nil
  "Non-nil if the pending change touches a heading line.
Mirrors `org-indent-modified-headline-flag'.")

(defvar-local markdown-modern-indent--initial-marker nil
  "Marker tracking how far the idle agent has progressed in this buffer.")

(defvar markdown-modern-indent--agent-timer nil
  "Idle timer driving `markdown-modern-indent--agent'.")

(defvar markdown-modern-indent--agentized-buffers nil
  "List of buffers still being initialized by the indent agent.")

(defconst markdown-modern-indent--active-delay '(0 2 0)
  "Idle slice used by the agent when its buffer is current.
See `org-indent-agent-active-delay'.")

(defconst markdown-modern-indent--passive-delay '(0 0 400000)
  "Idle slice used by the agent when its buffer is not current.
See `org-indent-agent-passive-delay'.")

(defconst markdown-modern-indent--resume-delay '(0 0 100000)
  "Idle pause left to other timers between agent slices.
See `org-indent-agent-resume-delay'.")

(defconst markdown-modern-indent--heading-regexp
  "^\\(#\\{1,6\\}\\)\\(?:[ \t]+\\|$\\)"
  "Regexp matching an ATX heading line, group 1 = the hashes.")

(defconst markdown-modern-indent--fenced-open-regexp
  "^[ \t]*\\(?:`\\{3,\\}\\|~\\{3,\\}\\)"
  "Regexp matching the opening fence of a GFM/tilde code block.")

(defun markdown-modern-indent--compute-prefixes ()
  "Precompute per-level prefix strings for the current buffer.
Populates the three vectors used by `markdown-modern-indent--add'."
  (let ((w (max 0 (or markdown-modern-indent 0))))
    (setq markdown-modern-indent--heading-prefixes
          (make-vector (1+ markdown-modern-indent--deepest-level) nil))
    (setq markdown-modern-indent--text-prefixes
          (make-vector (1+ markdown-modern-indent--deepest-level) nil))
    (setq markdown-modern-indent--wrap-prefixes
          (make-vector (1+ markdown-modern-indent--deepest-level) nil))
    (dotimes (n (1+ markdown-modern-indent--deepest-level))
      ;; A level-N heading is indented (N-1)*w (so level-1 sits at the
      ;; margin); its body sits one level deeper, at N*w.
      (let* ((head-indent (max 0 (* w (max 0 (1- n)))))
             (body-indent (* w n))
             (head-prefix (if (> head-indent 0)
                              (make-string head-indent ?\s)
                            nil))
             (text-prefix (if (> body-indent 0)
                              (make-string body-indent ?\s)
                            nil)))
        (aset markdown-modern-indent--heading-prefixes n head-prefix)
        (aset markdown-modern-indent--text-prefixes n text-prefix)
        (aset markdown-modern-indent--wrap-prefixes n text-prefix)))))

(defun markdown-modern-indent--remove-properties (beg end)
  "Remove `line-prefix'/`wrap-prefix' between BEG and END."
  (with-silent-modifications
    (remove-text-properties beg end '(line-prefix nil wrap-prefix nil))))

(defun markdown-modern-indent--level-at (pos)
  "Return the ATX heading level at POS, or nil if POS is not on a heading."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (when (looking-at markdown-modern-indent--heading-regexp)
      (- (match-end 1) (match-beginning 1)))))

(defconst markdown-modern-indent--fence-regexp
  "^[ \t]*\\(`\\{3,\\}\\|~\\{3,\\}\\)"
  "Regexp matching a fenced code block delimiter line (open or close).")

(defun markdown-modern-indent--code-state-at (pos)
  "Return non-nil if the line at POS is inside a fenced code block.
Counts fence delimiter lines from `point-min' up to POS.  This is
independent of font-lock, so it works before the buffer is fontified
and inside the idle agent.  O(n) but only called for seeding."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((n 0)
            (limit (save-excursion (goto-char pos) (line-end-position))))
        (while (re-search-forward markdown-modern-indent--fence-regexp limit t)
          (cl-incf n))
        (cl-oddp n)))))

(defun markdown-modern-indent--fence-line-p ()
  "Return non-nil if the current line is a fenced code delimiter."
  (save-excursion
    (forward-line 0)
    (looking-at-p markdown-modern-indent--fence-regexp)))

(defun markdown-modern-indent--seed-level (pos)
  "Return the heading level in effect at POS.
Scans backward from POS for the nearest ATX heading and returns its
level (1..6), or 0 if there is none.  A single bounded search; no
per-line loop."
  (save-excursion
    (save-match-data
      (goto-char pos)
      (forward-line 0)
      ;; If POS itself is a heading, its level is the answer.
      (or (markdown-modern-indent--level-at (point))
          (progn
            ;; One cheap backward search; bounded by bob.
            (if (re-search-backward markdown-modern-indent--heading-regexp
                                    nil t)
                (- (match-end 1) (match-beginning 1))
              0))))))

(defun markdown-modern-indent--add (beg end &optional delay)
  "Add indent properties between BEG and END.

When DELAY (a time value) is given, the pass is interruptible: it
yields after DELAY and on pending input, returning the position
where it should resume.  Otherwise it runs to completion.

BEG is normalized to a line start.  The initial heading level is
seeded once with `markdown-modern-indent--seed-level'; inside the
main loop the level is advanced monotonically as headings are
encountered, so there is no per-line backward search.

Code-block membership is tracked by a fence-flipping state machine
fed from `markdown-modern-indent--code-state-at' as the seed, then
toggled on each delimiter line.  This is independent of font-lock
so it works before the buffer is fontified.

Code blocks are indented like ordinary body text at the current
heading level (matching `org-indent-mode').  Their only special
property is that `#' lines inside are never treated as headings."
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (goto-char beg)
        (forward-line 0)
        (let* ((line-beg (point))
               (level (markdown-modern-indent--seed-level beg))
               (in-code (markdown-modern-indent--code-state-at line-beg))
               (time-limit (and delay (time-add nil delay)))
               (line-end (lambda ()
                           (min (line-beginning-position 2) (point-max)))))
          (with-silent-modifications
            (while (and (< line-beg end)
                        (not (eobp)))
              (cond
               ((and delay (input-pending-p))
                (throw 'markdown-modern-indent--interrupt line-beg))
               ((and delay (time-less-p time-limit nil))
                (throw 'markdown-modern-indent--interrupt line-beg))
               (t
                (let* ((fence (looking-at markdown-modern-indent--fence-regexp))
                       ;; `#' is a heading only outside code blocks.
                       (here-level (and (not in-code)
                                        (markdown-modern-indent--level-at line-beg)))
                       (cur (or here-level level))
                       (idx (min cur markdown-modern-indent--deepest-level))
                       ;; Both headings and code/body lines use the
                       ;; heading prefix when the line itself is a
                       ;; heading; otherwise the text prefix.
                       (prefix
                        (if here-level
                            (aref markdown-modern-indent--heading-prefixes idx)
                          (aref markdown-modern-indent--text-prefixes idx)))
                       (wrap (aref markdown-modern-indent--wrap-prefixes idx)))
                  (add-text-properties line-beg (funcall line-end)
                                       `(line-prefix ,prefix wrap-prefix ,wrap))
                  (when here-level
                    (setq level here-level))
                  ;; Flip code state AFTER assigning the prefix: a
                  ;; fence delimiter line belongs to the block it
                  ;; opens/closes, so it shares that block's level.
                  (when fence
                    (setq in-code (not in-code))))))
              (forward-line 1)
              (setq line-beg (point)))))))))

(defun markdown-modern-indent--notify (beg _end)
  "Set the modified-heading flag if BEG..END overlaps a heading.
Attached to `before-change-functions'."
  (when markdown-modern-indent-mode
    (setq markdown-modern-indent--modified-heading-flag
          (save-excursion
            (save-match-data
              (goto-char beg)
              (or (and (looking-at markdown-modern-indent--heading-regexp)
                       (< beg (line-end-position)))
                  (markdown-modern-indent--level-at beg)))))))

(defun markdown-modern-indent--refresh (beg end _length)
  "Refresh indent properties after a change in BEG..END.
Attached to `after-change-functions'."
  (when markdown-modern-indent-mode
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (if markdown-modern-indent--modified-heading-flag
              ;; A heading was edited: re-indent from this heading's
              ;; start through the next heading, because the level
              ;; shift cascades to the whole subsection.
              (let* ((head-beg
                      (progn (goto-char beg) (forward-line 0) (point)))
                     (head-end
                      (progn
                        (goto-char end)
                        (if (re-search-forward
                             markdown-modern-indent--heading-regexp nil t)
                            (line-beginning-position)
                          (point-max)))))
                (setq markdown-modern-indent--modified-heading-flag nil)
                (markdown-modern-indent--add head-beg head-end))
            ;; Plain text change: only the touched lines need refresh.
            (markdown-modern-indent--add
             (progn (goto-char beg) (forward-line 0) (point))
             (progn (goto-char end) (line-end-position)))))))))

(defun markdown-modern-indent--agent ()
  "Advance initialization of agentized buffers.
Mirrors `org-indent-initialize-agent'."
  (when markdown-modern-indent--agentized-buffers
    (setq markdown-modern-indent--agentized-buffers
          (cl-remove-if-not #'buffer-live-p
                            markdown-modern-indent--agentized-buffers)))
  (cond
   ((null markdown-modern-indent--agentized-buffers)
    (when markdown-modern-indent--agent-timer
      (cancel-timer markdown-modern-indent--agent-timer)
      (setq markdown-modern-indent--agent-timer nil)))
   ((memq (current-buffer) markdown-modern-indent--agentized-buffers)
    (markdown-modern-indent--initialize-buffer
     (current-buffer) markdown-modern-indent--active-delay))
   (t
    (markdown-modern-indent--initialize-buffer
     (car (last markdown-modern-indent--agentized-buffers))
     markdown-modern-indent--passive-delay))))

(defun markdown-modern-indent--initialize-buffer (buffer delay)
  "Continue indenting BUFFER asynchronously, yielding after DELAY."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when markdown-modern-indent-mode
        (save-excursion
          (save-restriction
            (widen)
            (let ((start
                   (or (and (markerp markdown-modern-indent--initial-marker)
                            (marker-position markdown-modern-indent--initial-marker))
                       (point-min))))
              (let ((resume
                     (catch 'markdown-modern-indent--interrupt
                       (markdown-modern-indent--add start (point-max) delay)
                       nil)))
                (if resume
                    (move-marker markdown-modern-indent--initial-marker resume)
                  (when (markerp markdown-modern-indent--initial-marker)
                    (set-marker markdown-modern-indent--initial-marker nil))
                  (setq markdown-modern-indent--agentized-buffers
                        (delq buffer
                              markdown-modern-indent--agentized-buffers))
                  (when (null markdown-modern-indent--agentized-buffers)
                    (when markdown-modern-indent--agent-timer
                      (cancel-timer markdown-modern-indent--agent-timer)
                      (setq markdown-modern-indent--agent-timer nil))))))))))))

(defun markdown-modern-indent--bootstrap ()
  "Schedule the idle agent to finish indenting the current buffer."
  (setq markdown-modern-indent--initial-marker (copy-marker (point-min) t))
  (cl-pushnew (current-buffer) markdown-modern-indent--agentized-buffers)
  (unless markdown-modern-indent--agent-timer
    (setq markdown-modern-indent--agent-timer
          (run-with-idle-timer 0.2 t #'markdown-modern-indent--agent))))

;;;###autoload
(define-minor-mode markdown-modern-indent-mode
  "Dynamic virtual indentation for `markdown-mode', à la `org-indent-mode'.

Body text and headings are indented relative to their enclosing
heading level via `line-prefix' and `wrap-prefix' text properties.

Updates are incremental (driven by `after-change-functions') and
the initial pass over large buffers runs during idle time, so
editing latency is not affected.  This replaces the previous
font-lock based indentation, which rescanned the buffer on every
redisplay and caused noticeable lag."
  :lighter " MInd"
  :group 'markdown-modern
  (cond
   (markdown-modern-indent-mode
    (unless markdown-modern-indent
      (setq markdown-modern-indent 2))
    (markdown-modern-indent--compute-prefixes)
    (add-hook 'before-change-functions
              #'markdown-modern-indent--notify nil 'local)
    (add-hook 'after-change-functions
              #'markdown-modern-indent--refresh nil 'local)
    (markdown-modern-indent--remove-properties (point-min) (point-max))
    ;; Small/medium buffers: indent synchronously (a few ms).  Only
    ;; very large buffers defer to the idle agent to avoid a startup
    ;; hiccup; `after-change-functions' keeps everything current
    ;; afterwards regardless.
    (if (< (buffer-size) 50000)
        (markdown-modern-indent--add (point-min) (point-max))
      (markdown-modern-indent--bootstrap)))
   (t
    (remove-hook 'before-change-functions
                 #'markdown-modern-indent--notify 'local)
    (remove-hook 'after-change-functions
                 #'markdown-modern-indent--refresh 'local)
    (setq markdown-modern-indent--agentized-buffers
          (delq (current-buffer) markdown-modern-indent--agentized-buffers))
    (when (markerp markdown-modern-indent--initial-marker)
      (set-marker markdown-modern-indent--initial-marker nil))
    (markdown-modern-indent--remove-properties (point-min) (point-max))
    (font-lock-flush))))

(defun markdown-modern-indent-fontify-buffer ()
  "Synchronously indent the whole buffer, bypassing the idle agent.
Useful after bulk operations (e.g. `markdown-outline-cycle-all')."
  (interactive)
  (markdown-modern-indent--remove-properties (point-min) (point-max))
  (markdown-modern-indent--add (point-min) (point-max)))

(provide 'markdown-modern)
;;; markdown-modern.el ends here
