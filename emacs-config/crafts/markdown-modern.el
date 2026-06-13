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
When a number, indent headings and body text relative to their level.
For example, if 2, a level 2 heading is indented by 2 spaces and its
body text is indented by 4 spaces.  Set to nil to disable."
  :type '(choice (const :tag "Off" nil) integer)
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
  "Calculate indentation properties for the matched line."
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
                               0
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
  "Unfontify prettified elements between BEG and END."
  (let ((font-lock-extra-managed-props
         (append '(display invisible line-prefix wrap-prefix)
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
    ;; Indentation
    ,@(when markdown-modern-indent
        '(("\\(?:^.*\n\\|.+\\$\\)"
           (0 (markdown-modern--indent-properties)))))
    ;; Tables (must come last — expensive)
    (,markdown-modern--table-row-regexp (0 (markdown-modern--table)))))

;;;###autoload
(define-minor-mode markdown-modern-mode
  "Modern looks for Markdown (org-modern style).

Prettifies tables, checkboxes, list bullets, headings, and
horizontal rules.  Each feature is individually configurable."
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
      (remove-from-invisibility-spec 'markdown-modern)
      (font-lock-remove-keywords nil markdown-modern--font-lock-keywords)
      (setq-local font-lock-unfontify-region-function #'font-lock-default-unfontify-region)
      (remove-hook 'pre-redisplay-functions #'markdown-modern--pre-redisplay 'local)))
    (with-silent-modifications
      (markdown-modern--unfontify (point-min) (point-max)))
    (font-lock-flush)))

(defun markdown-modern--on ()
  "Enable `markdown-modern-mode' in Markdown buffers."
  (when (derived-mode-p 'markdown-mode)
    (markdown-modern-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-markdown-modern-mode
  markdown-modern-mode markdown-modern--on
  :group 'markdown-modern)

(provide 'markdown-modern)
;;; markdown-modern.el ends here
