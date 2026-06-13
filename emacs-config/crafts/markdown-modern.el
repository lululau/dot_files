;;; markdown-modern.el --- Modern looks for Markdown -*- lexical-binding: t; -*-

;; A focused port of `org-modern''s table prettifier to `markdown-mode'.
;;
;; It turns ASCII pipe tables such as
;;
;;   | A | B |
;;   |---|---|
;;   | 1 | 2 |
;;
;; into box-drawn tables with solid vertical rules and a thin horizontal
;; separator, mirroring how `org-modern' renders Org tables.
;;
;; Mechanism (identical idea to `org-modern--table'):
;;   - Each cell separator "|" is replaced by a fixed-width stretch glyph
;;     painted with `:inverse-video', i.e. a solid bar in the foreground
;;     color of `markdown-table-face'.
;;   - Separator rows ("|---|", "|:--:|", ...) get an `:overline' plus a
;;     reduced `:height', collapsing the row into a thin horizontal rule,
;;     while their dashes/colons become font-width spaces so columns stay
;;     aligned.
;;
;; Enable per buffer with `markdown-modern-mode', or everywhere with
;; `global-markdown-modern-mode'.

;;; Code:

(require 'markdown-mode)
(require 'cl-lib)

(defgroup markdown-modern nil
  "Modern looks for Markdown tables and checkboxes."
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

(defface markdown-modern--hide '((t :inherit default))
  "Internal face used to hide separators when vertical lines are disabled.
Its foreground is kept in sync with the default background by
`markdown-modern--update-faces'."
  :group 'markdown-modern)

(defface markdown-modern-checkbox-face nil
  "Face used for checkbox icons.
You can specify a font `:family' if the default font does not
contain the checkbox glyphs (e.g., a Nerd Font).  Inherits from
`org-modern-symbol' when available, so it stays in sync."
  :group 'markdown-modern)

(defvar-local markdown-modern--font-lock-keywords nil)
(defvar-local markdown-modern--checkbox-cache nil)
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
  "Unfontify prettified table elements between BEG and END."
  (let ((font-lock-extra-managed-props
         (append '(display invisible) font-lock-extra-managed-props)))
    (font-lock-default-unfontify-region beg end)))

(defun markdown-modern--make-font-lock-keywords ()
  "Return font-lock keywords for prettifying Markdown tables and checkboxes."
  `(,@(when markdown-modern-checkbox
        `((,markdown-modern--checkbox-regexp
           (1 (markdown-modern--checkbox) prepend t))))
    (,markdown-modern--table-row-regexp (0 (markdown-modern--table)))))

;;;###autoload
(define-minor-mode markdown-modern-mode
  "Modern looks for Markdown tables (org-modern style)."
  :group 'markdown-modern
  (let ((kw (markdown-modern--make-font-lock-keywords)))
    (cond
     (markdown-modern-mode
      (setq markdown-modern--checkbox-cache (markdown-modern--make-checkbox-cache))
      (setq markdown-modern--font-lock-keywords kw)
      (font-lock-add-keywords nil kw 'append)
      (setq-local font-lock-unfontify-region-function #'markdown-modern--unfontify)
      (add-hook 'pre-redisplay-functions #'markdown-modern--pre-redisplay nil 'local)
      (markdown-modern--update-faces))
     (t
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
