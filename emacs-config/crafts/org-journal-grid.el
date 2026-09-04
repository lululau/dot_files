;;; org-journal-grid.el --- Read-only SVG grid for org-journal -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Liu Xiang
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; List org-journal headings on an SVG time grid.  Rendering is adapted
;; from org-timegrid (https://github.com/Gleek/org-timegrid).

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'calendar)
(require 'seq)
(require 'subr-x)
(require 'org-journal-grid-model)
(require 'org-journal-grid-render)

(defgroup org-journal-grid nil
  "Read-only SVG time grid for org-journal files."
  :group 'org
  :prefix "org-journal-grid-")

(defcustom org-journal-grid-directory nil
  "Journal directory.
Nil means use `org-journal-dir' when bound, otherwise
\"~/Documents/materials/journal/\"."
  :type '(choice (const :tag "Follow org-journal-dir" nil)
                 directory))

(defcustom org-journal-grid-show-todo nil
  "When non-nil, include unfinished TODO headings on the grid."
  :type 'boolean)

(defcustom org-journal-grid-default-duration-minutes 30
  "Duration in minutes for a heading that has only a start time."
  :type 'integer)

(defcustom org-journal-grid-tag-color-alist nil
  "Alist of Org tag strings to calendar colours."
  :type '(alist :key-type string :value-type (choice symbol color)))

(defun org-journal-grid--resolved-directory ()
  "Return the journal directory to scan."
  (expand-file-name
   (or org-journal-grid-directory
       (and (boundp 'org-journal-dir) org-journal-dir)
       "~/Documents/materials/journal/")))

(defun org-journal-grid--parse-clock (title)
  "Return minute-of-day at the start of TITLE, or nil."
  (and (stringp title)
       (string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\(?: \\|$\\)" title)
       (let ((hour (string-to-number (match-string 1 title)))
             (minute (string-to-number (match-string 2 title))))
         (and (<= 0 hour 23) (<= 0 minute 59)
              (+ (* hour 60) minute)))))

(defun org-journal-grid--display-title (title)
  "Strip a leading HH:MM from TITLE for block text."
  (if (and (stringp title)
           (string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\(?: \\|$\\)" title)
           (org-journal-grid--parse-clock title))
      (let ((rest (substring title (match-end 0))))
        (if (string-empty-p rest) title rest))
    title))

(defun org-journal-grid--include-todo-p (todo-keyword)
  "Return non-nil when TODO-KEYWORD should appear on the grid."
  (or org-journal-grid-show-todo
      (null todo-keyword)
      (not (member todo-keyword org-not-done-keywords))))

(defun org-journal-grid--clamp-end (start duration)
  "Return exclusive end minute for START plus DURATION, clamped to midnight."
  (let ((day-end (* (1+ (floor start 1440)) 1440)))
    (min (+ start duration) day-end)))

(defun org-journal-grid--file-name (absolute-date)
  "Return the YYYY-MM-DD basename for ABSOLUTE-DATE."
  (let ((date (calendar-gregorian-from-absolute absolute-date)))
    (format "%04d-%02d-%02d" (nth 2 date) (nth 0 date) (nth 1 date))))

(defun org-journal-grid-toggle-todo ()
  "Buffer-locally toggle display of unfinished TODO headings."
  (interactive)
  (setq-local org-journal-grid-show-todo (not org-journal-grid-show-todo))
  (when (fboundp 'org-journal-grid--refresh-data)
    (org-journal-grid--refresh-data))
  (message "TODO entries %s"
           (if org-journal-grid-show-todo "shown" "hidden")))

(defconst org-journal-grid-max-adjust-days 9
  "Maximum day count set from the grid `+' / `-' / digit keys.")

(defun org-journal-grid-set-days (n)
  "Set the visible day count to N, keeping the right-edge date.
N is clamped to 1 through `org-journal-grid-max-adjust-days'.
The value is buffer-local and is not written to Customize."
  (unless (derived-mode-p 'org-journal-grid-mode)
    (user-error "Not in a journal grid"))
  (let* ((old org-journal-grid-days)
         (new (min org-journal-grid-max-adjust-days (max 1 n)))
         (week-start (org-journal-grid--calendar-state-week-start
                      org-journal-grid--state)))
    (if (= new old)
        (message "Showing last %d day%s" new (if (= new 1) "" "s"))
      (setq-local org-journal-grid-days new)
      (org-journal-grid--reload-state
       (org-journal-grid--range-start-keeping-end week-start old new))
      (when-let* ((cursor (org-journal-grid--calendar-state-cursor
                           org-journal-grid--state)))
        (setf (org-journal-grid--cursor-state-day cursor)
              (min (org-journal-grid--cursor-state-day cursor)
                   (1- new))))
      (org-journal-grid--refresh t)
      (message "Showing last %d day%s" new (if (= new 1) "" "s")))))

(defun org-journal-grid-adjust-days (delta)
  "Change the visible day count by DELTA, keeping the right-edge date."
  (org-journal-grid-set-days (+ org-journal-grid-days delta)))

(defun org-journal-grid-increase-days (&optional n)
  "Show one more trailing day, or set the count to prefix N (1-9)."
  (interactive "P")
  (if n
      (org-journal-grid-set-days (prefix-numeric-value n))
    (org-journal-grid-adjust-days 1)))

(defun org-journal-grid-decrease-days (&optional n)
  "Show one fewer trailing day, or set the count to prefix N (1-9)."
  (interactive "P")
  (if n
      (org-journal-grid-set-days (prefix-numeric-value n))
    (org-journal-grid-adjust-days -1)))

(defun org-journal-grid--tag-color (tags)
  "Return the colour for the first TAGS member in the colour alist."
  (seq-some (lambda (tag)
              (cdr (assoc tag org-journal-grid-tag-color-alist)))
            tags))

(defun org-journal-grid--headline-event (file headline absolute-date)
  "Build an event from FILE HEADLINE on ABSOLUTE-DATE, or nil."
  (when (= (org-element-property :level headline) 2)
    (let* ((todo (org-element-property :todo-keyword headline))
           (raw (org-element-property :raw-value headline))
           (clock (org-journal-grid--parse-clock raw)))
      (when (and clock (org-journal-grid--include-todo-p todo))
        (let* ((begin (org-element-property :begin headline))
               (start (+ (* absolute-date 1440) clock))
               (end (org-journal-grid--clamp-end
                     start org-journal-grid-default-duration-minutes))
               (tags (org-element-property :tags headline)))
          (org-journal-grid-event-create
           :id (format "%s:%s" file begin)
           :title (org-journal-grid--display-title raw)
           :start start
           :end end
           :all-day nil
           :tags tags
           :state nil
           :color (org-journal-grid--tag-color tags)
           :source (list :file file :position begin)))))))

(defun org-journal-grid--parse-org-buffer (file absolute-date)
  "Parse the current org buffer (widened) for FILE on ABSOLUTE-DATE."
  (save-restriction
    (widen)
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (headline)
        (org-journal-grid--headline-event file headline absolute-date)))))

(defun org-journal-grid--parse-org-string (file absolute-date string)
  "Parse STRING as org for FILE on ABSOLUTE-DATE."
  (with-temp-buffer
    (insert string)
    (delay-mode-hooks (org-mode))
    (org-journal-grid--parse-org-buffer file absolute-date)))

(defun org-journal-grid--parse-file (file absolute-date)
  "Return events from FILE for ABSOLUTE-DATE.
Prefer unsaved text from a visiting buffer.  Parse under org-mode with
the restriction widened so a narrowed buffer still lists every heading.
A visiting buffer that is not org-mode (or a derivative such as
org-journal-mode) is copied into a temporary org-mode buffer."
  (if-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf
        (if (derived-mode-p 'org-mode 'org-journal-mode)
            (org-journal-grid--parse-org-buffer file absolute-date)
          (org-journal-grid--parse-org-string
           file absolute-date
           (save-restriction
             (widen)
             (buffer-string)))))
    (with-temp-buffer
      (insert-file-contents file)
      (delay-mode-hooks (org-mode))
      (org-journal-grid--parse-org-buffer file absolute-date))))

(defun org-journal-grid--events-for-day (absolute-date)
  "Return events for ABSOLUTE-DATE, or nil if the file is missing.
A corrupt file that still passes `file-readable-p' yields nil (empty
column) instead of aborting `org-journal-grid--list-events'."
  (let* ((dir (org-journal-grid--resolved-directory))
         (file (expand-file-name (org-journal-grid--file-name absolute-date) dir)))
    (when (and (file-regular-p file) (file-readable-p file))
      (condition-case err
          (delq nil (org-journal-grid--parse-file file absolute-date))
        (error
         (message "org-journal-grid: skipping %s (%s)"
                  file (error-message-string err))
         nil)))))

(defun org-journal-grid--list-events (start end)
  "Return journal events intersecting START and END (absolute minutes)."
  (let ((show-todo org-journal-grid-show-todo)
        (start-day (floor start 1440))
        (end-day (floor (1- end) 1440))
        events)
    ;; `t' does `setq-local'.  A self-let of a buffer-local custom stays in
    ;; this buffer, so parse would see the global nil.  Bind it in a buffer
    ;; without a local value so journal/temp buffers inherit the default.
    (with-temp-buffer
      (let ((org-journal-grid-show-todo show-todo))
        (cl-loop for day from start-day to end-day
                 do (setq events (nconc events (org-journal-grid--events-for-day day))))
        events))))

(defun org-journal-grid--visit (event)
  "Jump to EVENT's org-journal heading."
  (let* ((source (org-journal-grid-event-source event))
         (file (plist-get source :file))
         (position (plist-get source :position)))
    (unless (and file (file-exists-p file))
      (user-error "Journal file disappeared: %s" file))
    (find-file file)
    (widen)
    (unless (and position
                 (>= position (point-min))
                 (<= position (point-max)))
      (user-error "Journal heading disappeared: %s" file))
    (goto-char position)
    (org-fold-show-context 'org-goto)))

(defun org-journal-grid--read-date (absolute-start _duration)
  "Read a date with `org-read-date' and return start minutes.
ABSOLUTE-START prefills the prompt.  The result is a cons of midnight
absolute minutes and a nil duration, matching
`org-journal-grid-read-timestamp-default'."
  (let* ((day (floor absolute-start 1440))
         (date (calendar-gregorian-from-absolute day))
         (default-time (encode-time 0 0 12
                                    (nth 1 date) (nth 0 date) (nth 2 date)))
         (time (org-read-date nil t nil nil default-time))
         (decoded (decode-time time))
         (absolute (calendar-absolute-from-gregorian
                    (list (nth 4 decoded) (nth 3 decoded) (nth 5 decoded)))))
    (cons (* absolute 1440) nil)))

(defvar org-journal-grid-backend
  (org-journal-grid-backend-create
   :name "org-journal"
   :list-function #'org-journal-grid--list-events
   :visit-function #'org-journal-grid--visit)
  "Read-only backend for `org-journal-grid'.")

(setf (org-journal-grid-backend-read-timestamp-function org-journal-grid-backend)
      #'org-journal-grid--read-date)

(defun org-journal-grid (&optional days)
  "Open a read-only journal time grid.
Without a prefix, show `org-journal-grid-days' ending today.
A numeric prefix DAYS overrides the width for this buffer only."
  (interactive "P")
  (unless (image-type-available-p 'svg)
    (user-error "org-journal-grid requires Emacs SVG support"))
  (let* ((width (if days (prefix-numeric-value days) org-journal-grid-days))
         (dir (org-journal-grid--resolved-directory)))
    (unless (and (integerp width) (> width 0))
      (user-error "org-journal-grid-days must be a positive integer"))
    (unless (file-directory-p dir)
      (user-error "Journal directory does not exist: %s" dir))
    (when-let* ((buf (get-buffer org-journal-grid-buffer-name)))
      (with-current-buffer buf
        (setq-local org-journal-grid-days width)
        (setq-local org-journal-grid--stale t)))
    (let ((org-journal-grid-days width))
      (org-journal-grid-open org-journal-grid-backend)
      (when-let* ((buf (get-buffer org-journal-grid-buffer-name)))
        (with-current-buffer buf
          (setq-local org-journal-grid-days width))))))

(defun org-journal-grid--enable-tooltips ()
  "Show full heading titles as GUI tooltips when hovering a block."
  (when (and (display-graphic-p) (not tooltip-mode))
    (tooltip-mode 1)))

(add-hook 'org-journal-grid-mode-hook #'org-journal-grid--enable-tooltips)

(with-eval-after-load 'evil
  (evil-set-initial-state 'org-journal-grid-mode 'emacs))

(provide 'org-journal-grid)
;;; org-journal-grid.el ends here
