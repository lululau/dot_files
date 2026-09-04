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

(defun org-journal-grid--parse-file (file absolute-date)
  "Return events from FILE for ABSOLUTE-DATE."
  (let ((parse
         (lambda ()
           (org-element-map (org-element-parse-buffer 'headline) 'headline
             (lambda (headline)
               (org-journal-grid--headline-event file headline absolute-date))))))
    (condition-case nil
        (if-let ((buf (find-buffer-visiting file)))
            (with-current-buffer buf
              (funcall parse))
          (with-temp-buffer
            (insert-file-contents file)
            (delay-mode-hooks (org-mode))
            (funcall parse)))
      (error nil))))

(defun org-journal-grid--events-for-day (absolute-date)
  "Return events for ABSOLUTE-DATE, or nil if the file is missing."
  (let* ((dir (org-journal-grid--resolved-directory))
         (file (expand-file-name (org-journal-grid--file-name absolute-date) dir)))
    (when (and (file-regular-p file) (file-readable-p file))
      (delq nil (org-journal-grid--parse-file file absolute-date)))))

(defun org-journal-grid--list-events (start end)
  "Return journal events intersecting START and END (absolute minutes)."
  (let* ((start-day (floor start 1440))
         (end-day (floor (1- end) 1440))
         events)
    (cl-loop for day from start-day to end-day
             do (setq events (nconc events (org-journal-grid--events-for-day day))))
    events))

(defun org-journal-grid--visit (event)
  "Jump to EVENT's org-journal heading."
  (let* ((source (org-journal-grid-event-source event))
         (file (plist-get source :file))
         (position (plist-get source :position)))
    (unless (and file (file-exists-p file))
      (user-error "Journal file disappeared: %s" file))
    (find-file file)
    (goto-char (or position (point-min)))
    (org-fold-show-context 'org-goto)))

(defvar org-journal-grid-backend
  (org-journal-grid-backend-create
   :name "org-journal"
   :list-function #'org-journal-grid--list-events
   :visit-function #'org-journal-grid--visit)
  "Read-only backend for `org-journal-grid'.")

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
    (let ((org-journal-grid-days width))
      (org-journal-grid-open org-journal-grid-backend)
      (when-let ((buf (get-buffer org-journal-grid-buffer-name)))
        (with-current-buffer buf
          (setq-local org-journal-grid-days width))))))

(provide 'org-journal-grid)
;;; org-journal-grid.el ends here
