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

(defvar org-journal-grid-days 7
  "Number of consecutive calendar days shown.
The renderer later replaces this with a defcustom of the same name.")

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

(provide 'org-journal-grid)
;;; org-journal-grid.el ends here
