;;; org-journal-grid-model.el --- Records and backend protocol for org-journal-grid -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Umar Ahmad
;; Adapted for org-journal-grid by Liu Xiang.

;; Author: Umar Ahmad <Gleek@users.noreply.github.com>
;; Maintainer: Umar Ahmad <Gleek@users.noreply.github.com>
;; Version: 0.1.0
;; Keywords: calendar, outlines, convenience
;; URL: https://github.com/Gleek/org-journal-grid

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Data records and layout helpers shared by the journal grid renderer.
;; Adapted from org-timegrid (https://github.com/Gleek/org-timegrid).
;; Keep the original GPL-3 copyright of Umar Ahmad above.

;;; Code:

(require 'calendar)
(require 'cl-lib)

(cl-defstruct (org-journal-grid-event
               (:constructor org-journal-grid-event-create))
  "A backend-neutral calendar event.
START and END are integer minutes based on Emacs absolute Gregorian dates.
SOURCE is opaque to the renderer and belongs to the backend."
  id title start end all-day tags state color source metadata)

(cl-defstruct (org-journal-grid-block
               (:constructor org-journal-grid-block-create))
  "A backend event expressed in coordinates relative to a displayed week.
TIME-KIND is either `timed' or `all-day'.  The remaining layout slots are
filled by the renderers without changing the event's calendar meaning."
  id day start end title time-kind tags state color done event preview
  source-day source-start source-end allow-top allow-bottom boundary-edge
  nest-depth root-id parent-id layout-path nested-family lane lanes
  rail-start rail-end continues-left continues-right rail-lane)

(defun org-journal-grid-block-all-day-p (block)
  "Return non-nil when BLOCK belongs on an all-day surface."
  (eq (org-journal-grid-block-time-kind block) 'all-day))

(defun org-journal-grid-event-time-kind (event)
  "Return EVENT's explicit presentation kind."
  (if (org-journal-grid-event-all-day event) 'all-day 'timed))

(cl-defstruct (org-journal-grid-backend
               (:constructor org-journal-grid-backend-create))
  "Operations supplied by a calendar data backend.
LIST-FUNCTION receives inclusive START and exclusive END absolute minutes.
CREATE-FUNCTION receives TITLE, START, END, an optional source event to
copy, an optional existing backend record selected by READ-ENTRY-FUNCTION
or retained from an event's SOURCE while copying it, and TIME-KIND, either
`timed' or `all-day'.  UPDATE-FUNCTION receives an existing event, its new
START and END, an optional heading title, and TIME-KIND.  Callers remain
compatible with older functions that do not accept TIME-KIND.
DELETE-FUNCTION removes an event's time;
DELETE-ENTRY-FUNCTION removes the record that carried it.  UNDO-FUNCTION
receives non-nil when it continues an unbroken run of undos, and REDO
when the caller wants the reverse; the backend owns undo because only it
knows where the edit landed.  READ-ENTRY-FUNCTION may return a cons of the
display title and an opaque existing record.  READ-TIMESTAMP-FUNCTION lets a
backend supply its own date prompt.  Mutation functions may be nil."
  name list-function create-function update-function delete-function
  delete-entry-function undo-function visit-function
  read-entry-function read-timestamp-function)

(defcustom org-journal-grid-slot-minutes 15
  "Granularity of the calendar, in minutes.
Edits snap to this, it is the smallest range an entry may have, and it is
the height of the keyboard cursor."
  ;; The group is declared in `org-journal-grid', which requires this file rather
  ;; than the other way round, so it is named here instead of inherited.
  :group 'org-journal-grid
  :type 'integer)

(defun org-journal-grid-week-start (&optional absolute-date)
  "Return the first absolute date of the week containing ABSOLUTE-DATE."
  (let* ((absolute (or absolute-date
                       (calendar-absolute-from-gregorian
                        (calendar-current-date))))
         (weekday (calendar-day-of-week
                   (calendar-gregorian-from-absolute absolute)))
         (first-day (if (boundp 'calendar-week-start-day)
                        calendar-week-start-day
                      1)))
    (- absolute (mod (- weekday first-day) 7))))

(defun org-journal-grid-date-label (absolute-date)
  "Return a compact label for ABSOLUTE-DATE."
  (let* ((date (calendar-gregorian-from-absolute absolute-date))
         (month (nth 0 date))
         (day (nth 1 date))
         (day-name (calendar-day-name date t)))
    (format "%s %d/%d" day-name day month)))

(defun org-journal-grid-event-valid-p (event)
  "Return non-nil when EVENT satisfies the renderer contract."
  (and (org-journal-grid-event-p event)
       (org-journal-grid-event-id event)
       (stringp (org-journal-grid-event-title event))
       (integerp (org-journal-grid-event-start event))
       (integerp (org-journal-grid-event-end event))
       (< (org-journal-grid-event-start event)
          (org-journal-grid-event-end event))))

(defun org-journal-grid-backend-list (backend start end)
  "Return validated BACKEND events intersecting START through END."
  (unless (and (org-journal-grid-backend-p backend)
               (functionp
                (org-journal-grid-backend-list-function backend)))
    (error "Calendar backend has no event listing function"))
  (cl-remove-if-not
   (lambda (event)
     (and (org-journal-grid-event-valid-p event)
          (< (org-journal-grid-event-start event) end)
          (> (org-journal-grid-event-end event) start)))
   (funcall (org-journal-grid-backend-list-function backend) start end)))

(defun org-journal-grid-event-to-block (event week-start)
  "Convert EVENT to renderer coordinates relative to WEEK-START."
  (let* ((week-minute (* week-start 1440))
         (relative-start (- (org-journal-grid-event-start event)
                            week-minute))
         (relative-end (- (org-journal-grid-event-end event)
                          week-minute))
         (day (floor relative-start 1440))
         (day-start (* day 1440)))
    (org-journal-grid-block-create
     :id (org-journal-grid-event-id event)
     :day day
     :start (- relative-start day-start)
     :end (- relative-end day-start)
     :time-kind (org-journal-grid-event-time-kind event)
     :title (org-journal-grid-event-title event)
     :tags (org-journal-grid-event-tags event)
     :state (org-journal-grid-event-state event)
     :color (org-journal-grid-event-color event)
     :done (eq (org-journal-grid-event-state event) 'done)
     :event event)))

(defun org-journal-grid-events-to-blocks (events week-start)
  "Convert EVENTS to renderer blocks relative to WEEK-START."
  (mapcar (lambda (event)
            (org-journal-grid-event-to-block event week-start))
          events))

(defun org-journal-grid--assign-group-lanes (group)
  "Return copies of GROUP blocks annotated with overlap lanes."
  (let (lane-ends assigned)
    (dolist (block group)
      (let ((lane 0))
        (while (and (< lane (length lane-ends))
                    (> (nth lane lane-ends) (org-journal-grid-block-start block)))
          (setq lane (1+ lane)))
        (if (= lane (length lane-ends))
            (setq lane-ends
                  (append lane-ends (list (org-journal-grid-block-end block))))
          (setf (nth lane lane-ends) (org-journal-grid-block-end block)))
        (let ((copy (copy-org-journal-grid-block block)))
          (setf (org-journal-grid-block-lane copy) lane)
          (push copy assigned))))
    (let ((count (max 1 (length lane-ends))))
      (mapcar (lambda (block)
                (setf (org-journal-grid-block-lanes block) count)
                block)
              (nreverse assigned)))))

(defun org-journal-grid-basic-layout-day (blocks day)
  "Annotate BLOCKS on DAY with stable overlap lanes."
  (let ((sorted (sort (cl-remove-if-not
                       (lambda (block) (= day (org-journal-grid-block-day block)))
                       (copy-sequence blocks))
                      (lambda (left right)
                        (< (org-journal-grid-block-start left)
                           (org-journal-grid-block-start right)))))
        group group-end output)
    (dolist (block sorted)
      (if (or (null group) (< (org-journal-grid-block-start block) group-end))
          (progn
            (push block group)
            (setq group-end
                  (max (or group-end 0) (org-journal-grid-block-end block))))
        (setq output
              (nconc output
                     (org-journal-grid--assign-group-lanes
                      (nreverse group))))
        (setq group (list block)
              group-end (org-journal-grid-block-end block))))
    (when group
      (setq output
            (nconc output
                   (org-journal-grid--assign-group-lanes
                    (nreverse group)))))
    output))

(provide 'org-journal-grid-model)
;;; org-journal-grid-model.el ends here
