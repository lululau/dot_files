;;; org-journal-grid-render.el --- SVG week calendar with mouse and keyboard editing -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Umar Ahmad
;; Adapted for org-journal-grid by Liu Xiang.

;; Author: Umar Ahmad <Gleek@users.noreply.github.com>
;; Maintainer: Umar Ahmad <Gleek@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: calendar, outlines, convenience
;; URL: https://github.com/Gleek/org-timegrid

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

;; Read-only journal grid renderer adapted from org-timegrid
;; (https://github.com/Gleek/org-timegrid).  It renders whatever a backend
;; supplies, through the protocol in `org-journal-grid-model'.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'face-remap)
(require 'svg)
(require 'subr-x)
(require 'org-journal-grid-model)

(defgroup org-journal-grid nil
  "An SVG week calendar for Emacs, edited by mouse and keyboard."
  :group 'calendar
  :prefix "org-journal-grid-")

(defcustom org-journal-grid-start-hour 0
  "First hour drawn in the week canvas."
  :type 'integer)

(defcustom org-journal-grid-end-hour 24
  "Hour at the bottom edge of the week canvas."
  :type 'integer)

(defcustom org-journal-grid-days 7
  "Number of consecutive calendar days displayed in the calendar.
The visible range is a trailing window of this many days ending on the
anchor date.  It does not snap to `calendar-week-start-day'.
The value must be a positive integer."
  :type '(integer :tag "Days"))

(defun org-journal-grid--last-day-index ()
  "Return the zero-based index of the final visible day."
  (1- org-journal-grid-days))

(defun org-journal-grid--today-absolute ()
  "Return today's absolute Gregorian date."
  (calendar-absolute-from-gregorian (calendar-current-date)))

(defun org-journal-grid--clamp-week-start (week-start &optional days)
  "Return WEEK-START so a window of DAYS does not extend past today."
  (let* ((n (or days org-journal-grid-days))
         (today (org-journal-grid--today-absolute))
         (end (+ week-start (1- n))))
    (if (<= end today)
        week-start
      (- today (1- n)))))

(defun org-journal-grid--range-start (&optional absolute-date)
  "Return the first visible day of a trailing window ending on ABSOLUTE-DATE.
The window never extends past today."
  (let* ((today (org-journal-grid--today-absolute))
         (absolute (min today (or absolute-date today))))
    (- absolute (org-journal-grid--last-day-index))))

(defun org-journal-grid--range-start-keeping-end (week-start old-days new-days)
  "Return a window start that keeps WEEK-START/OLD-DAYS right edge for NEW-DAYS.
The resulting window is clamped so it does not extend past today."
  (org-journal-grid--clamp-week-start
   (- (+ week-start (1- old-days)) (1- new-days))
   new-days))

(defcustom org-journal-grid-pixels-per-minute 0.9
  "Vertical SVG scale in pixels per minute."
  :type 'number)

(defcustom org-journal-grid-default-zoom 0.7
  "Base zoom multiplier for scalable calendar content.
The value must be a positive number.  Buffer-local text-scale commands apply
on top of it and reset to this configured base."
  :type 'number)

(defcustom org-journal-grid-block-gap 1
  "Vertical gap in pixels between consecutive blocks."
  :type 'integer)

(defcustom org-journal-grid-corner-radius 0
  "Radius in pixels for event blocks and their accent bars.
Set this to zero for square event corners."
  :type 'number)

(defcustom org-journal-grid-edge-pixels 8
  "Height of the top and bottom resize zones in pixels."
  :type 'integer)

(defcustom org-journal-grid-edge-slop 3
  "Extra pixels outside a block that count as a resize edge."
  :type 'integer)

(defcustom org-journal-grid-midnight-grip-pixels 6
  "Height of a resize grip shown across an exact midnight boundary."
  :type 'integer)

(defcustom org-journal-grid-nesting-indent 8
  "Horizontal indent in pixels for each contained calendar block."
  :type 'integer)

(defcustom org-journal-grid-title-clearance 18
  "Required vertical pixels before one block may nest inside another.
This keeps at least one line of the containing block's title visible."
  :type 'integer)

(defcustom org-journal-grid-colors
  '((blue     . "#4da3ff")
    (cyan     . "#32ade6")
    (teal     . "#30b0c7")
    (indigo   . "#5856d6")
    (purple   . "#af52de")
    (pink     . "#ff2d55")
    (red      . "#ff3b30")
    (orange   . "#ff9500")
    (yellow   . "#ffcc00")
    (lime     . "#a2c73a")
    (green    . "#34c759")
    (brown    . "#a2845e")
    (graphite . "#8e8e93"))
  "Named colors a backend may use for an event.
The names and values follow the macOS system palette, so a calendar
coloured to match one there needs no translation.  Blocks are drawn as a
translucent wash of the colour with a saturated bar down the left edge,
which is why the values here are saturated rather than pastel.

An event's colour need not be a name: any string Emacs understands, such
as \"#b4d74a\" or \"DarkSeaGreen\", is used as given.  Names exist so a
backend can speak in terms a user recognises."
  :type '(alist :key-type symbol :value-type color))

(defcustom org-journal-grid-default-color 'blue
  "Colour for an event whose backend gives it none.
A name in `org-journal-grid-colors', or any colour string."
  :type '(choice symbol color))

(defcustom org-journal-grid-cursor-opacity 0.22
  "Fill opacity of the keyboard cursor.
The cursor is drawn over the blocks, so its fill stays translucent and
lets a block underneath remain readable."
  :type 'number)

(defcustom org-journal-grid-default-duration-minutes 30
  "Duration assumed wherever no end time is given.
Keyboard creation offers it, and a backend event that has a start but no
end is shown this long."
  :type 'integer)

(defcustom org-journal-grid-all-day-max-lanes 5
  "Maximum number of all-day event lanes shown in the sticky date rail.
Overflow is summarized per day.  The prototype reserves another row for
future keyboard creation and navigation."
  :type 'integer)

(defcustom org-journal-grid-all-day-lane-height 22
  "Height in pixels of one event row in the sticky date rail."
  :type 'integer)

(defcustom org-journal-grid-cursor-step-minutes 15
  "Minutes the keyboard cursor moves per ordinary step.
The default matches `org-journal-grid-slot-minutes'."
  :type 'integer)

(defcustom org-journal-grid-compact-font-size 11
  "Point size of text in a compact one-day strip.
Everything else in the strip is derived from it: the line height, how many
characters fit across a block, and therefore how many lines of title a
block of a given length can hold."
  :type 'integer)

(defcustom org-journal-grid-compact-shadow-pixels 10
  "Height of the shadow at the edges of a compact viewport.
An edge is shaded only when the day holds something past it, so a shadow
means \"more this way\" rather than merely marking where the window stops."
  :type 'integer)

(defcustom org-journal-grid-compact-pixels-per-minute 0.95
  "Vertical scale of a compact one-day strip, in pixels per minute.
This trades detail for reach: a lower value fits more hours into the same
height, at the cost of how much title a short block can show.  The
default puts six hours in about eighteen text lines and still leaves a
half-hour block room for two lines of title."
  :type 'number)

(defcustom org-journal-grid-compact-label-width 40
  "Width in pixels of the time-label gutter in a compact one-day strip."
  :type 'integer)

(defcustom org-journal-grid-data-refresh-seconds 300
  "Seconds between visible backend data refreshes."
  :type 'number)

(defcustom org-journal-grid-keyboard-commit-delay 0.65
  "Idle seconds before a keyboard block movement is written to its backend."
  :type 'number)

(defcustom org-journal-grid-buffer-name "*org-journal-grid*"
  "Name of the calendar buffer."
  :type 'string)

(defconst org-journal-grid--label-width 48)
(defconst org-journal-grid--lane-gap 3)
(defconst org-journal-grid--grid-top-inset 6
  "Pixels between the sticky date rail and the midnight grid line.")
(defconst org-journal-grid--header-title-height 44)
(defconst org-journal-grid--header-date-height 30)
(defconst org-journal-grid--rail-top
  (+ org-journal-grid--header-title-height org-journal-grid--header-date-height))
(defconst org-journal-grid--reference-font-size 10
  "Default-face pixel size for which the SVG's design sizes are specified.")
(defconst org-journal-grid--reference-font-height 18
  "Frame character height for which the fixed header sizes are specified.")

(defun org-journal-grid--default-font-height (&optional window)
  "Return the rendered default-face height for WINDOW in pixels.
Use the current calendar window when WINDOW is nil, with a frame-based
fallback for buffers that are not displayed."
  (let ((window (or window (get-buffer-window (current-buffer) t))))
    (if (window-live-p window)
        (window-font-height window 'default)
      (default-font-height))))

(defun org-journal-grid--frame-font-size (&optional window)
  "Return WINDOW's unscaled default-face font size in pixels."
  (let* ((window (or window (get-buffer-window (current-buffer) t)))
         (frame (and (window-live-p window) (window-frame window)))
         (font (face-attribute 'default :font frame t))
         (size (and (fontp font) (font-get font :size))))
    (if (numberp size)
        size
      (/ (face-attribute 'default :height frame t) 10.0))))

(defun org-journal-grid--default-font-size (&optional window)
  "Return WINDOW's effective default-face font size in SVG pixels."
  (unless (and (numberp org-journal-grid-default-zoom)
               (> org-journal-grid-default-zoom 0))
    (user-error "Org-journal-grid-default-zoom must be positive"))
  (let* ((window (or window (get-buffer-window (current-buffer) t)))
         (frame (and (window-live-p window) (window-frame window)))
         (frame-height (frame-char-height frame)))
    (* (org-journal-grid--frame-font-size window)
       org-journal-grid-default-zoom
       (/ (org-journal-grid--default-font-height window)
          (float frame-height)))))

(defun org-journal-grid--zoom-factor (&optional window)
  "Return the unified SVG scale implied by WINDOW's default font.
Typography and geometry share this factor so their alignment is preserved."
  (/ (org-journal-grid--default-font-size window)
     (float org-journal-grid--reference-font-size)))

(defun org-journal-grid--frame-font-factor (&optional window)
  "Return WINDOW's fixed-header scale without calendar or text zoom."
  (let* ((window (or window (get-buffer-window (current-buffer) t)))
         (frame (and (window-live-p window) (window-frame window))))
    (/ (frame-char-height frame)
       (float org-journal-grid--reference-font-height))))

(defun org-journal-grid--frame-scale-pixels (pixels &optional window)
  "Scale PIXELS with WINDOW's frame font, excluding buffer-local zoom."
  (* pixels (org-journal-grid--frame-font-factor window)))

(defun org-journal-grid--scale-pixels (pixels)
  "Scale vertical PIXELS by the current buffer's text zoom."
  (* pixels (org-journal-grid--zoom-factor)))

(defun org-journal-grid--pixels-per-minute ()
  "Return the zoom-adjusted vertical calendar scale."
  (* org-journal-grid-pixels-per-minute (org-journal-grid--zoom-factor)))

(defun org-journal-grid--font-size (size)
  "Return SVG font SIZE adjusted by the unified calendar zoom."
  (* size (org-journal-grid--zoom-factor)))

(defun org-journal-grid--label-width ()
  "Return the width of the time-label gutter.
Let it grow with buffer zoom, but not shrink below the frame-font width needed
by the fixed week label in the header."
  (max (org-journal-grid--scale-pixels org-journal-grid--label-width)
       (org-journal-grid--frame-scale-pixels org-journal-grid--label-width)))

(defun org-journal-grid--grid-top-inset ()
  "Return the zoom-adjusted inset above the timed grid."
  (org-journal-grid--scale-pixels org-journal-grid--grid-top-inset))

(defun org-journal-grid--header-title-height ()
  "Return the fixed month-and-year title section height."
  org-journal-grid--header-title-height)

(defun org-journal-grid--rail-top ()
  "Return the top of the all-day rail.
The title stays fixed and the date row follows only the frame's base font."
  (+ org-journal-grid--header-title-height
     (org-journal-grid--frame-scale-pixels org-journal-grid--header-date-height)))

(defun org-journal-grid--all-day-lane-height ()
  "Return the zoom-adjusted height of one all-day lane."
  (org-journal-grid--scale-pixels org-journal-grid-all-day-lane-height))
(defvar-local org-journal-grid--geometry nil)
(defvar-local org-journal-grid--header-geometry nil)
(defvar-local org-journal-grid--drag-rail-rows nil
  "Rail row count frozen for the duration of the current mouse gesture.")
(defvar-local org-journal-grid--rendered-ui nil)
(defvar org-journal-grid--header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-1] #'org-journal-grid-header-press)
    (define-key map [header-line double-down-mouse-1]
                #'org-journal-grid-ignore-double-press)
    (define-key map [header-line mouse-1] #'org-journal-grid-header-click)
    (define-key map [header-line double-mouse-1] #'org-journal-grid-header-visit)
    map)
  "Mouse map installed directly on the sticky calendar header image.")
;; Refresh an existing map when this source is evaluated in a live Emacs.
;; Header-line events arrive prefixed through the mode map but unprefixed
;; through the image string's `keymap' property, so support both forms.
(define-key org-journal-grid--header-map [down-mouse-1]
            #'org-journal-grid-header-press)
(define-key org-journal-grid--header-map [double-down-mouse-1]
            #'org-journal-grid-ignore-double-press)
(define-key org-journal-grid--header-map [mouse-1]
            #'org-journal-grid-header-click)
(define-key org-journal-grid--header-map [double-mouse-1]
            #'org-journal-grid-header-visit)
(define-key org-journal-grid--header-map [header-line down-mouse-1]
            #'org-journal-grid-header-press)
(define-key org-journal-grid--header-map [header-line double-down-mouse-1]
            #'org-journal-grid-ignore-double-press)
(define-key org-journal-grid--header-map [header-line mouse-1]
            #'org-journal-grid-header-click)
(define-key org-journal-grid--header-map [header-line double-mouse-1]
            #'org-journal-grid-header-visit)
(dolist (area '(calendar-rail-block calendar-rail-resize))
  (define-key org-journal-grid--header-map (vector area 'down-mouse-1)
              #'org-journal-grid-header-press)
  (define-key org-journal-grid--header-map (vector area 'double-down-mouse-1)
              #'org-journal-grid-ignore-double-press)
  (define-key org-journal-grid--header-map (vector area 'mouse-1)
              #'org-journal-grid-header-click)
  (define-key org-journal-grid--header-map (vector area 'double-mouse-1)
              #'org-journal-grid-header-visit))
(defvar-local org-journal-grid--image-height nil)
(defvar-local org-journal-grid--last-width nil)
(defvar-local org-journal-grid--pointer-overlay nil)
(defvar-local org-journal-grid--resize-timer nil)
(defvar-local org-journal-grid--clock-timer nil)
(defvar-local org-journal-grid--data-timer nil)
(defvar-local org-journal-grid--scroll-restore-timer nil)
(defvar-local org-journal-grid--keyboard-edit-timer nil)
(defvar-local org-journal-grid--keyboard-edit nil)
(defvar-local org-journal-grid--stale nil)
(defvar-local org-journal-grid--saved-vscroll 0
  "Pixel scroll position restored when this calendar is shown again.")
(defconst org-journal-grid--scroll-rebound-seconds 0.35
  "Seconds to suppress a reversed wheel event after hitting an edge.")
(defvar-local org-journal-grid--scroll-boundary nil
  "Most recently hit scroll boundary as (SIDE . TIME).")
(defvar-local org-journal-grid--last-zoom-factor 1.0
  "Zoom factor used by the currently rendered SVG tiles.")
(defvar-local org-journal-grid--tile-height nil)
(defvar-local org-journal-grid--tile-count nil)
(defvar-local org-journal-grid--tile-width nil)
(defvar-local org-journal-grid--tile-markers nil)
(defvar-local org-journal-grid--static-inner nil)
(defvar-local org-journal-grid--static-images nil)
(defvar-local org-journal-grid--dynamic-tiles nil)
(defvar-local org-journal-grid--clock-fragment nil)
(defvar-local org-journal-grid--clock-tiles nil)
(defvar-local org-journal-grid--base-excluded-id nil)
(defvar-local org-journal-grid--fringe-remap-cookie nil)
(defvar org-journal-grid--static-render nil)
(defvar org-journal-grid--render-excluded-id nil)
(defvar org-journal-grid--theme-timer nil)
(defvar-local org-journal-grid--backend nil)
(defvar-local org-journal-grid--state nil)

(cl-defstruct (org-journal-grid--cursor-state
               (:constructor org-journal-grid--cursor-state-create))
  "A position on either the timed grid or the all-day rail."
  surface day minute lane)

(cl-defstruct (org-journal-grid--operation
               (:constructor org-journal-grid--operation-create))
  "A proposed calendar mutation and its preview BLOCK."
  kind block replace-id error)

(cl-defstruct (org-journal-grid--calendar-state
               (:constructor org-journal-grid--calendar-state-create))
  "Backend data and interactive state for one displayed week."
  week-start events blocks preview cursor selected-id cursor-visible)

(defun org-journal-grid--make-block
    (id day start end title &optional color done time-kind)
  "Construct a renderer block from ID, DAY, START, END, TITLE and COLOR.
DONE and TIME-KIND describe the block's completion and timestamp kind."
  (org-journal-grid-block-create
   :id id :day day :start start :end end :title title
   :time-kind (or time-kind 'timed) :color color :done done))

(defun org-journal-grid--timed-blocks ()
  "Return the current state's timed blocks."
  (seq-remove #'org-journal-grid-block-all-day-p
              (org-journal-grid--calendar-state-blocks org-journal-grid--state)))

(defun org-journal-grid--all-day-blocks ()
  "Return the current state's all-day blocks."
  (seq-filter #'org-journal-grid-block-all-day-p
              (org-journal-grid--calendar-state-blocks org-journal-grid--state)))

(defun org-journal-grid--load-state (week-start)
  "Load renderer state for WEEK-START from the current backend.
WEEK-START is clamped so the window does not extend past today."
  (setq week-start (org-journal-grid--clamp-week-start week-start))
  (let* ((events (org-journal-grid-backend-list
                  org-journal-grid--backend
                  (* week-start 1440)
                  (* (+ week-start org-journal-grid-days) 1440)))
         (blocks (org-journal-grid-events-to-blocks events week-start)))
    ;; The cursor has a remembered position and a separate visibility, so
    ;; hiding it with C-g keeps the place, and block selection can record a
    ;; position without drawing anything.
    (org-journal-grid--calendar-state-create
     :week-start week-start :events events :blocks blocks)))

(defun org-journal-grid--default-cursor (week-start)
  "Return the initial keyboard cursor for the week at WEEK-START.
It sits on the current fifteen-minute slot when today is visible, and on
the first visible day at the configured start hour otherwise."
  (let* ((today (calendar-absolute-from-gregorian (calendar-current-date)))
         (offset (- today week-start)))
    (if (<= 0 offset (org-journal-grid--last-day-index))
        (let ((now (decode-time)))
          (org-journal-grid--cursor-state-create
           :surface 'grid :day offset
           :minute (org-journal-grid--snap-minute
                    (+ (* 60 (decoded-time-hour now))
                       (decoded-time-minute now)))
           :lane 0))
      (org-journal-grid--cursor-state-create
       :surface 'grid :day 0
       :minute (* 60 org-journal-grid-start-hour) :lane 0))))

(defun org-journal-grid--snap-minute (minute)
  "Return MINUTE rounded down to a fifteen-minute slot inside one day."
  (let ((slots (/ (- (* 60 24) org-journal-grid-slot-minutes)
                  org-journal-grid-slot-minutes)))
    (* org-journal-grid-slot-minutes
       (max 0 (min slots (floor minute org-journal-grid-slot-minutes))))))

(defun org-journal-grid--cursor ()
  "Return the remembered cursor position, which may be hidden."
  (org-journal-grid--calendar-state-cursor org-journal-grid--state))

(defun org-journal-grid--cursor-visible-p ()
  "Return non-nil when the cursor is currently drawn."
  (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state))

(defun org-journal-grid--blocks-starting-at (day minute)
  "Return committed blocks starting inside DAY's slot at MINUTE.
A real Org range can start at 13:10, which is no slot at all, so the test
is whether the start falls within this slot rather than equalling it;
otherwise such a block could never be selected.  Shortest first, so a
nested child is offered before its parent.  Several blocks can share a
slot, which is what the cursor's :lane disambiguates."
  (when (and (numberp day) (numberp minute))
    (sort (seq-filter (lambda (block)
                        (and (not (org-journal-grid-block-preview block))
                             (= (org-journal-grid-block-day block) day)
                             (>= (org-journal-grid-block-start block) minute)
                             (< (org-journal-grid-block-start block)
                                (+ minute org-journal-grid-slot-minutes))))
                      (org-journal-grid--timed-blocks))
          (lambda (left right)
            (< (- (org-journal-grid-block-end left) (org-journal-grid-block-start left))
               (- (org-journal-grid-block-end right) (org-journal-grid-block-start right)))))))

(defun org-journal-grid--selected-id ()
  "Return the explicitly selected block id, or nil while hidden."
  (and (org-journal-grid--cursor-visible-p)
       (org-journal-grid--calendar-state-selected-id org-journal-grid--state)))

(defun org-journal-grid--cursor-rectangle (&optional geometry-list)
  "Return the cursor slot's pixel rectangle, or nil while it is hidden.
The rectangle narrows to the selected block's lane, which is how one lane
among several is visible at all.  GEOMETRY-LIST defaults to the committed
geometry; `org-journal-grid--svg' passes the list it is still building, so the
cursor it draws and this agree."
  (when-let* (((org-journal-grid--cursor-visible-p))
              (cursor (org-journal-grid--cursor)))
    (let* ((canvas (max 560 (org-journal-grid--window-width)))
           (column (/ (- canvas (org-journal-grid--label-width))
                      (float org-journal-grid-days)))
           (selected (org-journal-grid--selected-id))
           (lane (and selected
                      (cl-find-if
                       (lambda (item)
                         (and (equal (plist-get item :id) selected)
                              (= (plist-get item :day)
                                 (org-journal-grid--cursor-state-day cursor))
                              (not (plist-get item :boundary-edge))))
                       (or geometry-list org-journal-grid--geometry)))))
      (list :x (if lane
                   (plist-get lane :x)
                 (+ 1 (org-journal-grid--label-width)
                    (* (org-journal-grid--cursor-state-day cursor) column)))
            :y (+ (org-journal-grid--grid-top-inset)
                  (* (- (org-journal-grid--cursor-state-minute cursor)
                        (* 60 org-journal-grid-start-hour))
                     (org-journal-grid--pixels-per-minute)))
            :width (if lane (plist-get lane :width) (- column 2))
            :height (* org-journal-grid-slot-minutes
                       (org-journal-grid--pixels-per-minute))))))

(defun org-journal-grid--ensure-cursor ()
  "Return the cursor position, defaulting it when nothing is remembered."
  (or (org-journal-grid--cursor)
      (let ((cursor (org-journal-grid--default-cursor
                     (org-journal-grid--calendar-state-week-start
                      org-journal-grid--state))))
        (setf (org-journal-grid--calendar-state-cursor org-journal-grid--state) cursor)
        cursor)))

(defun org-journal-grid--reveal-cursor ()
  "Show the cursor, returning non-nil when it was hidden until now.
The first movement key reveals the cursor where it was left rather than
also moving it, so its position is visible before it is used."
  (unless (org-journal-grid--cursor-visible-p)
    (org-journal-grid--ensure-cursor)
    (setf (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state) t)
    t))

(defun org-journal-grid--cursor-absolute ()
  "Return the cursor's absolute week minute."
  (let ((cursor (org-journal-grid--ensure-cursor)))
    (+ (* (org-journal-grid--cursor-state-day cursor) 1440)
       (org-journal-grid--cursor-state-minute cursor))))

(defun org-journal-grid--set-cursor (day minute &optional lane)
  "Move the cursor to DAY and MINUTE, clamped to the visible week.
LANE picks between blocks sharing that start, and defaults to zero."
  (let* ((day (max 0 (min (org-journal-grid--last-day-index) day)))
         (minute (org-journal-grid--snap-minute minute))
         (lane (or lane 0))
         (candidates (org-journal-grid--blocks-starting-at day minute))
         (selected (and candidates
                        (org-journal-grid-block-id
                         (nth (min lane (1- (length candidates))) candidates)))))
    (setf (org-journal-grid--calendar-state-cursor org-journal-grid--state)
          (org-journal-grid--cursor-state-create
           :surface 'grid :day day :minute minute :lane lane)
          (org-journal-grid--calendar-state-selected-id org-journal-grid--state)
          selected))
  (org-journal-grid--cursor))

(defun org-journal-grid--reload-state (week-start)
  "Reload WEEK-START from the backend, keeping cursor and selection.
Navigation and refresh drop preview and history, but they must not throw
the cursor back to today, and they must not clear the selection: every
edit refreshes, so a cleared selection would make repeated keyboard
nudges of one block impossible.  A selection that no longer resolves
after the reload is dropped."
  (let ((cursor (org-journal-grid--calendar-state-cursor org-journal-grid--state))
        (visible (org-journal-grid--calendar-state-cursor-visible
                  org-journal-grid--state))
        (selected (org-journal-grid--calendar-state-selected-id
                   org-journal-grid--state)))
    (setq-local org-journal-grid--state (org-journal-grid--load-state week-start))
    (when cursor
      (setf (org-journal-grid--calendar-state-cursor org-journal-grid--state) cursor
            (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state)
            visible
            (org-journal-grid--calendar-state-selected-id org-journal-grid--state)
            selected))
    (setq-local org-journal-grid--static-inner nil)))

(defun org-journal-grid--block (id)
  "Return the current renderer block identified by ID."
  (cl-find id (org-journal-grid--calendar-state-blocks org-journal-grid--state)
           :key #'org-journal-grid-block-id :test #'equal))

(defun org-journal-grid--set-absolute-range
    (block absolute-start absolute-end)
  "Set BLOCK to ABSOLUTE-START and ABSOLUTE-END week-minute values."
  (let* ((day (floor absolute-start 1440))
         (day-start (* day 1440)))
    (setf (org-journal-grid-block-day block) day
          (org-journal-grid-block-start block) (- absolute-start day-start)
          (org-journal-grid-block-end block) (- absolute-end day-start))
    block))

(defun org-journal-grid--transform-block-range (block delta edge)
  "Return a copy of BLOCK shifted by DELTA minutes at optional EDGE.
All-day ranges use whole-day granularity and may continue beyond the
visible week.  Timed ranges remain clamped to the visible week."
  (let* ((copy (copy-org-journal-grid-block block))
         (all-day (org-journal-grid-block-all-day-p block))
         (minimum (if all-day 1440 org-journal-grid-slot-minutes))
         (start (+ (* (org-journal-grid-block-day block) 1440)
                   (org-journal-grid-block-start block)))
         (end (+ (* (org-journal-grid-block-day block) 1440)
                 (org-journal-grid-block-end block)))
         (duration (- end start))
         (week-end (* org-journal-grid-days 1440)))
    (pcase edge
      ('top
       (setq start (min (+ start delta) (- end minimum)))
       (unless all-day (setq start (max 0 start))))
      ('bottom
       (setq end (max (+ end delta) (+ start minimum)))
       (unless all-day (setq end (min week-end end))))
      (_
       (setq start
             (if all-day
                 (max (- 1440 duration)
                      (min (+ start delta) (- week-end 1440)))
               (max 0 (min (+ start delta) (- week-end duration))))
             end (+ start duration))))
    (org-journal-grid--set-absolute-range copy start end)
    (setf (org-journal-grid-block-preview copy) t)
    copy))

(defun org-journal-grid--proposal (origin target copy-kind)
  "Return a drag proposal from ORIGIN to TARGET.
COPY-KIND is nil for a move, `duplicate-entry' for an independent copy,
or `add-occurrence' for another time on the source entry."
  (let* ((id (plist-get origin :block-id))
         (copying (memq copy-kind '(duplicate-entry add-occurrence)))
         (origin-surface (or (plist-get origin :surface) 'grid))
         (target-surface (or (plist-get target :surface) 'grid))
         (origin-day (plist-get origin :day))
         (target-day (plist-get target :day))
         (origin-minute (plist-get origin :minute))
         (target-minute (plist-get target :minute))
         (edge (plist-get origin :edge)))
    (cond
     ((or (null origin-day) (null origin-minute)
          (null target-day) (null target-minute))
      (org-journal-grid--operation-create
       :error "Release inside a calendar cell"))
     ((null id)
      (if (/= origin-day target-day)
          (org-journal-grid--operation-create
           :error "New ranges currently stay within one day")
        (let ((block (org-journal-grid--make-block
                      'preview origin-day
                      (min origin-minute target-minute)
                      (+ (max origin-minute target-minute)
                         org-journal-grid-slot-minutes)
                      "New block" 'blue)))
          (setf (org-journal-grid-block-preview block) t)
          (org-journal-grid--operation-create :kind 'create :block block))))
     ((not (eq origin-surface target-surface))
      (let ((source (org-journal-grid--block id)))
        (cond
         ((null source)
          (org-journal-grid--operation-create
           :error "Drag an existing block between the all-day rail and time grid"))
         ((and (eq origin-surface 'rail)
               (/= (- (org-journal-grid-block-end source)
                       (org-journal-grid-block-start source))
                   1440))
          (org-journal-grid--operation-create
           :error "Multi-day blocks cannot move into the time grid"))
         (t
          (let* ((block (copy-org-journal-grid-block source))
                 (start (+ (* target-day 1440)
                           (if (eq target-surface 'rail) 0 target-minute)))
                 (duration (if (eq target-surface 'rail)
                               1440
                             org-journal-grid-default-duration-minutes)))
            (org-journal-grid--set-absolute-range block start (+ start duration))
            (setf (org-journal-grid-block-time-kind block)
                  (if (eq target-surface 'rail) 'all-day 'timed)
                  (org-journal-grid-block-preview block) t)
            (org-journal-grid--operation-create
             :kind (if copying copy-kind 'move) :block block
             :replace-id (and (not copying) id)))))))
     (t
      (let ((source (org-journal-grid--block id)))
        (if (null source)
            (org-journal-grid--operation-create
             :error "That calendar block changed; refresh and try again")
          (let* ((block (copy-org-journal-grid-block source))
                 (absolute-start (+ (* (org-journal-grid-block-day block) 1440)
                                    (org-journal-grid-block-start block)))
                 (absolute-end (+ (* (org-journal-grid-block-day block) 1440)
                                  (org-journal-grid-block-end block)))
                 (duration (- absolute-end absolute-start))
                 (absolute-origin (+ (* origin-day 1440) origin-minute))
                 (absolute-target (+ (* target-day 1440) target-minute))
                 kind)
            (let ((unit (if (eq origin-surface 'rail)
                            1440
                          org-journal-grid-slot-minutes)))
              (cond
             ((and (eq edge 'top) (not copying))
              (setq kind 'resize)
              (org-journal-grid--set-absolute-range
               block (max 0 (min absolute-target
                                 (- absolute-end unit)))
               absolute-end))
             ((and (eq edge 'bottom) (not copying))
              (setq kind 'resize)
              (org-journal-grid--set-absolute-range
               block absolute-start
               (min (* org-journal-grid-days 1440)
                    (max (+ absolute-target unit)
                         (+ absolute-start unit)))))
             (t
              (setq kind (if copying copy-kind 'move))
              (let* ((grab-offset (- absolute-origin absolute-start))
                     (new-start (max 0 (min (- absolute-target grab-offset)
                                            (- (* org-journal-grid-days 1440) duration)))))
                (org-journal-grid--set-absolute-range
                 block new-start (+ new-start duration))))))
            (setf (org-journal-grid-block-preview block) t)
            (org-journal-grid--operation-create
             :kind kind :block block
             :replace-id (and (not copying) id)))))))))

(defun org-journal-grid--remapped-color (face attribute)
  "Return ATTRIBUTE of FACE as remapped in this buffer, or nil.
`face-remapping-alist' is how a buffer gets a background of its own --
solaire-mode dims every buffer that is not visiting a file, and per-buffer
theming works the same way -- and `face-attribute' does not consult it.
Reading the global face instead draws a calendar that does not match the
buffer it sits in.  An entry is a face, or a list of faces with the first
taking priority, or an inline attribute list."
  (let ((remap (cdr (assq face face-remapping-alist))))
    (seq-some
     (lambda (entry)
       (cond ((and (symbolp entry) (facep entry))
              (let ((value (face-attribute entry attribute nil t)))
                (and (stringp value) value)))
             ((and (consp entry) (keywordp (car entry)))
              (plist-get entry attribute))))
     (if (proper-list-p remap) remap (list remap)))))

(defun org-journal-grid--face-color
    (face attribute fallback)
  "Return FACE ATTRIBUTE as a color string, or FALLBACK.
Buffer-local face remapping wins, so the drawing matches the buffer it is
drawn into rather than the frame's idea of the face."
  (let ((value (or (org-journal-grid--remapped-color face attribute)
                   (face-attribute face attribute nil t))))
    (org-journal-grid--svg-color value fallback)))

(defun org-journal-grid--svg-color (color &optional fallback)
  "Return COLOR as an SVG-safe six-digit hexadecimal string.
Emacs accepts X11 names such as `Red1' that SVG does not.  Resolve every
color through Emacs before handing it to the SVG renderer.  Use FALLBACK
when COLOR cannot be resolved."
  (when-let* ((rgb (or (org-journal-grid--rgb color)
                      (org-journal-grid--rgb fallback))))
    (apply #'color-rgb-to-hex (append rgb '(2)))))

(defun org-journal-grid--blend (foreground background amount)
  "Blend FOREGROUND into BACKGROUND by AMOUNT."
  (let ((foreground-rgb (org-journal-grid--rgb foreground))
        (background-rgb (org-journal-grid--rgb background)))
    (if (and foreground-rgb background-rgb)
        (apply #'color-rgb-to-hex
               (append
                (cl-mapcar (lambda (foreground-part background-part)
                             (+ (* amount foreground-part)
                                (* (- 1 amount) background-part)))
                           foreground-rgb background-rgb)
                '(2)))
      background)))

(defun org-journal-grid--rgb (color)
  "Return normalized RGB components for COLOR.
Parse hexadecimal values directly because some macOS builds interpret a
six-digit value as three one-digit components in `color-values'."
  (if (and (stringp color)
           (string-match "\\`#\\([[:xdigit:]]+\\)\\'" color)
           (= (% (length (match-string 1 color)) 3) 0))
      (let* ((hex (match-string 1 color))
             (digits (/ (length hex) 3))
             (maximum (float (1- (expt 16 digits)))))
        (cl-loop for offset from 0 below (length hex) by digits
                 collect (/ (string-to-number
                              (substring hex offset (+ offset digits)) 16)
                            maximum)))
    (color-name-to-rgb color)))

(defun org-journal-grid--palette ()
  "Return SVG colors derived from the current Emacs faces."
  (let* ((background (org-journal-grid--face-color
                      'default :background "#ffffff"))
         (foreground (org-journal-grid--face-color
                      'default :foreground "#30343a"))
         (muted (org-journal-grid--face-color
                 'shadow :foreground foreground))
         ;; Chrome only: the selection outline and the current-time line.
         ;; Event colours come from `org-journal-grid-colors'.
         (blue (org-journal-grid--face-color
                'link :foreground "#3979d6"))
         (red (org-journal-grid--face-color
               'error :foreground "#d94b4b"))
         (highlight (org-journal-grid--face-color
                     'highlight :background blue)))
    (list :background background :foreground foreground :muted muted
          :grid (org-journal-grid--blend foreground background 0.16)
          :half-grid (org-journal-grid--blend foreground background 0.08)
          :time-background
          (org-journal-grid--blend foreground background 0.035)
          :time-label
          (org-journal-grid--blend foreground background 0.64)
          :secondary-text
          (org-journal-grid--blend foreground background 0.72)
          :weekend (org-journal-grid--blend foreground background 0.025)
          :today (org-journal-grid--blend highlight background 0.10)
          :blue blue :red red
          :cursor (org-journal-grid--face-color 'cursor :background blue)
          :preview-fill (org-journal-grid--blend muted background 0.18)
          :done-fill (org-journal-grid--blend muted background 0.11))))

(defun org-journal-grid--sync-fringe-background ()
  "Match this buffer's fringes to its time-label gutter."
  (when org-journal-grid--fringe-remap-cookie
    (face-remap-remove-relative org-journal-grid--fringe-remap-cookie))
  (setq-local
   org-journal-grid--fringe-remap-cookie
   (face-remap-add-relative
    'fringe
    (list :background
          (plist-get (org-journal-grid--palette) :time-background)))))

(defun org-journal-grid--layout-day (blocks day)
  "Lay out BLOCKS on DAY as nested, independently split sibling groups."
  (let* ((sorted (sort (cl-remove-if-not
                        (lambda (block) (= day (org-journal-grid-block-day block)))
                        (copy-sequence blocks))
                       (lambda (left right)
                         (if (= (org-journal-grid-block-start left)
                                (org-journal-grid-block-start right))
                             (> (org-journal-grid-block-end left)
                                (org-journal-grid-block-end right))
                           (< (org-journal-grid-block-start left)
                              (org-journal-grid-block-start right))))))
         annotated)
    (dolist (block sorted)
      (let* ((containers
              (cl-remove-if-not
               (lambda (candidate)
                 (and (<= (org-journal-grid-block-start candidate)
                           (org-journal-grid-block-start block))
                      (>= (org-journal-grid-block-end candidate)
                           (org-journal-grid-block-end block))
                      (or (< (org-journal-grid-block-start candidate)
                             (org-journal-grid-block-start block))
                          (> (org-journal-grid-block-end candidate)
                             (org-journal-grid-block-end block)))
                      (>= (* (- (org-journal-grid-block-start block)
                                (org-journal-grid-block-start candidate))
                             (org-journal-grid--pixels-per-minute))
                          (org-journal-grid--scale-pixels
                           org-journal-grid-title-clearance))))
               annotated))
             (parent
              (car (sort containers
                         (lambda (left right)
                           (> (org-journal-grid-block-nest-depth left)
                              (org-journal-grid-block-nest-depth right))))))
             (copy (copy-org-journal-grid-block block)))
        (setf (org-journal-grid-block-nest-depth copy)
              (if parent
                  (1+ (org-journal-grid-block-nest-depth parent))
                0)
              (org-journal-grid-block-root-id copy)
              (if parent
                  (org-journal-grid-block-root-id parent)
                (org-journal-grid-block-id copy))
              (org-journal-grid-block-parent-id copy)
              (and parent (org-journal-grid-block-id parent)))
        (push copy annotated)))
    (setq annotated (nreverse annotated))
    (let ((siblings (make-hash-table :test #'equal))
          (local-layout (make-hash-table :test #'equal))
          (output (make-hash-table :test #'equal))
          (family-size (make-hash-table :test #'equal)))
      (dolist (block annotated)
        (let ((parent-id (org-journal-grid-block-parent-id block))
              (root-id (org-journal-grid-block-root-id block)))
          (puthash parent-id
                   (cons block (gethash parent-id siblings))
                   siblings)
          (puthash root-id (1+ (gethash root-id family-size 0)) family-size)))
      ;; Every parent's children get their own overlap lanes.  This prevents
      ;; deep siblings from painting over each other's title line while both
      ;; remain geometrically contained by the same ancestor.
      (maphash
       (lambda (_parent-id children)
         (dolist (child
                  (org-journal-grid-basic-layout-day
                   (nreverse children) day))
           (puthash (org-journal-grid-block-id child) child local-layout)))
       siblings)
      ;; Parents precede their children in ANNOTATED, so their completed path
      ;; is available when constructing each descendant's path.
      (mapcar
       (lambda (block)
         (let* ((copy (copy-org-journal-grid-block block))
                (id (org-journal-grid-block-id copy))
                (parent-id (org-journal-grid-block-parent-id copy))
                (parent (and parent-id (gethash parent-id output)))
                (local (gethash id local-layout))
                (step (cons (or (org-journal-grid-block-lane local) 0)
                            (max 1 (or (org-journal-grid-block-lanes local) 1))))
                (path (append (and parent (org-journal-grid-block-layout-path parent))
                              (list step))))
           (setf (org-journal-grid-block-layout-path copy) path
                 (org-journal-grid-block-nested-family copy)
                 (> (gethash (org-journal-grid-block-root-id copy)
                             family-size 0)
                    1))
           (puthash id copy output)
           copy))
       annotated))))

(defun org-journal-grid--layout-frame
    (block day-x column-width)
  "Return BLOCK's horizontal (X . WIDTH) inside a day column.
DAY-X and COLUMN-WIDTH describe the full column."
  (let ((x day-x)
        (width column-width)
        (depth 0))
    (dolist (step (org-journal-grid-block-layout-path block))
      (when (> depth 0)
        (setq x (+ x org-journal-grid-nesting-indent)
              width (max 4 (- width
                              org-journal-grid-nesting-indent))))
      (let* ((lane (car step))
             (lanes (max 1 (cdr step)))
             (available (- width
                           (* (1- lanes)
                              org-journal-grid--lane-gap)))
             (lane-width (/ available lanes)))
        (setq x (+ x (* lane (+ lane-width
                                org-journal-grid--lane-gap)))
              width lane-width))
      (setq depth (1+ depth)))
    (cons x width)))

(defun org-journal-grid--wrap-title
    (title width max-lines)
  "Wrap TITLE to WIDTH characters and at most MAX-LINES lines."
  (let ((remaining (string-trim (or title "")))
        lines)
    (dotimes (line max-lines)
      (when (not (string-empty-p remaining))
        (if (= line (1- max-lines))
            (progn
              (push (truncate-string-to-width remaining width nil nil "…") lines)
              (setq remaining ""))
          (if (<= (string-width remaining) width)
              (progn (push remaining lines) (setq remaining ""))
            (let ((cut (min width (length remaining))))
              (while (and (> cut 1)
                          (> (string-width (substring remaining 0 cut)) width))
                (setq cut (1- cut)))
              (let* ((space (cl-position ?\s remaining :from-end t :end cut))
                     (end (if (and space (> space 0)) space cut)))
                (push (string-trim-right (substring remaining 0 end)) lines)
                (setq remaining
                      (string-trim-left
                       (substring remaining
                                  (if (and space (= end space)) (1+ end) end))))))))))
    (nreverse lines)))

(defun org-journal-grid--format-minute (minute)
  "Format MINUTE as a clock time normalized to the 24-hour day."
  (let ((normalized (% minute 1440)))
    (format "%02d:%02d" (/ normalized 60) (% normalized 60))))

(defun org-journal-grid--format-range (start end)
  "Format logical START and END minutes, including the end-day offset."
  (let ((day-offset (floor end 1440)))
    (format "%s–%s%s"
            (org-journal-grid--format-minute start)
            (org-journal-grid--format-minute end)
            (if (> day-offset 0) (format " (+%d)" day-offset) ""))))

(defun org-journal-grid--resolve-color (color palette)
  "Return COLOR as a colour string, resolving a name through PALETTE.
COLOR is a name in `org-journal-grid-colors', any colour string, or nil for
`org-journal-grid-default-color'.  An unknown name falls back to the theme's
own accent rather than drawing nothing."
  (let ((color (or color org-journal-grid-default-color)))
    (org-journal-grid--svg-color
     (or (and (symbolp color) (cdr (assq color org-journal-grid-colors)))
         (and (stringp color) (color-defined-p color) color)
         (cdr (assq org-journal-grid-default-color org-journal-grid-colors)))
     (plist-get palette :blue))))

(defun org-journal-grid--accent (block palette)
  "Return the accent color for BLOCK from PALETTE."
  (if (or (org-journal-grid-block-preview block) (org-journal-grid-block-done block))
      (plist-get palette :muted)
    (org-journal-grid--resolve-color (org-journal-grid-block-color block) palette)))

(defun org-journal-grid--color (block palette)
  "Return the fill color for BLOCK from PALETTE.
A block is a wash of its accent over the buffer background, so any colour
works without the palette having to know it in advance."
  (cond
   ((org-journal-grid-block-preview block) (plist-get palette :preview-fill))
   ((org-journal-grid-block-done block) (plist-get palette :done-fill))
   (t (org-journal-grid--blend (org-journal-grid--accent block palette)
                           (plist-get palette :background) 0.17))))

(defun org-journal-grid--display-segments (block)
  "Return visible per-day segments and exact-midnight grips for BLOCK."
  (let* ((source-day (org-journal-grid-block-day block))
         (source-start (org-journal-grid-block-start block))
         (source-end (org-journal-grid-block-end block))
         (absolute-start (+ (* source-day 1440) source-start))
         (absolute-end (+ (* source-day 1440) source-end))
         segments)
    (dotimes (day org-journal-grid-days)
      (let* ((day-start (* day 1440))
             (day-end (+ day-start 1440))
             (segment-start (max absolute-start day-start))
             (segment-end (min absolute-end day-end)))
        (when (< segment-start segment-end)
          (let ((segment (copy-org-journal-grid-block block)))
            (setf (org-journal-grid-block-source-day segment) source-day
                  (org-journal-grid-block-source-start segment) source-start
                  (org-journal-grid-block-source-end segment) source-end
                  (org-journal-grid-block-day segment) day
                  (org-journal-grid-block-start segment) (- segment-start day-start)
                  (org-journal-grid-block-end segment) (- segment-end day-start)
                  (org-journal-grid-block-allow-top segment)
                  (= segment-start absolute-start)
                  (org-journal-grid-block-allow-bottom segment)
                  (= segment-end absolute-end))
            (push segment segments)))))
    ;; A range ending exactly at midnight needs a small bottom-edge target at
    ;; the top of the next day so it can be extended forward.
    (when (and (= (% absolute-end 1440) 0)
               (< 0 absolute-end (* org-journal-grid-days 1440)))
      (let* ((day (/ absolute-end 1440))
             (grip (copy-org-journal-grid-block block)))
        (setf (org-journal-grid-block-source-day grip) source-day
              (org-journal-grid-block-source-start grip) source-start
              (org-journal-grid-block-source-end grip) source-end
              (org-journal-grid-block-day grip) day
              (org-journal-grid-block-start grip) 0
              (org-journal-grid-block-end grip) 1
              (org-journal-grid-block-title grip) ""
              (org-journal-grid-block-boundary-edge grip) 'bottom
              (org-journal-grid-block-allow-top grip) nil
              (org-journal-grid-block-allow-bottom grip) t)
        (push grip segments)))
    ;; A range starting exactly at midnight gets its top-edge target at the
    ;; bottom of the preceding day so it can be extended backward.
    (when (and (= (% absolute-start 1440) 0)
               (< 0 absolute-start (* org-journal-grid-days 1440)))
      (let* ((day (1- (/ absolute-start 1440)))
             (grip (copy-org-journal-grid-block block)))
        (setf (org-journal-grid-block-source-day grip) source-day
              (org-journal-grid-block-source-start grip) source-start
              (org-journal-grid-block-source-end grip) source-end
              (org-journal-grid-block-day grip) day
              (org-journal-grid-block-start grip) 1439
              (org-journal-grid-block-end grip) 1440
              (org-journal-grid-block-title grip) ""
              (org-journal-grid-block-boundary-edge grip) 'top
              (org-journal-grid-block-allow-top grip) t
              (org-journal-grid-block-allow-bottom grip) nil)
        (push grip segments)))
    (nreverse segments)))

(defun org-journal-grid-day-blocks (backend absolute-day)
  "Return BACKEND blocks on ABSOLUTE-DAY, clipped to it and given lanes.
Coordinates are minutes within that day, so a range running past midnight
arrives clipped at 1440 rather than spilling into a day that is not being
drawn.  Overlaps get plain side-by-side lanes: the Week view nests a child
inside its parent, which needs more height than one compact row has.
Date-only events are omitted: compact day images have no all-day rail, and
rendering them as midnight-to-midnight timed blocks would be misleading."
  (let* ((start (* absolute-day 1440))
         (events
          (seq-remove
           #'org-journal-grid-event-all-day
           (org-journal-grid-backend-list backend start (+ start 1440))))
         (blocks
          (mapcar
           (lambda (event)
             (org-journal-grid-block-create
              :id (org-journal-grid-event-id event)
              :day 0
              :start (max 0 (- (org-journal-grid-event-start event) start))
              :end (min 1440 (- (org-journal-grid-event-end event) start))
              :time-kind 'timed
              :title (org-journal-grid-event-title event)
              :color (org-journal-grid-event-color event)
              :done (eq (org-journal-grid-event-state event) 'done)
              :event event))
           events)))
    (org-journal-grid-basic-layout-day blocks 0)))

(defun org-journal-grid--draw-edge-shadow (svg x y width direction palette)
  "Shade WIDTH pixels of SVG inward from X and Y, going DIRECTION.
DIRECTION is 1 for an edge at the top and -1 for one at the bottom.
A shadow always darkens, whatever the theme.  The usual advice is to lift
rather than darken on a dark background, but a pale band across the foot
of a dark calendar reads as a highlight, not as depth.  Stacked bands
rather than an SVG gradient: five rectangles are indistinguishable at this
size and need no gradient definition."
  (let* ((bands 5)
         (band (/ (org-journal-grid--scale-pixels
                   (float org-journal-grid-compact-shadow-pixels))
                  bands))
         (color (org-journal-grid--blend "#000000"
                                    (plist-get palette :background) 0.5)))
    (dotimes (index bands)
      (svg-rectangle svg x
                     (if (> direction 0)
                         (+ y (* index band))
                       (- y (* (1+ index) band)))
                     width band
                     :fill color
                     ;; Densest at the cut, fading inward, so content
                     ;; dissolves into the edge instead of stopping at it.
                     :fill-opacity (max 0.04 (- 0.5 (* index 0.11)))))))

(defun org-journal-grid-day-image (blocks start-minute end-minute width
                                      &optional now)
  "Return an SVG image of BLOCKS between START-MINUTE and END-MINUTE.
WIDTH is in pixels.  NOW, a minute of the day, draws a current time line.
This is a read-only strip: one day, no cursor, and no hit-test geometry,
which is what makes it safe to drop into a buffer the calendar does not
own."
  (let* ((zoom (org-journal-grid--zoom-factor))
         (scale (* org-journal-grid-compact-pixels-per-minute zoom))
         (font-size (* org-journal-grid-compact-font-size zoom))
         (line-height (+ font-size (* 2 zoom)))
         ;; Roughly the advance width of a digit at this size, which is what
         ;; decides how much of a title fits before it has to wrap.
         (character-width (* font-size 0.6))
         (span (max 60 (- end-minute start-minute)))
         (height (ceiling (* span scale)))
         (label-width
          (max (* org-journal-grid-compact-label-width zoom)
               (org-journal-grid--frame-scale-pixels
                org-journal-grid-compact-label-width)))
         (column (max 40 (- width label-width 2)))
         (palette (org-journal-grid--palette))
         (font-family (let ((family (face-attribute 'default :family nil t)))
                        (if (stringp family) family "monospace")))
         (svg (svg-create width height :stroke-width 0)))
    (svg-rectangle svg 0 0 width height
                   :fill (plist-get palette :background))
    ;; Hour rules, with the half hours a shade lighter, as in the Week view.
    (cl-loop for minute from (* 60 (ceiling start-minute 60))
             to end-minute by 30 do
             (let ((y (* (- minute start-minute) scale))
                   (hourp (zerop (% minute 60))))
               (svg-line svg label-width y width y
                         :stroke (plist-get palette
                                            (if hourp :grid :half-grid))
                         :stroke-width 1)
               (when hourp
                 (svg-text svg (org-journal-grid--format-minute minute)
                           :x 2
                           :y (min (- height (* 2 zoom))
                                   (+ y (- line-height (* 2 zoom))))
                           :font-size font-size :font-family font-family
                           :fill (plist-get palette :time-label)))))
    (dolist (block blocks)
      (let* ((lanes (max 1 (or (org-journal-grid-block-lanes block) 1)))
             (lane (or (org-journal-grid-block-lane block) 0))
             (lane-gap (* org-journal-grid--lane-gap zoom))
             (lane-width (/ (- column (* (1- lanes) lane-gap))
                            (float lanes)))
             (x (+ label-width 1 (* lane (+ lane-width lane-gap))))
             ;; Clipped to the viewport, so a block that began before it
             ;; still shows the part that has not happened yet.
             (top (max start-minute (org-journal-grid-block-start block)))
             (bottom (min end-minute (org-journal-grid-block-end block)))
             (y (* (- top start-minute) scale))
             (block-height (max 3 (- (* (- bottom top) scale)
                                     (* org-journal-grid-block-gap zoom))))
             (characters (max 1 (floor (/ (- lane-width 8) character-width))))
             (lines (unless (< (org-journal-grid-block-start block) start-minute)
                      (org-journal-grid--wrap-title
                       (org-journal-grid-block-title block) characters
                       (max 1 (floor (/ block-height line-height)))))))
        (when (> bottom top)
          (svg-rectangle svg x y lane-width block-height
                         :rx org-journal-grid-corner-radius
                         :ry org-journal-grid-corner-radius
                         :fill (org-journal-grid--color block palette))
          (svg-rectangle svg (+ x 1) (+ y 1) 2 (max 1 (- block-height 2))
                         :fill (org-journal-grid--accent block palette))
          (cl-loop for line in lines
                   for index from 0 do
                   (svg-text svg line
                             :x (+ x 6)
                             :y (+ y (- line-height 2) (* index line-height))
                             :font-size font-size :font-family font-family
                             :fill (if (org-journal-grid-block-done block)
                                       (plist-get palette :muted)
                                     (plist-get palette :foreground)))))))
    ;; Shade an edge only when the day has something past it.  A shadow over
    ;; an empty evening claims there is more to see, and there is not.
    (when (and (> start-minute 0)
               (seq-some (lambda (block)
                           (< (org-journal-grid-block-start block) start-minute))
                         blocks))
      (org-journal-grid--draw-edge-shadow svg 0 0 width 1 palette))
    (when (and (< end-minute 1440)
               (seq-some (lambda (block)
                           (> (org-journal-grid-block-end block) end-minute))
                         blocks))
      (org-journal-grid--draw-edge-shadow svg 0 height width -1 palette))
    ;; Last, so the shadows never dim the one line that says where now is.
    (when (and now (<= start-minute now end-minute))
      (let ((y (* (- now start-minute) scale)))
        (svg-line svg label-width y width y
                  :stroke (plist-get palette :red) :stroke-width 2)
        (svg-circle svg label-width y (* 3 zoom)
                    :fill (plist-get palette :red))))
    (svg-image svg :scale 1 :ascent 'center)))

(defun org-journal-grid--effective-blocks ()
  "Return per-day display segments with the current preview applied."
  (let* ((preview (org-journal-grid--calendar-state-preview org-journal-grid--state))
         (preview-block (and preview
                             (org-journal-grid--operation-block preview)))
         (timed-preview (and preview-block
                             (not (org-journal-grid-block-all-day-p
                                   preview-block))))
         (replace-id (and preview
                          timed-preview
                          (org-journal-grid--operation-replace-id preview)))
         (blocks (org-journal-grid--timed-blocks)))
    (mapcan
     #'org-journal-grid--display-segments
     (let ((display-blocks
            (if (null timed-preview)
                blocks
              (append (if replace-id
                          (cl-remove replace-id blocks
                                     :key (lambda (block)
                                            (org-journal-grid-block-id block))
                                     :test #'equal)
                        blocks)
                      (list preview-block)))))
       (if org-journal-grid--render-excluded-id
           (cl-remove org-journal-grid--render-excluded-id display-blocks
                      :key (lambda (block) (org-journal-grid-block-id block))
                      :test #'equal)
         display-blocks)))))

(defun org-journal-grid--window-width ()
  "Return the prototype window's body width in pixels."
  (if-let* ((window (get-buffer-window (current-buffer) t)))
      (window-body-width window t)
    900))

(defun org-journal-grid--ensure-state ()
  "Ensure the current prototype buffer has a usable calendar state.
Keep existing blocks when possible, but reconstruct missing date metadata."
  (unless (and (org-journal-grid--calendar-state-p org-journal-grid--state)
               (numberp (org-journal-grid--calendar-state-week-start
                         org-journal-grid--state)))
    (setq-local org-journal-grid--state
                (org-journal-grid--load-state (org-journal-grid--range-start))))
  org-journal-grid--state)

(defun org-journal-grid--draw-block
    (svg block canvas-height start-minute scale column-width palette font-family)
  "Draw BLOCK on SVG and return its hit-test geometry."
  (let* ((day (org-journal-grid-block-day block))
         (day-x (+ (org-journal-grid--label-width) (* day column-width)))
         (horizontal (org-journal-grid--layout-frame block day-x column-width))
         (x (car horizontal))
         (boundary-edge (org-journal-grid-block-boundary-edge block))
         (grip (org-journal-grid--scale-pixels
                org-journal-grid-midnight-grip-pixels))
         (raw-y (cond ((eq boundary-edge 'top)
                       (- canvas-height grip))
                      ((eq boundary-edge 'bottom)
                       (org-journal-grid--grid-top-inset))
                      (t (+ (org-journal-grid--grid-top-inset)
                            (* (- (org-journal-grid-block-start block) start-minute)
                               scale)))))
         (raw-height
          (if boundary-edge
              (+ grip (org-journal-grid--scale-pixels org-journal-grid-block-gap))
            (* (- (org-journal-grid-block-end block) (org-journal-grid-block-start block)) scale)))
         (gap (org-journal-grid--scale-pixels org-journal-grid-block-gap))
         (y (+ raw-y (/ gap 2.0)))
         (block-height (max 4 (- raw-height gap)))
         (block-width (max 4 (- (cdr horizontal) 1)))
         (selected (and (not org-journal-grid--static-render)
                        (not (org-journal-grid-block-preview block))
                        (equal (org-journal-grid-block-id block)
                               (org-journal-grid--selected-id))))
         (fill (org-journal-grid--color block palette))
         (accent (org-journal-grid--accent block palette))
         (small-font (org-journal-grid--font-size 8))
         (font-size (if (< block-height (org-journal-grid--scale-pixels 13))
                        small-font
                      (org-journal-grid--font-size 10)))
         (line-height (if (= font-size small-font)
                          (org-journal-grid--scale-pixels 10)
                        (org-journal-grid--scale-pixels 13)))
         (characters (max 1 (floor (/ (- block-width 12)
                                      (* font-size 0.62)))))
         (max-lines (max 1 (floor (/ (max 1 (- block-height 3))
                                     line-height))))
         (title-lines (org-journal-grid--wrap-title
                       (org-journal-grid-block-title block) characters max-lines))
         (clip-id (format "occs-block-%s-%x" day
                          (sxhash (org-journal-grid-block-id block))))
         (clip (svg-clip-path svg :id clip-id))
         (radius (max 0 org-journal-grid-corner-radius))
         (accent-radius (min 1.5 (/ radius 2.0))))
    (svg-rectangle clip x y block-width block-height :rx radius :ry radius)
    (svg-rectangle svg x y block-width block-height
                   :rx radius :ry radius :fill fill
                   :fill-opacity (if (org-journal-grid-block-preview block) 0.72 1)
                   :stroke (if selected
                               (plist-get palette :blue)
                             (plist-get palette :background))
                   :stroke-width (if selected 2 1))
    (svg-rectangle svg (+ x 2) (+ y 2) 3 (max 1 (- block-height 4))
                   :rx accent-radius :ry accent-radius :fill accent)
    (cl-loop for line in (unless boundary-edge title-lines)
             for index from 0 do
             (svg-text svg line :x (+ x 9)
                       :y (+ y (min (- block-height 1)
                                    (+ (if (= font-size small-font)
                                           (org-journal-grid--scale-pixels 8)
                                         (org-journal-grid--scale-pixels 10))
                                       (* index line-height))))
                       :font-size font-size :font-weight "600"
                       :font-family font-family
                       :clip-path (format "url(#%s)" clip-id)
                       :fill (if (org-journal-grid-block-done block)
                                 (plist-get palette :muted)
                               (plist-get palette :foreground))))
    (when (and (not boundary-edge) (< (length title-lines) max-lines))
      (svg-text svg
                (org-journal-grid--format-range
                 (or (org-journal-grid-block-source-start block) (org-journal-grid-block-start block))
                 (or (org-journal-grid-block-source-end block) (org-journal-grid-block-end block)))
                :x (+ x 9)
                :y (+ y (org-journal-grid--scale-pixels 10)
                      (* (length title-lines) line-height))
                :font-size (org-journal-grid--font-size 9) :font-family font-family
                :clip-path (format "url(#%s)" clip-id)
                :fill (plist-get palette :secondary-text)))
    (list :id (org-journal-grid-block-id block) :day day
          :start (org-journal-grid-block-start block) :end (org-journal-grid-block-end block)
          :x x :y y :width block-width :height block-height
          :preview (org-journal-grid-block-preview block)
          :allow-top (org-journal-grid-block-allow-top block)
          :allow-bottom (org-journal-grid-block-allow-bottom block)
          :boundary-edge boundary-edge)))

(defun org-journal-grid--draw-current-time
    (svg width height column-width palette font-family start-minute scale)
  "Draw the current time marker on SVG with WIDTH and PALETTE.
HEIGHT, COLUMN-WIDTH, FONT-FAMILY, START-MINUTE and SCALE determine its
geometry.  Return the marker's Y coordinate."
  (let* ((now (decode-time))
         (today (calendar-absolute-from-gregorian (calendar-current-date)))
         (today-day (- today (org-journal-grid--calendar-state-week-start org-journal-grid--state)))
         (now-minute (+ (* 60 (decoded-time-hour now))
                        (decoded-time-minute now))))
    (when (and (<= 0 today-day (org-journal-grid--last-day-index))
               (<= start-minute now-minute
                   (* 60 org-journal-grid-end-hour)))
      (let* ((y (+ (org-journal-grid--grid-top-inset)
                   (* (- now-minute start-minute) scale)))
             (today-x (+ (org-journal-grid--label-width)
                         (* today-day column-width)))
             (label (format "%02d:%02d"
                            (decoded-time-hour now)
                            (decoded-time-minute now)))
             (bubble-width (org-journal-grid--scale-pixels 36))
             (bubble-height (org-journal-grid--scale-pixels 14))
             (bubble-x (/ (- (org-journal-grid--label-width) bubble-width) 2.0))
             (bubble-right (+ bubble-x bubble-width))
             (today-line-x (if (= today-day 0) bubble-right today-x))
             (edge-inset (org-journal-grid--scale-pixels 1))
             (bubble-y (max edge-inset
                            (min (- height bubble-height edge-inset)
                                   (- y (/ bubble-height 2.0))))))
        (svg-line svg bubble-right y width y
                  :stroke (plist-get palette :red) :stroke-width 2
                  :stroke-opacity 0.16)
        (svg-line svg today-line-x y (+ today-x column-width) y
                  :stroke (plist-get palette :red) :stroke-width 2)
        (when (> today-day 0)
          (svg-circle svg today-x y 4 :fill (plist-get palette :red)))
        (svg-rectangle svg bubble-x bubble-y bubble-width bubble-height
                       :rx (org-journal-grid--scale-pixels 7)
                       :ry (org-journal-grid--scale-pixels 7)
                       :fill (plist-get palette :red))
        (svg-text svg label :x (/ (org-journal-grid--label-width) 2.0)
                  :y (+ bubble-y (org-journal-grid--scale-pixels 10))
                  :font-size (org-journal-grid--font-size 8) :font-weight "600"
                  :font-family font-family :text-anchor "middle"
                  :fill "#ffffff")
        y))))

(defun org-journal-grid--svg ()
  "Build the calendar SVG and update hit-test geometry."
  (org-journal-grid--ensure-state)
  (let* ((width (max 560 (org-journal-grid--window-width)))
         (start-minute (* 60 org-journal-grid-start-hour))
         (end-minute (* 60 org-journal-grid-end-hour))
         (scale (org-journal-grid--pixels-per-minute))
         (height (+ (org-journal-grid--grid-top-inset)
                    (ceiling (* (- end-minute start-minute) scale))))
         (column-width (/ (- width (org-journal-grid--label-width))
                          (float org-journal-grid-days)))
         (svg (svg-create width height :stroke-width 0))
         (palette (org-journal-grid--palette))
         (font-family (let ((family (face-attribute 'default :family nil t)))
                        (if (stringp family) family "monospace")))
         (week-start (org-journal-grid--calendar-state-week-start org-journal-grid--state))
         (today-column
          (- (calendar-absolute-from-gregorian (calendar-current-date))
             week-start))
         (blocks (org-journal-grid--effective-blocks))
         (day-blocks (make-vector org-journal-grid-days nil))
         geometry)
    (setq-local org-journal-grid--image-height height)
    (svg-rectangle svg 0 0 width height :fill (plist-get palette :background))
    (svg-rectangle svg 0 0 (org-journal-grid--label-width) height
                   :fill (plist-get palette :time-background))
    (dotimes (day org-journal-grid-days)
      (let* ((x (+ (org-journal-grid--label-width)
                   (* day column-width)))
             (weekday
              (calendar-day-of-week
               (calendar-gregorian-from-absolute (+ week-start day)))))
        (svg-rectangle svg x 0 column-width height
                       :fill (cond ((= day today-column) (plist-get palette :today))
                                   ((memq weekday '(0 6))
                                    (plist-get palette :weekend))
                                   (t (plist-get palette :background))))
        (svg-line svg x 0 x height
                  :stroke (plist-get palette :grid) :stroke-width 1)
        (aset day-blocks day
              (org-journal-grid--layout-day blocks day))))
    (svg-line svg width 0 width height
              :stroke (plist-get palette :grid) :stroke-width 1)
    (cl-loop for minute from start-minute to end-minute by 30 do
             (let* ((y (+ (org-journal-grid--grid-top-inset)
                          (* (- minute start-minute) scale)))
                    (hourp (= (% minute 60) 0)))
               (svg-line svg (org-journal-grid--label-width) y
                         width y
                         :stroke (if hourp
                                     (plist-get palette :grid)
                                   (plist-get palette :half-grid))
                         :stroke-width 1)
               (when (and hourp (< minute end-minute))
                 (svg-text svg (format "%02d:00" (/ minute 60))
                           :x 5 :y (+ y (org-journal-grid--scale-pixels 11))
                           :font-size (org-journal-grid--font-size 10)
                           :font-family font-family
                           :fill (plist-get palette :time-label)))))
    (dotimes (day org-journal-grid-days)
      (dolist (block (aref day-blocks day))
        (push (org-journal-grid--draw-block
               svg block height start-minute scale column-width
               palette font-family)
              geometry)))
    ;; The keyboard cursor is one fifteen-minute slot drawn over the blocks.
    ;; Its fill is translucent so a block underneath stays readable, and it
    ;; is drawn only while visible.
    (when-let* (((not org-journal-grid--static-render))
                ((org-journal-grid--cursor-visible-p))
                ((eq (org-journal-grid--cursor-state-surface
                      (org-journal-grid--cursor))
                     'grid))
                ;; A selected block already draws its own outline, and two
                ;; borders around one slot read as a bug.
                ((null (org-journal-grid--selected-id)))
                (cursor (org-journal-grid--cursor))
                (cursor-minute (org-journal-grid--cursor-state-minute cursor)))
      (when (<= start-minute cursor-minute (- end-minute
                                              org-journal-grid-slot-minutes))
        ;; A cursor that selects a block narrows to that block's lane, which
        ;; is what makes one lane among several visible.  Otherwise it spans
        ;; the whole day column.
        (let* ((rectangle (org-journal-grid--cursor-rectangle geometry))
               (x (plist-get rectangle :x))
               (width (plist-get rectangle :width))
               (y (+ (org-journal-grid--grid-top-inset)
                     (* (- cursor-minute start-minute) scale))))
          (svg-rectangle svg x y width
                         (* org-journal-grid-slot-minutes scale)
                         :fill (plist-get palette :cursor)
                         :fill-opacity org-journal-grid-cursor-opacity
                         :stroke (plist-get palette :cursor)
                         :stroke-width 1
                         :rx org-journal-grid-corner-radius))))
    (unless org-journal-grid--static-render
      (org-journal-grid--draw-current-time
       svg width height column-width palette font-family start-minute scale))
    (setq-local org-journal-grid--geometry geometry)
    svg))

(defun org-journal-grid--svg-inner-xml (svg)
  "Return SVG's serialized child elements."
  (with-temp-buffer
    (svg-print svg)
    (let* ((xml (buffer-string))
           (start (1+ (string-match ">" xml)))
           (end (string-match "</svg>\\'" xml)))
      (substring xml start end))))

(defun org-journal-grid--update-clock-fragment ()
  "Rebuild the current time fragment and remember its overlapping tiles."
  (let* ((svg (svg-create org-journal-grid--tile-width org-journal-grid--image-height
                          :stroke-width 0))
         (palette (org-journal-grid--palette))
         (font-family (let ((family (face-attribute 'default :family nil t)))
                        (if (stringp family) family "monospace")))
         (start-minute (* 60 org-journal-grid-start-hour))
         (column-width (/ (- org-journal-grid--tile-width
                             (org-journal-grid--label-width))
                          (float org-journal-grid-days)))
         (y (org-journal-grid--draw-current-time
             svg org-journal-grid--tile-width org-journal-grid--image-height
             column-width palette font-family start-minute
             (org-journal-grid--pixels-per-minute))))
    (setq-local org-journal-grid--clock-fragment
                (and y (org-journal-grid--svg-inner-xml svg))
                org-journal-grid--clock-tiles
                (and y (org-journal-grid--tiles-intersecting (- y 7) (+ y 7))))))

(defun org-journal-grid--tile-bounds (tile)
  "Return TILE's (TOP . HEIGHT) inside the canvas, in canvas pixels.
The last tile takes whatever is left over.  A leftover of a few pixels
would still take a whole text line on screen, and that looks like a blank
band under the grid."
  (let* ((top (* tile org-journal-grid--tile-height))
         (lastp (>= tile (1- org-journal-grid--tile-count)))
         (height (if lastp
                     (- org-journal-grid--image-height top)
                   org-journal-grid--tile-height)))
    (cons top (max 0 height))))

(defun org-journal-grid--tile-at-pixel (pixel)
  "Return the tile containing canvas PIXEL, clamped to the canvas."
  (when (and (numberp org-journal-grid--tile-count)
             (> org-journal-grid--tile-count 0))
    (let ((pixel (max 0 (min pixel (1- org-journal-grid--image-height)))))
      (cl-loop for tile from 0 below org-journal-grid--tile-count
               for bounds = (org-journal-grid--tile-bounds tile)
               when (< pixel (+ (car bounds) (cdr bounds)))
               return tile
               finally return (1- org-journal-grid--tile-count)))))

(defun org-journal-grid--tiles-intersecting (top bottom)
  "Return the tiles intersecting the canvas interval TOP to BOTTOM."
  (let ((top (max 0 top))
        (bottom (min org-journal-grid--image-height bottom)))
    (when (< top bottom)
      (cl-loop for tile from 0 below org-journal-grid--tile-count
               for bounds = (org-journal-grid--tile-bounds tile)
               for tile-top = (car bounds)
               for tile-bottom = (+ tile-top (cdr bounds))
               when (and (< tile-top bottom) (< top tile-bottom))
               collect tile))))

(defun org-journal-grid--tile-xml (tile &optional fragment)
  "Return the complete SVG document for TILE and dynamic FRAGMENT."
  (let* ((bounds (org-journal-grid--tile-bounds tile))
         (y (car bounds))
         (height (cdr bounds)))
    (format
     (concat "<svg width=\"%s\" height=\"%s\" viewBox=\"0 %s %s %s\" "
             "version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" "
             "xmlns:xlink=\"http://www.w3.org/1999/xlink\">%s%s</svg>")
     org-journal-grid--tile-width height y org-journal-grid--tile-width height
     org-journal-grid--static-inner (or fragment ""))))

(defun org-journal-grid--tile-image-map (tile)
  "Return the production image map clipped and translated for TILE."
  (let* ((bounds (org-journal-grid--tile-bounds tile))
         (top (car bounds))
         (bottom (min org-journal-grid--image-height (+ top (cdr bounds))))
         translated)
    (dolist (entry (org-journal-grid--image-map))
      (pcase-let* ((`(,shape ,id ,properties) entry)
                   (`(rect . ((,left . ,y) . (,right . ,end))) shape))
        (when (and (< y bottom) (> end top))
          (push (list `(rect . ((,left . ,(max 0 (- y top)))
                                . (,right . ,(- (min end bottom) top))))
                      id properties)
                translated))))
    (nreverse translated)))

(defun org-journal-grid--make-tile-image (tile &optional fragment)
  "Create TILE's image, adding optional dynamic SVG FRAGMENT."
  (let ((map (org-journal-grid--tile-image-map tile))
        ;; The current-time marker is calendar chrome, so paint it after
        ;; selection and preview fragments instead of letting those cover it.
        (fragment (concat fragment
                          (and (memq tile org-journal-grid--clock-tiles)
                               org-journal-grid--clock-fragment))))
    ;; Pass `:scale 1' like the header image does.  Without it Emacs scales
    ;; the tile by `image-scaling-factor', which is `char-width / 10' by
    ;; default.  The header would keep its size and the grid would not, so the
    ;; columns drift away from the day names.
    (if map
        (create-image (org-journal-grid--tile-xml tile fragment) 'svg t
                      :ascent 90 :scale 1 :map map :original-map map)
      (create-image (org-journal-grid--tile-xml tile fragment) 'svg t
                    :ascent 90 :scale 1))))

(defun org-journal-grid--cache-static-tiles (&optional excluded-id)
  "Cache hour tiles, omitting EXCLUDED-ID from their static layer."
  (let ((org-journal-grid--static-render t)
        (org-journal-grid--render-excluded-id excluded-id)
        (org-journal-grid--state
         (copy-org-journal-grid--calendar-state org-journal-grid--state)))
    (setf (org-journal-grid--calendar-state-preview org-journal-grid--state) nil
          (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state) nil)
    (let ((svg (org-journal-grid--svg)))
      (setq-local org-journal-grid--static-inner (org-journal-grid--svg-inner-xml svg)
                  org-journal-grid--tile-width (dom-attr svg 'width)
                  org-journal-grid--tile-height
                  (ceiling (* 60 (org-journal-grid--pixels-per-minute)))
                  org-journal-grid--tile-count
                  ;; Round down: the remainder goes to the last tile in
                  ;; `org-journal-grid--tile-bounds' instead of becoming a tile of
                  ;; its own that is only a few pixels tall.
                  (max 1 (floor org-journal-grid--image-height
                                (ceiling (* 60 (org-journal-grid--pixels-per-minute)))))
                  org-journal-grid--base-excluded-id excluded-id)))
  (org-journal-grid--update-clock-fragment)
  (setq-local org-journal-grid--static-images
              (make-vector org-journal-grid--tile-count nil))
  (dotimes (tile org-journal-grid--tile-count)
    (aset org-journal-grid--static-images tile
          (org-journal-grid--make-tile-image tile))))

(defun org-journal-grid--dynamic-blocks ()
  "Return laid-out blocks belonging to the current dynamic layer."
  (let* ((preview (org-journal-grid--calendar-state-preview org-journal-grid--state))
         (selected (and (null preview) (org-journal-grid--selected-id)))
         (blocks (org-journal-grid--effective-blocks)))
    (when (or preview selected)
      (cl-loop for day from 0 below org-journal-grid-days
               append
               (cl-remove-if-not
                (lambda (block)
                  (and (not (org-journal-grid-block-boundary-edge block))
                       (if preview
                           (org-journal-grid-block-preview block)
                         (equal (org-journal-grid-block-id block) selected))))
                (org-journal-grid--layout-day blocks day))))))

(defun org-journal-grid--geometry-tiles (geometry &optional margin)
  "Return tiles intersected by GEOMETRY and its visual MARGIN."
  (let ((margin (or margin 0))
        (top (plist-get geometry :y)))
    (org-journal-grid--tiles-intersecting
     (- top margin)
     (+ top (plist-get geometry :height) margin))))

(defun org-journal-grid--dynamic-fragment ()
  "Return dynamic SVG XML followed by the tiles it intersects."
  (let* ((svg (svg-create org-journal-grid--tile-width org-journal-grid--image-height
                          :stroke-width 0))
         (palette (org-journal-grid--palette))
         (font-family (let ((family (face-attribute 'default :family nil t)))
                        (if (stringp family) family "monospace")))
         (start-minute (* 60 org-journal-grid-start-hour))
         (column-width (/ (- org-journal-grid--tile-width
                             (org-journal-grid--label-width))
                          (float org-journal-grid-days)))
         (preview (org-journal-grid--calendar-state-preview org-journal-grid--state))
         (selected (and (null preview) (org-journal-grid--selected-id)))
         geometry tiles)
    (dolist (block (org-journal-grid--dynamic-blocks))
      (let ((item (org-journal-grid--draw-block
                   svg block org-journal-grid--image-height start-minute
                   (org-journal-grid--pixels-per-minute) column-width
                   palette font-family)))
        (push item geometry)
        ;; Selected blocks have a two-pixel outline.  Include its full visual
        ;; bounds when it crosses an SVG tile edge.
        (dolist (tile (org-journal-grid--geometry-tiles item 1))
          (cl-pushnew tile tiles))))
    (when (and (null preview) (org-journal-grid--cursor-visible-p)
               (eq (org-journal-grid--cursor-state-surface
                    (org-journal-grid--cursor))
                   'grid)
               (null selected))
      (when-let* ((rectangle (org-journal-grid--cursor-rectangle
                             org-journal-grid--geometry)))
        (svg-rectangle svg
                       (plist-get rectangle :x) (plist-get rectangle :y)
                       (plist-get rectangle :width) (plist-get rectangle :height)
                       :fill (plist-get palette :cursor)
                       :fill-opacity org-journal-grid-cursor-opacity
                       :stroke (plist-get palette :cursor) :stroke-width 1
                       :rx org-journal-grid-corner-radius)
        ;; SVG strokes are centered on their path, so the cursor extends half
        ;; a pixel beyond its geometric rectangle on every side.
        (dolist (tile (org-journal-grid--geometry-tiles rectangle 0.5))
          (cl-pushnew tile tiles))))
    (cons (org-journal-grid--svg-inner-xml svg) (sort tiles #'<))))

(defun org-journal-grid--set-tile-image (tile image)
  "Display IMAGE at TILE's buffer marker."
  (let ((marker (aref org-journal-grid--tile-markers tile))
        (inhibit-read-only t))
    (put-text-property marker (1+ marker) 'display image)))

(defun org-journal-grid--render-dynamic (&optional redisplay-now)
  "Redraw tiles touched by cursor selection or a drag preview.
When REDISPLAY-NOW is non-nil, force display before returning."
  (let* ((preview (org-journal-grid--calendar-state-preview org-journal-grid--state))
         (excluded (and preview (org-journal-grid--operation-replace-id preview))))
    (when (and (vectorp org-journal-grid--tile-markers)
               (not (equal excluded org-journal-grid--base-excluded-id)))
      (let* ((window (get-buffer-window (current-buffer) t))
             (scroll (and window
                          (org-journal-grid--window-scroll-pixels window))))
        (org-journal-grid--cache-static-tiles excluded)
        (org-journal-grid--insert-tiles)
        (when window (org-journal-grid--set-vscroll window scroll)))))
  (pcase-let* ((`(,fragment . ,new-tiles) (org-journal-grid--dynamic-fragment))
               (changed (delete-dups
                         (append new-tiles org-journal-grid--dynamic-tiles))))
    (dolist (tile changed)
      (org-journal-grid--set-tile-image
       tile
       (if (memq tile new-tiles)
           (org-journal-grid--make-tile-image tile fragment)
         (aref org-journal-grid--static-images tile))))
    (setq-local org-journal-grid--dynamic-tiles new-tiles)
    (set-buffer-modified-p nil)
    (when redisplay-now (redisplay t))))

(defun org-journal-grid--insert-tiles ()
  "Replace the buffer with the cached hour tiles."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local org-journal-grid--tile-markers
                (make-vector org-journal-grid--tile-count nil))
    (dotimes (tile org-journal-grid--tile-count)
      (let ((start (point)))
        (aset org-journal-grid--tile-markers tile start)
        (insert (propertize " "
                            'org-journal-grid-tile tile
                            'occs-svg-image t
                            'display (aref org-journal-grid--static-images tile)))
        ;; No newline after the last tile.  It would leave an empty final
        ;; line, which shows up as blank space under the calendar.
        (when (< tile (1- org-journal-grid--tile-count))
          (insert "\n"))))
    (setq-local org-journal-grid--dynamic-tiles nil)
    (set-buffer-modified-p nil)))

(defun org-journal-grid--all-day-less-p (left right)
  "Return non-nil when all-day block LEFT should sort before RIGHT."
  (let* ((ls (+ (* (org-journal-grid-block-day left) 1440) (org-journal-grid-block-start left)))
         (rs (+ (* (org-journal-grid-block-day right) 1440) (org-journal-grid-block-start right)))
         (le (+ (* (org-journal-grid-block-day left) 1440) (org-journal-grid-block-end left)))
         (re (+ (* (org-journal-grid-block-day right) 1440) (org-journal-grid-block-end right)))
         (lt (or (org-journal-grid-block-title left) ""))
         (rt (or (org-journal-grid-block-title right) "")))
    (cond ((/= ls rs) (< ls rs))
          ((/= (- le ls) (- re rs)) (> (- le ls) (- re rs)))
          ((not (equal lt rt)) (string-lessp lt rt))
          (t (string-lessp (prin1-to-string (org-journal-grid-block-id left))
                           (prin1-to-string (org-journal-grid-block-id right)))))))

(defun org-journal-grid--all-day-layout ()
  "Return visible all-day blocks annotated with stable rail lanes.
The ordering favours earlier and longer spans, then title and identity."
  (let* ((week-end (* org-journal-grid-days 1440))
         (preview (org-journal-grid--calendar-state-preview org-journal-grid--state))
         (preview-block (and preview (org-journal-grid--operation-block preview)))
         (replace-id (and preview
                          (org-journal-grid--operation-replace-id preview)))
         (source
          (append
           (if replace-id
               (cl-remove replace-id (org-journal-grid--all-day-blocks)
                          :key #'org-journal-grid-block-id
                          :test #'equal)
             (org-journal-grid--all-day-blocks))
           (and preview-block
                (org-journal-grid-block-all-day-p preview-block)
                (list preview-block))))
         (blocks (sort (mapcar #'copy-org-journal-grid-block source)
                       #'org-journal-grid--all-day-less-p))
         lane-ends result)
    (dolist (block blocks (nreverse result))
      (let* ((real-start (+ (* (org-journal-grid-block-day block) 1440)
                            (org-journal-grid-block-start block)))
             (real-end (+ (* (org-journal-grid-block-day block) 1440)
                          (org-journal-grid-block-end block)))
             (start (max 0 real-start))
             (end (min week-end real-end))
             (lane 0))
        (while (and (< lane (length lane-ends))
                    (> (nth lane lane-ends) start))
          (setq lane (1+ lane)))
        (if (= lane (length lane-ends))
            (setq lane-ends (append lane-ends (list end)))
          (setf (nth lane lane-ends) end))
        (setf (org-journal-grid-block-rail-start block) start
              (org-journal-grid-block-rail-end block) end
              (org-journal-grid-block-continues-left block) (< real-start 0)
              (org-journal-grid-block-continues-right block) (> real-end week-end)
              (org-journal-grid-block-rail-lane block) lane)
        (push block result)))))

(defun org-journal-grid--rail-row-count (layout)
  "Return the visible rail row count needed for all-day LAYOUT.
Return 0 when LAYOUT is empty so only date labels remain.  When events
exist, the final row is intentionally empty and can hold a cross-surface
preview."
  (if (null layout)
      0
    (let ((highest-lane (apply #'max
                               (mapcar #'org-journal-grid-block-rail-lane layout))))
      (1+ (min org-journal-grid-all-day-max-lanes (1+ highest-lane))))))

(defun org-journal-grid--draw-all-day-block
    (svg block left-offset column-width top palette font-family)
  "Draw one laid-out all-day BLOCK into SVG rail at TOP."
  (let* ((start-day (/ (org-journal-grid-block-rail-start block) 1440.0))
         (end-day (/ (org-journal-grid-block-rail-end block) 1440.0))
         (x (+ left-offset (org-journal-grid--label-width) (* start-day column-width) 2))
         (right (+ left-offset (org-journal-grid--label-width) (* end-day column-width) -2))
         (y (+ top 2))
         (height (- (org-journal-grid--all-day-lane-height)
                    (org-journal-grid--scale-pixels 4)))
         (arrow (min 9 (/ (- right x) 3.0)))
         (leftp (org-journal-grid-block-continues-left block))
         (rightp (org-journal-grid-block-continues-right block))
         (fill (org-journal-grid--color block palette))
         (selected (equal (org-journal-grid-block-id block) (org-journal-grid--selected-id)))
         (radius (min (max 0 org-journal-grid-corner-radius)
                      (/ height 2.0)))
         (mid (+ y (/ height 2.0)))
         ;; A single silhouette keeps the selection stroke on the outside of
         ;; continuation arrowheads.  Drawing the body and arrows separately
         ;; both exposed antialiasing seams and made the selected rectangle
         ;; cross through the arrow.
         (path
          (concat
           (if leftp
               (format "M %g %g L %g %g" x mid (+ x arrow) y)
             (format "M %g %g" (+ x radius) y))
           (if rightp
               (format " L %g %g L %g %g L %g %g"
                       (- right arrow) y right mid
                       (- right arrow) (+ y height))
             (format " L %g %g Q %g %g %g %g L %g %g Q %g %g %g %g"
                     (- right radius) y
                     right y right (+ y radius)
                     right (- (+ y height) radius)
                     right (+ y height) (- right radius) (+ y height)))
           (if leftp
               (format " L %g %g L %g %g Z" (+ x arrow) (+ y height) x mid)
             (format " L %g %g Q %g %g %g %g L %g %g Q %g %g %g %g Z"
                     (+ x radius) (+ y height)
                     x (+ y height) x (- (+ y height) radius)
                     x (+ y radius)
                     x y (+ x radius) y)))))
    (svg-node svg 'path :d path :fill fill
              :stroke (if selected
                          (plist-get palette :blue)
                        (plist-get palette :background))
              :stroke-width (if selected 2 1)
              :stroke-linejoin "round")
    (let* ((text-x (+ x (if leftp arrow 0) 9))
           (available (max 1 (- right text-x 5)))
           (characters (max 1 (floor (/ available
                                        (org-journal-grid--scale-pixels 6.2)))))
           (title (truncate-string-to-width
                   (org-journal-grid-block-title block) characters nil nil "…")))
      (svg-text svg title :x text-x
                :y (+ y (org-journal-grid--scale-pixels 13))
                :font-size (org-journal-grid--font-size 10)
                :font-weight "600" :font-family font-family
                :fill (plist-get palette :foreground)))
    (list :id (org-journal-grid-block-id block) :lane (org-journal-grid-block-rail-lane block)
          :x x :y y :width (- right x) :height height
          :allow-left (not leftp) :allow-right (not rightp))))

(defun org-journal-grid--rail-edge-at (geometry x)
  "Return resize endpoint for rail GEOMETRY at horizontal pixel X.
The operation model calls the start and end endpoints `top' and `bottom';
the rail presents those same endpoints as its left and right edges."
  (let* ((left (plist-get geometry :x))
         (right (+ left (plist-get geometry :width)))
         (edge (min org-journal-grid-edge-pixels
                    (max 1 (/ (plist-get geometry :width) 2.0)))))
    (cond ((and (plist-get geometry :allow-left)
                (<= left x (+ left edge)))
           'top)
          ((and (plist-get geometry :allow-right)
                (<= (- right edge) x right))
           'bottom))))

(defun org-journal-grid--header-image-map ()
  "Return move and horizontal-resize hotspots for all-day blocks."
  (let (edges bodies)
    (dolist (geometry org-journal-grid--header-geometry)
      (let* ((x (round (plist-get geometry :x)))
             (y (round (plist-get geometry :y)))
             (right (round (+ (plist-get geometry :x)
                              (plist-get geometry :width))))
             (bottom (round (+ (plist-get geometry :y)
                               (plist-get geometry :height))))
             (edge (min org-journal-grid-edge-pixels
                        (max 1 (floor (/ (- right x) 2.0))))))
        (when (plist-get geometry :allow-left)
          (push (list `(rect . ((,x . ,y) . (,(+ x edge) . ,bottom)))
                      'calendar-rail-resize
                      '(pointer hdrag help-echo "Drag to change the first day"))
                edges))
        (when (plist-get geometry :allow-right)
          (push (list `(rect . ((,(- right edge) . ,y) . (,right . ,bottom)))
                      'calendar-rail-resize
                      '(pointer hdrag help-echo "Drag to change the last day"))
                edges))
        (push (list `(rect . ((,x . ,y) . (,right . ,bottom)))
                    'calendar-rail-block
                    '(pointer hand help-echo "Drag to move; double-click to open"))
              bodies)))
    (append edges bodies)))

(defun org-journal-grid--header-month-parts (week-start)
  "Return the visible month title for WEEK-START as (PRIMARY SECONDARY).
SECONDARY is the final year and is drawn with lighter weight.  PRIMARY
includes both month names when the visible week crosses a boundary."
  (let* ((first (calendar-gregorian-from-absolute week-start))
         (last (calendar-gregorian-from-absolute
                (+ week-start (org-journal-grid--last-day-index))))
         (first-month (calendar-month-name (nth 0 first)))
         (last-month (calendar-month-name (nth 0 last)))
         (first-year (nth 2 first))
         (last-year (nth 2 last)))
    (cond
     ((and (= (nth 0 first) (nth 0 last)) (= first-year last-year))
      (list first-month (number-to-string first-year)))
     ((= first-year last-year)
      (list (format "%s–%s" first-month last-month)
            (number-to-string first-year)))
     (t
      (list (format "%s %d–%s" first-month first-year last-month)
            (number-to-string last-year))))))

(defun org-journal-grid--iso-week-format (absolute-date format)
  "Format ABSOLUTE-DATE as an ISO week using time FORMAT."
  (pcase-let ((`(,month ,day ,year)
               (calendar-gregorian-from-absolute absolute-date)))
    (format-time-string format (encode-time 0 0 12 day month year))))

(defun org-journal-grid--week-label (absolute-date)
  "Return the ISO week label that best represents the visible week.
ABSOLUTE-DATE is the first displayed day."
  (org-journal-grid--iso-week-format
   (+ absolute-date (floor org-journal-grid-days 2)) "W%V"))

(defun org-journal-grid--week-current-p (week-start today)
  "Return non-nil when the range starting at WEEK-START includes TODAY."
  (<= week-start today (+ week-start (org-journal-grid--last-day-index))))

(defun org-journal-grid--header ()
  "Return a pixel-aligned SVG header for the calendar."
  (org-journal-grid--ensure-state)
  (let* ((window (get-buffer-window (current-buffer) t))
         (left-offset (if window (or (car (window-fringes window)) 0) 0))
         (canvas-width
          (max 560 (org-journal-grid--window-width)))
         (width (+ left-offset canvas-width))
         (all-day (org-journal-grid--all-day-layout))
         (rail-rows (or org-journal-grid--drag-rail-rows
                        (org-journal-grid--rail-row-count all-day)))
         (event-rows (1- rail-rows))
         (header-title-height (org-journal-grid--header-title-height))
         (date-factor (org-journal-grid--frame-font-factor window))
         (rail-top (org-journal-grid--rail-top))
         (lane-height (org-journal-grid--all-day-lane-height))
         (height (+ rail-top (* rail-rows lane-height)))
         (column-width (/ (- canvas-width
                             (org-journal-grid--label-width))
                          (float org-journal-grid-days)))
         (palette (org-journal-grid--palette))
         (font-family (let ((family (face-attribute 'default :family nil t)))
                        (if (stringp family) family "monospace")))
         (week-start (org-journal-grid--calendar-state-week-start org-journal-grid--state))
         (today (calendar-absolute-from-gregorian (calendar-current-date)))
         (month-parts (org-journal-grid--header-month-parts week-start))
         (month-title (car month-parts))
         (year-title (cadr month-parts))
         (title-x (+ left-offset 8))
         (svg (svg-create width height :stroke-width 0))
         geometry)
    (svg-rectangle svg 0 0 width height
                   :fill (plist-get palette :time-background))
    (svg-text svg month-title :x title-x :y 35
              :font-size 24 :font-weight "700"
              :font-family font-family :fill (plist-get palette :foreground))
    (svg-text svg year-title
              :x (+ title-x (* 14.6 (string-width month-title)) 8)
              :y 35 :font-size 24 :font-weight "300"
              :font-family font-family
              :fill (plist-get palette :secondary-text))
    (svg-text svg (org-journal-grid--week-label week-start)
              :x (+ left-offset 7)
              :y (+ header-title-height (* date-factor 20))
              :font-size (* date-factor 14) :font-family font-family
              :fill (plist-get palette
                               (if (org-journal-grid--week-current-p week-start today)
                                   :red
                                 :secondary-text)))
    (dotimes (day org-journal-grid-days)
      (let* ((absolute (+ week-start day))
             (date (calendar-gregorian-from-absolute absolute))
             (day-name (calendar-day-name date t))
             (day-number (number-to-string (nth 1 date)))
             (x (+ left-offset
                   (org-journal-grid--label-width)
                   (* day column-width)))
             (center (+ x (/ column-width 2.0)))
             (baseline (+ header-title-height (* date-factor 20))))
        (if (= absolute today)
            (let* ((name-width (* date-factor 8.0
                                  (string-width day-name)))
                   (circle-radius (* date-factor 10))
                   (gap (* date-factor 5))
                   (group-width (+ name-width gap (* 2 circle-radius)))
                   (name-x (- center (/ group-width 2.0)))
                   (number-x (+ name-x name-width gap circle-radius)))
              (svg-text svg day-name :x name-x :y baseline
                        :font-size (* date-factor 14) :font-weight "500"
                        :font-family font-family
                        :fill (plist-get palette :foreground))
              (svg-circle svg number-x (- baseline (* date-factor 4))
                          circle-radius
                          :fill (plist-get palette :red))
              (svg-text svg day-number :x number-x :y baseline
                        :font-size (* date-factor 14) :font-weight "700"
                        :font-family font-family :text-anchor "middle"
                        :fill "#ffffff"))
          (svg-text svg (format "%s %s" day-name day-number)
                    :x center :y baseline
                    :font-size (* date-factor 14) :font-weight "500"
                    :font-family font-family :text-anchor "middle"
                    :fill (plist-get palette :foreground)))))
    (when (> rail-rows 0)
      (svg-line svg 0 (1- rail-top)
                width (1- rail-top)
                :stroke (plist-get palette :grid) :stroke-width 1)
      (svg-text svg "all-day" :x (+ left-offset 5)
                :y (+ rail-top (org-journal-grid--scale-pixels 15))
                :font-size (org-journal-grid--font-size 9) :font-family font-family
                :fill (plist-get palette :secondary-text))
      (dotimes (day org-journal-grid-days)
        (let ((x (+ left-offset (org-journal-grid--label-width) (* day column-width))))
          (svg-line svg x rail-top x height
                    :stroke (plist-get palette :grid) :stroke-width 1))))
    (dolist (block all-day)
      (when (< (org-journal-grid-block-rail-lane block) org-journal-grid-all-day-max-lanes)
        (push (org-journal-grid--draw-all-day-block
               svg block left-offset column-width
               (+ rail-top
                  (* (org-journal-grid-block-rail-lane block)
                     lane-height))
               palette font-family)
              geometry)))
    (when-let* (((org-journal-grid--cursor-visible-p))
                (cursor (org-journal-grid--cursor))
                ((eq (org-journal-grid--cursor-state-surface cursor) 'rail))
                ((null (org-journal-grid--selected-id))))
      (let* ((cursor-height (* org-journal-grid-slot-minutes
                               (org-journal-grid--pixels-per-minute)))
             (x (+ left-offset (org-journal-grid--label-width)
                   (* (org-journal-grid--cursor-state-day cursor) column-width) 1))
             (y (+ rail-top
                   (* (org-journal-grid--cursor-state-lane cursor)
                      lane-height)
                   (/ (- lane-height cursor-height) 2.0))))
        (svg-rectangle svg x y (- column-width 2)
                       cursor-height
                       :fill (plist-get palette :cursor)
                       :fill-opacity org-journal-grid-cursor-opacity
                       :stroke (plist-get palette :cursor)
                       :stroke-width 1
                       :rx org-journal-grid-corner-radius)))
    (dotimes (day org-journal-grid-days)
      (let ((hidden
             (seq-count
              (lambda (block)
                (and (>= (org-journal-grid-block-rail-lane block)
                         org-journal-grid-all-day-max-lanes)
                     (< (* day 1440) (org-journal-grid-block-rail-end block))
                     (< (org-journal-grid-block-rail-start block) (* (1+ day) 1440))))
              all-day)))
        (when (> hidden 0)
          (svg-text svg (format "+%d more" hidden)
                    :x (+ left-offset (org-journal-grid--label-width)
                          (* day column-width) 7)
                    :y (+ rail-top (* event-rows lane-height)
                          (org-journal-grid--scale-pixels 15))
                    :font-size (org-journal-grid--font-size 9) :font-family font-family
                    :fill (plist-get palette :secondary-text)))))
    (svg-line svg 0 (1- height) width (1- height)
              :stroke (plist-get palette :grid) :stroke-width 1)
    (setq-local org-journal-grid--header-geometry geometry)
    (let ((map (org-journal-grid--header-image-map)))
      (propertize " "
                ;; Header lines still use text baseline metrics for images.
                ;; With zero ascent Emacs reserves a full text ascent above
                ;; the SVG, which appears as an unexplained blank strip.
                'display (svg-image svg :ascent 100 :scale 1
                                    :map map :original-map map)
                'keymap org-journal-grid--header-map
                'help-echo "Click to select an all-day cell or event"))))

(defun org-journal-grid--edge-height (geometry)
  "Return the pixel resize-zone height for GEOMETRY."
  (min org-journal-grid-edge-pixels
       (max 1 (/ (- (plist-get geometry :height) 2) 2.0))))

(defun org-journal-grid--edge-at (geometry y)
  "Return `top', `bottom', or nil for the resize zone at pixel Y in GEOMETRY.
A zone reaches `org-journal-grid--edge-height' pixels into the block and
`org-journal-grid-edge-slop' pixels outside it, which is exactly the
hotspot geometry built by `org-journal-grid--image-map'.  Slop must not
extend inwards: on a fifteen-minute block the two zones would meet and
leave no central move target."
  (or (plist-get geometry :boundary-edge)
      (let* ((edge-height (org-journal-grid--edge-height geometry))
             (slop org-journal-grid-edge-slop)
             (top (plist-get geometry :y))
             (bottom (+ top (plist-get geometry :height)))
             (in-top (and (>= y (- top slop)) (<= y (+ top edge-height))))
             (in-bottom (and (<= y (+ bottom slop))
                             (>= y (- bottom edge-height)))))
        (cond
         ((and (plist-get geometry :allow-top) in-top
               (or (not (plist-get geometry :allow-bottom))
                   (not in-bottom)
                   (<= (abs (- y top)) (abs (- y bottom)))))
          'top)
         ((and (plist-get geometry :allow-bottom) in-bottom)
          'bottom)))))

(defun org-journal-grid--geometry-help-echo (geometry)
  "Return the full heading title for GEOMETRY, or nil."
  (when-let* ((id (plist-get geometry :id))
              (block (org-journal-grid--block id))
              (title (org-journal-grid-block-title block)))
    (unless (string-empty-p title)
      title)))

(defun org-journal-grid--image-map ()
  "Return pixel hotspots for block movement and edge resizing."
  (let (edges bodies)
    (dolist (geometry org-journal-grid--geometry)
      (unless (plist-get geometry :preview)
        (let* ((x (round (plist-get geometry :x)))
               (y (round (plist-get geometry :y)))
               (right (round (+ (plist-get geometry :x)
                                (plist-get geometry :width))))
               (bottom (round (+ (plist-get geometry :y)
                                 (plist-get geometry :height))))
               (edge (max 1 (round
                             (org-journal-grid--edge-height
                              geometry))))
               (slop org-journal-grid-edge-slop)
               (boundary-edge (plist-get geometry :boundary-edge))
               (echo (org-journal-grid--geometry-help-echo geometry)))
          (cond
           (boundary-edge
            (push (list `(rect . ((,x . ,y) . (,right . ,bottom)))
                        'calendar-resize
                        `(pointer hand help-echo ,echo))
                  edges))
           (t
            (when (plist-get geometry :allow-top)
              (push (list `(rect . ((,x . ,(max 0 (- y slop)))
                                    . (,right . ,(+ y edge))))
                          'calendar-resize
                          `(pointer hand help-echo ,echo))
                    edges))
            (when (plist-get geometry :allow-bottom)
              (push (list `(rect . ((,x . ,(- bottom edge))
                                    . (,right . ,(+ bottom slop))))
                          'calendar-resize
                          `(pointer hand help-echo ,echo))
                    edges))
            (push (list `(rect . ((,x . ,y) . (,right . ,bottom)))
                        'calendar-block
                        `(pointer hand help-echo ,echo))
                  bodies))))))
    (append edges bodies)))

(defun org-journal-grid--refresh (&optional preserve-scroll)
  "Rebuild cached tiles.
When PRESERVE-SCROLL is non-nil, preserve the pixel scroll position."
  (org-journal-grid--ensure-state)
  (org-journal-grid--sync-fringe-background)
  (let* ((window (get-buffer-window (current-buffer) t))
         (vscroll (and preserve-scroll window
                       (let ((current (org-journal-grid--window-scroll-pixels
                                       window)))
                         (if (and (zerop current)
                                (> org-journal-grid--saved-vscroll 0))
                             org-journal-grid--saved-vscroll
                           current))))
         (previewp (org-journal-grid--calendar-state-preview org-journal-grid--state)))
    (when (overlayp org-journal-grid--pointer-overlay)
      (delete-overlay org-journal-grid--pointer-overlay)
      (setq-local org-journal-grid--pointer-overlay nil))
    (org-journal-grid--cache-static-tiles
     (and previewp (org-journal-grid--operation-replace-id previewp)))
    (org-journal-grid--insert-tiles)
    (org-journal-grid--render-dynamic)
    (unless (and previewp header-line-format)
      (setq-local header-line-format
                  (org-journal-grid--header)))
    (setq-local org-journal-grid--rendered-ui (org-journal-grid--ui-snapshot))
    (setq-local org-journal-grid--last-width
                (org-journal-grid--window-width))
    (set-buffer-modified-p nil)
    (when window
      ;; Restore the viewport before redisplay.  Painting after the tile
      ;; insertion but before this restoration flashes the top of the
      ;; calendar whenever an edit reloads its backend data.
      (if vscroll
          (org-journal-grid--set-vscroll window vscroll)
        (set-window-start window (point-min) t)
        (set-window-point window (point-min))
        (set-window-vscroll window 0 t))
      (redisplay t))))

(defun org-journal-grid--text-scale-changed ()
  "Rerender the calendar after a buffer-local text scaling change.
Keep the same calendar minute at the vertical center of the window."
  (when (derived-mode-p 'org-journal-grid-mode)
    (let* ((window (get-buffer-window (current-buffer) t))
           (old-factor org-journal-grid--last-zoom-factor)
           (new-factor (org-journal-grid--zoom-factor))
           (body (and window (window-body-height window t)))
           (scroll (and window (org-journal-grid--window-scroll-pixels window)))
           (old-inset (* org-journal-grid--grid-top-inset old-factor))
           (old-scale (* org-journal-grid-pixels-per-minute old-factor))
           (anchor-minute
            (and body scroll
                 (+ (* 60 org-journal-grid-start-hour)
                    (/ (- (+ scroll (/ body 2.0)) old-inset)
                       old-scale)))))
      (setq-local org-journal-grid--last-zoom-factor new-factor)
      (org-journal-grid--refresh)
      (when (and window anchor-minute)
        (let* ((new-center
                (+ (org-journal-grid--grid-top-inset)
                   (* (- anchor-minute (* 60 org-journal-grid-start-hour))
                      (org-journal-grid--pixels-per-minute))))
               (maximum (max 0 (- org-journal-grid--image-height body)))
               (target (max 0 (min maximum (- new-center (/ body 2.0))))))
          (org-journal-grid--set-vscroll window target)
          (redisplay t))))))

(defun org-journal-grid--install-text-scale-hook ()
  "Install SVG zoom support in the current calendar buffer."
  (setq-local org-journal-grid--last-zoom-factor (org-journal-grid--zoom-factor))
  (add-hook 'text-scale-mode-hook #'org-journal-grid--text-scale-changed nil t))

(defun org-journal-grid--window-scroll-pixels (window)
  "Return WINDOW's absolute pixel offset in the tiled calendar."
  (let* ((start (window-start window))
         (tile (or (get-text-property start 'org-journal-grid-tile) 0)))
    (+ (if org-journal-grid--tile-count
           (car (org-journal-grid--tile-bounds tile))
         0)
       (window-vscroll window t))))

(defun org-journal-grid--set-vscroll (window pixels)
  "Set WINDOW's pixel scroll to PIXELS and remember it."
  (let* ((pixels (max 0 (round pixels)))
         (pixels (if (numberp org-journal-grid--image-height)
                     (min pixels (1- (ceiling org-journal-grid--image-height)))
                   pixels))
         (tile (or (org-journal-grid--tile-at-pixel pixels) 0))
         (top (if org-journal-grid--tile-count
                  (car (org-journal-grid--tile-bounds tile))
                0))
         (within (- pixels top)))
    (when (and (vectorp org-journal-grid--tile-markers)
               (< tile (length org-journal-grid--tile-markers)))
      (let ((marker (aref org-journal-grid--tile-markers tile)))
        (set-window-start window marker t)
        (set-window-point
         window
         (aref org-journal-grid--tile-markers
               (min (1- (length org-journal-grid--tile-markers))
                    (1+ tile))))))
    (set-window-vscroll window within t))
  (setq-local org-journal-grid--saved-vscroll pixels))

(defun org-journal-grid--restore-scroll (window)
  "Restore this calendar's saved pixel position in WINDOW."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (set-window-point window (point-min))
    (org-journal-grid--set-vscroll window org-journal-grid--saved-vscroll)))

(defun org-journal-grid--schedule-scroll-restore (window)
  "Restore WINDOW after the buffer switch has finished redisplaying."
  (when (timerp org-journal-grid--scroll-restore-timer)
    (cancel-timer org-journal-grid--scroll-restore-timer))
  (let ((buffer (current-buffer)))
    (setq-local
     org-journal-grid--scroll-restore-timer
     (run-at-time
      0.01 nil
      (lambda ()
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local org-journal-grid--scroll-restore-timer nil)
            (org-journal-grid--restore-scroll window))))))))

(defun org-journal-grid--restore-frame-calendars (frame)
  "Restore calendars newly displayed in a window on FRAME."
  (dolist (window (window-list frame 'no-minibuffer))
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'org-journal-grid-mode)
                 (> org-journal-grid--saved-vscroll 0))
        (org-journal-grid--schedule-scroll-restore window)))))

(defun org-journal-grid--theme-changed (&rest _ignored)
  "Redraw live SVG calendars after an Emacs theme face change."
  (when (timerp org-journal-grid--theme-timer)
    (cancel-timer org-journal-grid--theme-timer))
  (setq org-journal-grid--theme-timer
        (run-at-time
         0 nil
         (lambda ()
           (setq org-journal-grid--theme-timer nil)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when (derived-mode-p 'org-journal-grid-mode)
                 (if (/= (org-journal-grid--zoom-factor)
                         org-journal-grid--last-zoom-factor)
                     (org-journal-grid--text-scale-changed)
                   (org-journal-grid--refresh t)))))))))

(defun org-journal-grid--window-resized (window)
  "Schedule a redraw when WINDOW has a new pixel width or default font size."
  (let ((buffer (window-buffer window))
        (width (window-body-width window t))
        (factor (org-journal-grid--zoom-factor window)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'org-journal-grid-mode)
                   (or org-journal-grid--stale
                       (/= width (or org-journal-grid--last-width -1))
                       (/= factor org-journal-grid--last-zoom-factor)))
          (setq-local org-journal-grid--stale nil)
          (when (timerp org-journal-grid--resize-timer)
            (cancel-timer org-journal-grid--resize-timer))
          (setq-local
           org-journal-grid--resize-timer
           (run-at-time
            0 nil
            (lambda ()
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (setq-local org-journal-grid--resize-timer nil)
                  (if (/= (org-journal-grid--zoom-factor window)
                          org-journal-grid--last-zoom-factor)
                      (org-journal-grid--text-scale-changed)
                    (org-journal-grid--refresh t))))))))))))

(defun org-journal-grid--cancel-timers ()
  "Cancel timers owned by the SVG prototype buffer."
  (dolist (timer (list org-journal-grid--resize-timer
                       org-journal-grid--clock-timer
                       org-journal-grid--data-timer
                       org-journal-grid--keyboard-edit-timer
                       org-journal-grid--scroll-restore-timer))
    (when (timerp timer) (cancel-timer timer))))

(defun org-journal-grid--clock-tick (buffer)
  "Redraw the current time indicator in visible calendar BUFFER."
  (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'org-journal-grid-mode)
                 (null (org-journal-grid--calendar-state-preview org-journal-grid--state)))
        (let ((old-tiles org-journal-grid--clock-tiles))
          (org-journal-grid--update-clock-fragment)
          (dolist (tile (delete-dups
                         (append old-tiles org-journal-grid--clock-tiles)))
            (aset org-journal-grid--static-images tile
                  (org-journal-grid--make-tile-image tile))
            (unless (memq tile org-journal-grid--dynamic-tiles)
              (org-journal-grid--set-tile-image
               tile (aref org-journal-grid--static-images tile))))
          (when org-journal-grid--dynamic-tiles
            (org-journal-grid--render-dynamic))
          (redisplay t))))))

(defun org-journal-grid--data-tick (buffer)
  "Reload visible calendar BUFFER, or mark it stale while hidden."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'org-journal-grid-mode)
        (if (get-buffer-window buffer t)
            (org-journal-grid--refresh-data)
          (setq-local org-journal-grid--stale t))))))

(defun org-journal-grid--position-image-xy (position)
  "Return stable full-calendar pixel coordinates for mouse POSITION.
Use window-relative X so crossing image map hotspots and day columns cannot
reset the horizontal origin.  Add the tile offset to glyph-relative Y."
  (let* ((window-xy (posn-x-y position))
         (object-xy (posn-object-x-y position))
         (point (posn-point position))
         (tile (and (integer-or-marker-p point)
                    (or (get-text-property point 'org-journal-grid-tile)
                        (and (> point (point-min))
                             (get-text-property (1- point)
                                                'org-journal-grid-tile))))))
    (when (or window-xy object-xy)
      (cons (or (car-safe window-xy) (car-safe object-xy))
            (+ (if (and tile org-journal-grid--tile-count)
                   (car (org-journal-grid--tile-bounds tile))
                 0)
               (or (cdr-safe object-xy) (cdr-safe window-xy)))))))

(defun org-journal-grid--target (position)
  "Return calendar metadata at mouse POSITION in the SVG image."
  (let* ((xy (org-journal-grid--position-image-xy position))
         (x (and xy (car xy)))
         (y (and xy (cdr xy)))
         (width (org-journal-grid--window-width))
         (column-width (/ (- width (org-journal-grid--label-width))
                          (float org-journal-grid-days)))
         (start-minute (* 60 org-journal-grid-start-hour)))
    (when (and (numberp x) (numberp y)
               (>= x (org-journal-grid--label-width))
               (< x width) (>= y 0)
               (< y org-journal-grid--image-height))
      (let* ((day (min (org-journal-grid--last-day-index)
                       (floor (/ (- x (org-journal-grid--label-width))
                                    column-width))))
             (minute (+ start-minute
                        (* org-journal-grid-slot-minutes
                           (floor (/ (max 0 (- y (org-journal-grid--grid-top-inset)))
                                     (* (org-journal-grid--pixels-per-minute)
                                        org-journal-grid-slot-minutes))))))
             (geometry
              (cl-find-if (lambda (item)
                            (let ((slop
                                   org-journal-grid-edge-slop))
                              (and (not (plist-get item :preview))
                                   (<= (plist-get item :x) x
                                       (+ (plist-get item :x)
                                          (plist-get item :width)))
                                   (<= (- (plist-get item :y) slop) y
                                       (+ (plist-get item :y)
                                          (plist-get item :height) slop)))))
                          org-journal-grid--geometry))
             (edge (and geometry
                        (org-journal-grid--edge-at geometry y))))
        (list :surface 'grid :block-id (plist-get geometry :id)
              :day day :minute minute :edge edge)))))

(defun org-journal-grid--set-preview (proposal)
  "Display PROPOSAL without committing it."
  (let* ((old (org-journal-grid--calendar-state-preview org-journal-grid--state))
         (preview (and proposal (not (org-journal-grid--operation-error proposal)) proposal))
         (old-all-day (and old (org-journal-grid-block-all-day-p
                                (org-journal-grid--operation-block old))))
         (new-all-day (and preview (org-journal-grid-block-all-day-p
                                    (org-journal-grid--operation-block preview)))))
    (unless (equal preview (org-journal-grid--calendar-state-preview org-journal-grid--state))
      (setf (org-journal-grid--calendar-state-preview org-journal-grid--state) preview)
      (if (eq old-all-day new-all-day)
          (if new-all-day
              (org-journal-grid--render-header-dynamic)
            (org-journal-grid--render-dynamic t))
        ;; A cross-surface preview removes the source from one canvas and
        ;; paints it on the other, so both dynamic layers must be refreshed.
        (org-journal-grid--render-header-dynamic)
        (org-journal-grid--render-dynamic t)))))

(defun org-journal-grid--clear-preview ()
  "Remove a pending SVG preview."
  (when (org-journal-grid--calendar-state-preview org-journal-grid--state)
    (setf (org-journal-grid--calendar-state-preview org-journal-grid--state) nil)
    (org-journal-grid--refresh t)))

(defun org-journal-grid--backend-undo (redo)
  "Ask the backend to undo, or REDO, its most recent edit.
The calendar cannot undo for itself: the change lives in a source buffer
it does not own.  Repeating the key continues one run rather than
undoing the undo, which is what `undo' does at a keyboard."
  (let ((undoer (org-journal-grid-backend-undo-function org-journal-grid--backend)))
    (unless (functionp undoer)
      (user-error "This backend cannot undo; undo in the source buffer"))
    (funcall undoer
             (and (memq last-command '(org-journal-grid-undo org-journal-grid-redo)) t)
             redo)
    (org-journal-grid--refresh-data)))

(defun org-journal-grid-undo ()
  "Undo the calendar's most recent edit, in the file it touched."
  (interactive)
  (org-journal-grid--backend-undo nil))

(defun org-journal-grid-redo ()
  "Redo the calendar's most recently undone edit."
  (interactive)
  (org-journal-grid--backend-undo t))

(defun org-journal-grid--offer-entry-deletion (event)
  "Offer to delete EVENT's whole entry now that its time is gone.
Removing a block usually means the plan is over, not that the entry
should linger untimed, so the question is asked once here rather than
requiring a trip to the file.  Declining keeps the timestamp removed."
  (when-let* ((deleter (org-journal-grid-backend-delete-entry-function
                        org-journal-grid--backend))
              ((functionp deleter))
              ((y-or-n-p (format "Delete entry \"%s\" too? "
                                 (org-journal-grid-event-title event)))))
    (funcall deleter event)
    (org-journal-grid--refresh-data)))

(defun org-journal-grid-remove-selected (&optional keep-entry)
  "Remove the selected block from its calendar source.
KEEP-ENTRY skips the offer to delete the entry itself, which is what a
cut wants: the entry has to survive for the yank to copy it."
  (interactive)
  (let ((id (org-journal-grid--selected-id)))
    (if (null id)
        (message "No block selected")
      (let* ((block (org-journal-grid--block id))
             (event (and block (org-journal-grid-block-event block)))
             (deleter (org-journal-grid-backend-delete-function
                       org-journal-grid--backend)))
        (unless (and event (functionp deleter))
          (user-error "This backend cannot remove calendar entries"))
        (funcall deleter event)
        (org-journal-grid--refresh-data)
        (unless keep-entry
          (org-journal-grid--offer-entry-deletion event))))))

(defun org-journal-grid-edit-selected-title ()
  "Edit the selected calendar block's source heading title."
  (interactive)
  (let* ((id (org-journal-grid--selected-id))
         (source (and id (org-journal-grid--block id))))
    (if (null source)
        (message "No block selected")
      (let ((title (org-journal-grid--read-title
                    (org-journal-grid-block-title source))))
        (unless (string-empty-p title)
          (let ((event (org-journal-grid-block-event source))
                (updater (org-journal-grid-backend-update-function
                          org-journal-grid--backend)))
            (unless (and event (functionp updater))
              (user-error "This backend cannot rename calendar entries"))
            (org-journal-grid--call-update
             updater event
             (org-journal-grid-event-start event)
             (org-journal-grid-event-end event) title
             (org-journal-grid-event-time-kind event))
            (org-journal-grid--refresh-data)))))))

(defun org-journal-grid--read-title (&optional initial)
  "Read a block title in the echo area, prefilled with INITIAL."
  (read-string "Title: " initial))

(defun org-journal-grid--block-absolute-range (block)
  "Return BLOCK's absolute (START . END) minute range."
  (let ((day-minute
         (* (+ (org-journal-grid--calendar-state-week-start org-journal-grid--state)
               (org-journal-grid-block-day block))
            1440)))
    (cons (+ day-minute (org-journal-grid-block-start block))
          (+ day-minute (org-journal-grid-block-end block)))))

(defun org-journal-grid--accepts-arguments-p (function count)
  "Return non-nil when FUNCTION accepts COUNT arguments."
  (pcase-let ((`(,minimum . ,maximum) (func-arity function)))
    (and (<= minimum count)
         (or (eq maximum 'many) (>= maximum count)))))

(defun org-journal-grid--call-update (updater event start end title time-kind)
  "Call UPDATER for EVENT, START, END and TITLE.
Pass an explicit TIME-KIND when UPDATER's contract supports it."
  (cond
   ((org-journal-grid--accepts-arguments-p updater 5)
    (funcall updater event start end title time-kind))
   ((or title (org-journal-grid--accepts-arguments-p updater 4))
    (funcall updater event start end title))
   (t (funcall updater event start end))))

(defun org-journal-grid--backend-create (title block &optional source-event target)
  "Ask the active backend to create TITLE using BLOCK's range.
SOURCE-EVENT identifies an entry to reuse, and TARGET selects its destination."
  (let ((creator (org-journal-grid-backend-create-function
                  org-journal-grid--backend))
        (range (org-journal-grid--block-absolute-range block)))
    (unless (functionp creator)
      (user-error "This backend cannot create calendar entries"))
    (if (org-journal-grid--accepts-arguments-p creator 6)
        (funcall creator title (car range) (cdr range) source-event target
                 (org-journal-grid-block-time-kind block))
      (if target
          (funcall creator title (car range) (cdr range) source-event target)
        (funcall creator title (car range) (cdr range) source-event)))
    (org-journal-grid--refresh-data)))

(defun org-journal-grid--read-entry ()
  "Read a title and optional existing record for a new block."
  (let ((reader (and (fboundp 'org-journal-grid-backend-read-entry-function)
                     org-journal-grid--backend
                     (org-journal-grid-backend-read-entry-function
                      org-journal-grid--backend))))
    (if (functionp reader)
        (funcall reader)
      (cons (org-journal-grid--read-title) nil))))

(defun org-journal-grid--backend-update (block)
  "Ask the active backend to apply BLOCK's new range."
  (let* ((updater (org-journal-grid-backend-update-function
                   org-journal-grid--backend))
         (event (org-journal-grid-block-event block))
         (range (org-journal-grid--block-absolute-range block)))
    (unless (and event (functionp updater))
      (user-error "This backend cannot move or resize calendar entries"))
    (condition-case error-data
        (progn
          (org-journal-grid--call-update
           updater event (car range) (cdr range) nil
           (org-journal-grid-block-time-kind block))
          (org-journal-grid--refresh-data))
      (error
       (org-journal-grid--refresh-data)
       (signal (car error-data) (cdr error-data))))))

(defun org-journal-grid--apply (proposal)
  "Commit SVG drag PROPOSAL."
  (let ((error-message (org-journal-grid--operation-error proposal))
        (kind (org-journal-grid--operation-kind proposal))
        (block (and (org-journal-grid--operation-block proposal)
                    (copy-org-journal-grid-block
                     (org-journal-grid--operation-block proposal)))))
    (cond
     (error-message
      (org-journal-grid--clear-preview)
      (message "%s" error-message))
     ((eq kind 'create)
      (let (entry)
        (unwind-protect
            (progn
              (org-journal-grid--set-preview proposal)
              (setq entry (org-journal-grid--read-entry)))
          (setf (org-journal-grid--calendar-state-preview org-journal-grid--state) nil)
          (org-journal-grid--refresh t))
        (unless (string-empty-p (car entry))
          (org-journal-grid--backend-create
           (car entry) block nil (cdr entry)))))
     ((memq kind '(duplicate-entry add-occurrence))
      (setf (org-journal-grid--calendar-state-preview org-journal-grid--state) nil)
      (let ((source (org-journal-grid--block (org-journal-grid-block-id block))))
        (org-journal-grid--backend-create
         (org-journal-grid-block-title source) block
         (org-journal-grid-block-event source)
         (and (eq kind 'add-occurrence)
              (org-journal-grid-event-source
               (org-journal-grid-block-event source))))))
     ((memq kind '(move resize))
      (setf (org-journal-grid--calendar-state-preview org-journal-grid--state) nil
            (org-journal-grid-block-preview block) nil)
      (org-journal-grid--backend-update block))
     (t (org-journal-grid--clear-preview)))))

(defun org-journal-grid-click (event)
  "Put the cursor where SVG mouse EVENT landed.
Clicking a block moves the cursor to that block's own first slot, which is
what selects it; clicking empty space moves the cursor to that slot and so
selects nothing.  The mouse and the keyboard drive one shared cursor."
  (interactive "@e")
  (let* ((target (org-journal-grid--target (event-start event)))
         (block (org-journal-grid--block (plist-get target :block-id))))
    (setf (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state) t)
    (if block
        (org-journal-grid--goto-block block)
      (when (and (plist-get target :day) (plist-get target :minute))
        (org-journal-grid--set-cursor (plist-get target :day)
                                  (plist-get target :minute))
        (org-journal-grid--cursor-moved)))))

(defun org-journal-grid--header-target (position)
  "Return all-day rail metadata at header-line mouse POSITION."
  (let* ((xy (or (posn-object-x-y position) (posn-x-y position)))
         (x (car-safe xy))
         (y (cdr-safe xy))
         (window (posn-window position))
         (left-offset (if (window-live-p window)
                          (or (car (window-fringes window)) 0)
                        0))
         (canvas-width (org-journal-grid--window-width))
         (column-width (/ (- canvas-width (org-journal-grid--label-width))
                          (float org-journal-grid-days))))
    (when (and (numberp x) (numberp y)
               (>= y (org-journal-grid--rail-top))
               (>= x (+ left-offset (org-journal-grid--label-width)))
               (< x (+ left-offset canvas-width)))
      (let ((geometry
             (cl-find-if
              (lambda (item)
                (and (<= (plist-get item :x) x
                         (+ (plist-get item :x) (plist-get item :width)))
                     (<= (plist-get item :y) y
                         (+ (plist-get item :y) (plist-get item :height)))))
              org-journal-grid--header-geometry)))
        (list :surface 'rail
              :id (plist-get geometry :id)
              :block-id (plist-get geometry :id)
              :edge (and geometry (org-journal-grid--rail-edge-at geometry x))
              :day (min (org-journal-grid--last-day-index) (max 0 (floor
                                  (/ (- x left-offset
                                        (org-journal-grid--label-width))
                                     column-width))))
              :minute 0
              :lane (max 0 (floor (/ (- y (org-journal-grid--rail-top))
                                     (org-journal-grid--all-day-lane-height)))))))))

(defun org-journal-grid--mouse-position-xy (position)
  "Return comparable POSITION coordinates across the rail and time grid."
  (let ((xy (or (posn-x-y position) (posn-object-x-y position))))
    (and xy (list (org-journal-grid--mouse-surface position)
                  (car xy) (cdr xy)))))

(defun org-journal-grid--rail-area-p (area)
  "Return non-nil when mouse AREA belongs to the all-day rail.
Image-map hotspots replace `header-line' in `posn-area', so all of the
rail's area names must be treated as one stable surface during a drag."
  (memq area '(header-line calendar-rail-block calendar-rail-resize)))

(defun org-journal-grid--mouse-surface (position)
  "Return the stable calendar surface containing mouse POSITION."
  (if (org-journal-grid--rail-area-p (posn-area position)) 'rail 'grid))

(defun org-journal-grid--mouse-target (position)
  "Return calendar metadata for POSITION on either mouse surface."
  (if (eq (org-journal-grid--mouse-surface position) 'rail)
      (org-journal-grid--header-target position)
    (org-journal-grid--target position)))

(defun org-journal-grid-header-click (event)
  "Move the shared calendar cursor to all-day rail mouse EVENT."
  (interactive "@e")
  (let* ((position (event-start event))
         (target (org-journal-grid--header-target position)))
    (if (null target)
        (message "Click inside an all-day cell")
      (let ((day (plist-get target :day))
            (lane (plist-get target :lane))
            (id (plist-get target :id)))
        (org-journal-grid--set-all-day-cursor day lane)
        (when id
          (setf (org-journal-grid--calendar-state-selected-id org-journal-grid--state)
                id))
        (setf (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state)
              t)
        (org-journal-grid--cursor-moved)))))

(defun org-journal-grid--all-day-create-proposal (origin target)
  "Return a date-only creation proposal spanning ORIGIN through TARGET.
Both endpoints are rail targets.  The mouse-selected final day is inclusive;
the block and backend range use an exclusive midnight endpoint."
  (let ((origin-day (plist-get origin :day))
        (target-day (plist-get target :day)))
    (if (or (null origin-day) (null target-day))
        (org-journal-grid--operation-create
         :error "Release inside an all-day cell")
      (let* ((first (min origin-day target-day))
             (last (max origin-day target-day))
             (block (org-journal-grid--make-block
                     'preview first 0 (* (1+ (- last first)) 1440)
                     "New all-day block" 'blue nil 'all-day)))
        (setf (org-journal-grid-block-preview block) t)
        (org-journal-grid--operation-create :kind 'create :block block)))))

(defun org-journal-grid--header-position-xy (position)
  "Return the SVG-local coordinates of header-line POSITION."
  (or (posn-object-x-y position) (posn-x-y position)))

(defun org-journal-grid--track-drag-gesture
    (event position-function target-function proposal-function click-function)
  "Track one mouse EVENT independently of its calendar surface.
POSITION-FUNCTION returns comparable pixel coordinates, TARGET-FUNCTION
returns model coordinates, PROPOSAL-FUNCTION builds a preview, and
CLICK-FUNCTION handles a press that never becomes a drag."
  (let* ((origin-position (event-start event))
         (origin (funcall target-function origin-position))
         (origin-xy (funcall position-function origin-position))
         ;; Cross-surface previews add to or remove from the all-day layout.
         ;; Letting that resize the sticky header moves the grid/rail boundary
         ;; beneath the held pointer and makes hit-testing oscillate.  The
         ;; rail's intentional empty row is enough preview space, so preserve
         ;; the initial coordinate system until release.
         (org-journal-grid--drag-rail-rows
          (org-journal-grid--rail-row-count (org-journal-grid--all-day-layout)))
         (end-target origin)
         dragged finished next basic)
    (track-mouse
      (while (not finished)
        (setq next (read-event)
              basic (event-basic-type next))
        (cond
         ((mouse-movement-p next)
          (setq next (org-journal-grid--latest-motion-event next))
          (let* ((position (event-end next))
                 (xy (funcall position-function position))
                 (target (funcall target-function position)))
            (when (and origin-xy xy (not (equal origin-xy xy)))
              (setq dragged t
                    end-target target)
              (if target
                  (org-journal-grid--set-preview
                   (funcall proposal-function origin end-target))
                (org-journal-grid--clear-preview)))))
         ((memq basic '(mouse-1 drag-mouse-1))
          ;; Commit only the cell under the actual release.  In particular,
          ;; do not retain a destructive cross-surface hover after the mouse
          ;; has left the calendar.
          (setq end-target (funcall target-function (event-end next))
                finished t))
         ((eq basic 'switch-frame))
         (t
          (push next unread-command-events)
          (setq finished 'cancelled)))))
    (cond
     ((eq finished 'cancelled)
      (org-journal-grid--clear-preview))
     (dragged
      (org-journal-grid--apply (funcall proposal-function origin end-target)))
     (t
      (funcall click-function origin-position)))))

(defun org-journal-grid-header-press (event)
  "Track an all-day range creation gesture beginning with mouse EVENT.
Dragging an empty rail cell across date columns creates an inclusive range.
Pressing an existing block retains ordinary click-to-select behavior."
  (interactive "@e")
  (when mark-active (deactivate-mark))
  (let* ((origin-position (event-start event))
         (origin (org-journal-grid--header-target origin-position))
         (modifiers (event-modifiers event))
         (copy-kind (cond ((memq 'super modifiers) 'duplicate-entry)
                          ((memq 'shift modifiers) 'add-occurrence)))
         (creator (and org-journal-grid--backend
                       (org-journal-grid-backend-create-function
                        org-journal-grid--backend))))
    (cond
     ((null origin)
      (message "Press inside an all-day cell"))
     ((plist-get origin :id)
      (let ((callback
             (and org-journal-grid--backend
                  (if copy-kind
                      (org-journal-grid-backend-create-function
                       org-journal-grid--backend)
                    (org-journal-grid-backend-update-function
                     org-journal-grid--backend)))))
        (if (not (functionp callback))
            (progn
              (org-journal-grid-header-click (list 'mouse-1 origin-position))
              (message "Unsupported drag"))
          (org-journal-grid--track-drag-gesture
           event
           #'org-journal-grid--mouse-position-xy
           #'org-journal-grid--mouse-target
           (lambda (from to) (org-journal-grid--proposal from to copy-kind))
           (lambda (position)
             (org-journal-grid-header-click (list 'mouse-1 position)))))))
     ((not (functionp creator))
      (org-journal-grid-header-click (list 'mouse-1 origin-position))
      (message "This backend cannot create calendar entries"))
     (t
      (org-journal-grid--track-drag-gesture
       event
       #'org-journal-grid--header-position-xy
       #'org-journal-grid--header-target
       #'org-journal-grid--all-day-create-proposal
       (lambda (position)
         (org-journal-grid-header-click (list 'mouse-1 position))))))))

(defun org-journal-grid-header-visit (event)
  "Visit the all-day event under header-line mouse EVENT."
  (interactive "@e")
  (when-let* ((target (org-journal-grid--header-target (event-start event)))
              (block (org-journal-grid--block (plist-get target :id)))
              (calendar-event (org-journal-grid-block-event block))
              (visitor (org-journal-grid-backend-visit-function
                        org-journal-grid--backend)))
    (funcall visitor calendar-event)))

(defun org-journal-grid-ignore-double-press (_event)
  "Ignore the second button press so its double-click release can visit.
The first press still follows the ordinary click-or-drag path."
  (interactive "e"))

(defun org-journal-grid-visit (event)
  "Visit the source event under double-click mouse EVENT."
  (interactive "@e")
  (let* ((target (org-journal-grid--target (event-start event)))
         (block (org-journal-grid--block
                 (plist-get target :block-id)))
         (calendar-event (org-journal-grid-block-event block))
         (visitor (and org-journal-grid--backend
                       (org-journal-grid-backend-visit-function
                        org-journal-grid--backend))))
    (if (and calendar-event (functionp visitor))
        (funcall visitor calendar-event)
      (message "No source to visit"))))

(defun org-journal-grid--latest-motion-event (event)
  "Return the newest consecutive mouse-motion EVENT waiting in the queue.
Leave the first non-motion event for the gesture loop to process."
  (let ((latest event))
    (catch 'done
      (while (input-pending-p)
        (let ((pending (read-event nil nil 0)))
          (cond
           ((null pending) (throw 'done nil))
           ((mouse-movement-p pending) (setq latest pending))
           (t
            (push pending unread-command-events)
            (throw 'done nil))))))
    latest))

(defun org-journal-grid-press (event)
  "Track a complete create, move, duplicate, occurrence, or resize EVENT."
  (interactive "@e")
  (when mark-active (deactivate-mark))
  (let* ((origin-position (event-start event))
         (origin (org-journal-grid--target origin-position))
         (modifiers (event-modifiers event))
         ;; Super, which is the Option key under the usual macOS mapping,
         ;; makes an independent entry.  Shift retains the source entry.
         (copy-kind (cond ((memq 'super modifiers) 'duplicate-entry)
                          ((memq 'shift modifiers) 'add-occurrence)))
         (required-callback
          (and org-journal-grid--backend
               (if (or copy-kind (null (plist-get origin :block-id)))
                   (org-journal-grid-backend-create-function
                    org-journal-grid--backend)
                 (org-journal-grid-backend-update-function
                  org-journal-grid--backend)))))
    (if (and org-journal-grid--backend
             (not (functionp required-callback)))
        (org-journal-grid-click (list 'mouse-1 origin-position))
      (org-journal-grid--track-drag-gesture
       event
       #'org-journal-grid--mouse-position-xy
       #'org-journal-grid--mouse-target
       (lambda (from to)
         (org-journal-grid--proposal from to copy-kind))
       (lambda (position)
         (org-journal-grid-click (list 'mouse-1 position)))))))

(defun org-journal-grid-pointer-feedback (event)
  "Set a resize or move pointer under SVG mouse EVENT."
  (interactive "e")
  (let* ((position (event-end event))
         (window (posn-window position)))
    (when (windowp window)
      (with-current-buffer (window-buffer window)
        (let* ((target (org-journal-grid--target position))
               (shape (cond ((plist-get target :edge) 'nhdrag)
                            ((plist-get target :block-id) 'hand)))
               (point (posn-point position)))
          (when (integer-or-marker-p point)
            (unless (overlayp org-journal-grid--pointer-overlay)
              (setq-local org-journal-grid--pointer-overlay
                          (make-overlay point (1+ point))))
            ;; Each hour tile is a separate display glyph.  Keeping this
            ;; overlay on the first tile visited makes its pointer property
            ;; appear to work only after unrelated selection redraws.
            (move-overlay org-journal-grid--pointer-overlay point (1+ point))
            (overlay-put org-journal-grid--pointer-overlay
                         'pointer shape)
            (force-window-update window)))))))

(defun org-journal-grid-scroll (pixels &optional window)
  "Scroll the tall SVG image by PIXELS in calendar WINDOW."
  (interactive "p")
  (let ((window (or window (get-buffer-window (current-buffer) t))))
    (when (window-live-p window)
      (with-current-buffer (window-buffer window)
        (when (and (derived-mode-p 'org-journal-grid-mode)
                   (numberp org-journal-grid--image-height))
          (let* ((now (float-time))
                 (current (org-journal-grid--window-scroll-pixels window))
                 (maximum
                  (max 0 (- org-journal-grid--image-height
                            (window-body-height window t))))
                 (rebound
                  (and org-journal-grid--scroll-boundary
                       (< (- now (cdr org-journal-grid--scroll-boundary))
                          org-journal-grid--scroll-rebound-seconds)
                       (or (and (eq (car org-journal-grid--scroll-boundary) 'top)
                                (> pixels 0))
                           (and (eq (car org-journal-grid--scroll-boundary) 'bottom)
                                (< pixels 0)))))
                 (next
                  (max 0 (min maximum
                              (+ current pixels)))))
            (cond
             ((and (zerop current) (< pixels 0))
              (setq-local org-journal-grid--scroll-boundary (cons 'top now)))
             ((and (= current maximum) (> pixels 0))
              (setq-local org-journal-grid--scroll-boundary (cons 'bottom now)))
             ((not rebound)
              (setq-local org-journal-grid--scroll-boundary nil)
              (org-journal-grid--set-vscroll window next)))))))))

(defun org-journal-grid--event-window (event)
  "Return the live calendar window associated with mouse EVENT."
  (let ((window (posn-window (event-start event))))
    (and (window-live-p window)
         (with-current-buffer (window-buffer window)
           (derived-mode-p 'org-journal-grid-mode))
         window)))

;;; Keyboard cursor

(defun org-journal-grid--ui-snapshot ()
  "Return the small model fragment that controls dynamic painting."
  (list :surface (and (org-journal-grid--cursor)
                      (org-journal-grid--cursor-state-surface
                       (org-journal-grid--cursor)))
        :cursor (and (org-journal-grid--cursor)
                     (copy-org-journal-grid--cursor-state
                      (org-journal-grid--cursor)))
        :selected-id (org-journal-grid--selected-id)
        :visible (and (org-journal-grid--cursor-visible-p) t)))

(defun org-journal-grid--render-header-dynamic ()
  "Repaint the small sticky surface without touching time-grid tiles."
  (setq-local header-line-format (org-journal-grid--header))
  (when (get-buffer-window (current-buffer) t)
    (force-mode-line-update t)
    (redisplay t)))

(defun org-journal-grid--render-ui-change ()
  "Paint the minimal damage between the rendered and current UI state."
  (let* ((old org-journal-grid--rendered-ui)
         (new (org-journal-grid--ui-snapshot))
         (old-surface (plist-get old :surface))
         (new-surface (plist-get new :surface)))
    (cond
     ((not org-journal-grid--static-inner)
      (org-journal-grid--refresh t))
     ((eq new-surface 'rail)
      ;; Same-surface rail motion changes only the header.  Crossing from the
      ;; grid must additionally restore any body tiles that held its cursor.
      (unless (eq old-surface 'rail)
        (org-journal-grid--render-dynamic))
      (org-journal-grid--render-header-dynamic))
     ((eq old-surface 'rail)
      ;; Clear the old rail cursor, then draw the new grid dynamic layer.
      (org-journal-grid--render-header-dynamic)
      (org-journal-grid--render-dynamic t))
     (t
      (org-journal-grid--render-dynamic t)))
    (setq-local org-journal-grid--rendered-ui new)))

(defun org-journal-grid--cursor-moved ()
  "Render a cursor/selection model change and keep it visible."
  (org-journal-grid--render-ui-change)
  (org-journal-grid--scroll-cursor-into-view))

(defun org-journal-grid--scroll-cursor-into-view ()
  "Scroll the minimum amount needed to make the whole cursor slot visible."
  (when-let* ((cursor (org-journal-grid--cursor))
              (window (get-buffer-window (current-buffer) t)))
    (let* ((scale (org-journal-grid--pixels-per-minute))
           (start-minute (* 60 org-journal-grid-start-hour))
           (top (+ (org-journal-grid--grid-top-inset)
                   (* (- (org-journal-grid--cursor-state-minute cursor) start-minute) scale)))
           (bottom (+ top (* org-journal-grid-slot-minutes scale)))
           (body (window-body-height window t))
           (vscroll (org-journal-grid--window-scroll-pixels window))
           (maximum (max 0 (- (or org-journal-grid--image-height 0) body))))
      (cond
       ((< top vscroll)
        (org-journal-grid--set-vscroll window (max 0 (min maximum top))))
       ((> bottom (+ vscroll body))
        (org-journal-grid--set-vscroll
         window (max 0 (min maximum (- bottom body)))))))))

(defun org-journal-grid-recenter ()
  "Scroll the cursor's slot to the middle of the window, leaving it put.
Ordinary motion scrolls only as far as it must, which keeps the cursor at
an edge after a long run; this is the view half of that on its own, so
the cursor never moves to satisfy the scroll."
  (interactive)
  (let ((cursor (org-journal-grid--ensure-cursor))
        (window (get-buffer-window (current-buffer) t)))
    (when (window-live-p window)
      (let* ((scale (org-journal-grid--pixels-per-minute))
             (top (+ (org-journal-grid--grid-top-inset)
                     (* (- (org-journal-grid--cursor-state-minute cursor)
                           (* 60 org-journal-grid-start-hour))
                        scale)))
             (body (window-body-height window t))
             (maximum (max 0 (- (or org-journal-grid--image-height 0) body))))
        (org-journal-grid--set-vscroll
         window
         (max 0 (min maximum
                     (round (- top (/ (- body (* org-journal-grid-slot-minutes
                                                scale))
                                      2))))))))))

(defun org-journal-grid--move-cursor (minutes days)
  "Move the cursor by MINUTES and DAYS, revealing it first when hidden."
  (if (org-journal-grid--reveal-cursor)
      (org-journal-grid--cursor-moved)
    (let ((cursor (org-journal-grid--cursor)))
      (org-journal-grid--set-cursor (+ (org-journal-grid--cursor-state-day cursor) days)
                                (+ (org-journal-grid--cursor-state-minute cursor) minutes)))
    (org-journal-grid--cursor-moved)))

(defun org-journal-grid--lane-count (day minute)
  "Return how many blocks start in the slot at DAY and MINUTE."
  (length (org-journal-grid--blocks-starting-at day minute)))

(defun org-journal-grid--all-day-visible-rows ()
  "Return the number of occupied event rows currently shown in the rail."
  (let ((blocks (org-journal-grid--all-day-layout)))
    (if blocks
        (min org-journal-grid-all-day-max-lanes
             (1+ (apply #'max (mapcar (lambda (block)
                                        (org-journal-grid-block-rail-lane block))
                                      blocks))))
      0)))

(defun org-journal-grid--all-day-at (day lane)
  "Return the all-day block occupying DAY and LANE, if any."
  (cl-find-if
   (lambda (block)
     (and (= lane (org-journal-grid-block-rail-lane block))
          (< (* day 1440) (org-journal-grid-block-rail-end block))
          (< (org-journal-grid-block-rail-start block) (* (1+ day) 1440))))
   (org-journal-grid--all-day-layout)))

(defun org-journal-grid--set-all-day-cursor (day lane)
  "Place the cursor in all-day rail cell DAY, LANE."
  (let ((block (org-journal-grid--all-day-at day lane)))
    (setf (org-journal-grid--calendar-state-cursor org-journal-grid--state)
          (org-journal-grid--cursor-state-create
           :surface 'rail :day day :minute 0 :lane lane)
          (org-journal-grid--calendar-state-selected-id org-journal-grid--state)
          (and block (org-journal-grid-block-id block)))))

(defun org-journal-grid-cursor-forward (&optional count)
  "Move the cursor COUNT fifteen-minute slots later."
  (interactive "p")
  (let ((count (or count 1)))
    (if (< count 0)
        (org-journal-grid-cursor-backward (- count))
      (if (org-journal-grid--reveal-cursor)
          (org-journal-grid--cursor-moved)
        (dotimes (_ count)
          (let ((cursor (org-journal-grid--cursor)))
            (if (eq (org-journal-grid--cursor-state-surface cursor) 'rail)
                (let ((next (1+ (org-journal-grid--cursor-state-lane cursor)))
                      (rows (org-journal-grid--all-day-visible-rows)))
                  (if (> next rows)
                      (org-journal-grid--set-cursor (org-journal-grid--cursor-state-day cursor) 0)
                    (org-journal-grid--set-all-day-cursor
                     (org-journal-grid--cursor-state-day cursor) next)))
              (org-journal-grid--set-cursor
               (org-journal-grid--cursor-state-day cursor)
               (+ (org-journal-grid--cursor-state-minute cursor)
                  org-journal-grid-cursor-step-minutes)))))
        (org-journal-grid--cursor-moved)))))

(defun org-journal-grid-cursor-backward (&optional count)
  "Move the cursor COUNT stops earlier."
  (interactive "p")
  (let ((count (or count 1)))
    (if (< count 0)
        (org-journal-grid-cursor-forward (- count))
      (if (org-journal-grid--reveal-cursor)
          (org-journal-grid--cursor-moved)
        (dotimes (_ count)
          (let ((cursor (org-journal-grid--cursor)))
            (cond
             ((eq (org-journal-grid--cursor-state-surface cursor) 'rail)
              (org-journal-grid--set-all-day-cursor
               (org-journal-grid--cursor-state-day cursor)
               (max 0 (1- (org-journal-grid--cursor-state-lane cursor)))))
             ((= (org-journal-grid--cursor-state-minute cursor) 0)
              (org-journal-grid--set-all-day-cursor
               (org-journal-grid--cursor-state-day cursor)
               (org-journal-grid--all-day-visible-rows)))
             (t
              (org-journal-grid--set-cursor
               (org-journal-grid--cursor-state-day cursor)
               (- (org-journal-grid--cursor-state-minute cursor)
                  org-journal-grid-cursor-step-minutes))))))
        (org-journal-grid--cursor-moved)))))

(defun org-journal-grid-cursor-forward-slot (&optional count)
  "Move the cursor COUNT fifteen-minute slots later."
  (interactive "p")
  (org-journal-grid--move-cursor
   (* (or count 1) org-journal-grid-slot-minutes) 0))

(defun org-journal-grid-cursor-backward-slot (&optional count)
  "Move the cursor COUNT fifteen-minute slots earlier."
  (interactive "p")
  (org-journal-grid-cursor-forward-slot (- (or count 1))))

(defun org-journal-grid-cursor-forward-day (&optional count)
  "Move the cursor one lane to the right, or COUNT day columns.
Where several blocks start in the cursor's slot this walks their lanes
first, so two events at the same time are both reachable; once past the
last lane, or where there is only one, it moves by a day."
  (interactive "p")
  (if (org-journal-grid--reveal-cursor)
      (org-journal-grid--cursor-moved)
    (let* ((count (or count 1))
           (cursor (org-journal-grid--cursor))
           (lane (or (org-journal-grid--cursor-state-lane cursor) 0))
           (lanes (org-journal-grid--lane-count (org-journal-grid--cursor-state-day cursor)
                                            (org-journal-grid--cursor-state-minute cursor))))
      (cond
       ((eq (org-journal-grid--cursor-state-surface cursor) 'rail)
        (let* ((target (+ (org-journal-grid--cursor-state-day cursor) count))
               (week-offset (* org-journal-grid-days
                               (floor target org-journal-grid-days)))
               (day (mod target org-journal-grid-days)))
          (when (/= week-offset 0)
            (org-journal-grid--reload-state
             (+ (org-journal-grid--calendar-state-week-start org-journal-grid--state) week-offset)))
          (org-journal-grid--set-all-day-cursor day lane)))
       ((and (> count 0) (< (1+ lane) lanes))
        (org-journal-grid--set-cursor (org-journal-grid--cursor-state-day cursor)
                                  (org-journal-grid--cursor-state-minute cursor) (1+ lane)))
       ((and (< count 0) (> lane 0))
        (org-journal-grid--set-cursor (org-journal-grid--cursor-state-day cursor)
                                  (org-journal-grid--cursor-state-minute cursor) (1- lane)))
       (t
        (let* ((target (+ (org-journal-grid--cursor-state-day cursor) count))
               (week-offset (* org-journal-grid-days
                               (floor target org-journal-grid-days)))
               (day (mod target org-journal-grid-days))
               (minute (org-journal-grid--cursor-state-minute cursor)))
          (when (/= week-offset 0)
            (org-journal-grid--reload-state
             (+ (org-journal-grid--calendar-state-week-start org-journal-grid--state) week-offset)))
          (org-journal-grid--set-cursor day minute 0))))
      (org-journal-grid--cursor-moved))))

(defun org-journal-grid-cursor-backward-day (&optional count)
  "Move the cursor one lane to the left, or COUNT day columns."
  (interactive "p")
  (org-journal-grid-cursor-forward-day (- (or count 1))))

(defun org-journal-grid-cursor-day-start ()
  "Move the cursor to midnight in its own day."
  (interactive)
  (unless (org-journal-grid--reveal-cursor)
    (org-journal-grid--set-cursor
     (org-journal-grid--cursor-state-day (org-journal-grid--cursor)) 0))
  (org-journal-grid--cursor-moved))

(defun org-journal-grid-cursor-day-end ()
  "Move the cursor to the last slot of its own day."
  (interactive)
  (unless (org-journal-grid--reveal-cursor)
    (org-journal-grid--set-cursor
     (org-journal-grid--cursor-state-day (org-journal-grid--cursor))
                              (- (* 60 24) org-journal-grid-slot-minutes)))
  (org-journal-grid--cursor-moved))

(defun org-journal-grid--page-minutes (&optional window)
  "Return one screenful expressed in minutes for WINDOW."
  (let ((window (or window (get-buffer-window (current-buffer) t))))
    (max org-journal-grid-slot-minutes
         (org-journal-grid--snap-minute
          (/ (- (if window (window-body-height window t) 400) 40)
             (org-journal-grid--pixels-per-minute))))))

(defun org-journal-grid-cursor-page-down (&optional count)
  "Move the cursor COUNT screenfuls later."
  (interactive "p")
  (org-journal-grid--move-cursor
   (* (or count 1) (org-journal-grid--page-minutes)) 0))

(defun org-journal-grid-cursor-page-up (&optional count)
  "Move the cursor COUNT screenfuls earlier."
  (interactive "p")
  (org-journal-grid--move-cursor
   (* (- (or count 1)) (org-journal-grid--page-minutes)) 0))

;;; Keyboard block selection

(defun org-journal-grid--block-absolute-start (block)
  "Return BLOCK's absolute week start minute."
  (+ (* (org-journal-grid-block-day block) 1440) (org-journal-grid-block-start block)))

(defun org-journal-grid--ordered-blocks ()
  "Return committed blocks in visible keyboard-navigation order.
For each date, all-day blocks anchored there follow their displayed lanes
from top to bottom, followed by that day's timed blocks.  A multi-day block
occurs once, at its first visible date."
  (let* ((all-day (org-journal-grid--all-day-layout))
         (timed (seq-filter
                 (lambda (block) (not (org-journal-grid-block-preview block)))
                 (org-journal-grid--timed-blocks)))
         result)
    (dotimes (day-index org-journal-grid-days)
      (let (day-all-day day-timed)
        (dolist (block all-day)
          (when (and (= day-index
                        (floor (org-journal-grid-block-rail-start block) 1440))
                     (< (org-journal-grid-block-rail-lane block)
                        org-journal-grid-all-day-max-lanes))
            (push block day-all-day)))
        (dolist (block timed)
          (when (= day-index (org-journal-grid-block-day block))
            (push block day-timed)))
        (setq result
              (append
               result
               (sort day-all-day
                     (lambda (left right)
                       (< (org-journal-grid-block-rail-lane left)
                          (org-journal-grid-block-rail-lane right))))
               (sort day-timed
                     (lambda (left right)
                       (let ((ls (org-journal-grid-block-start left))
                             (rs (org-journal-grid-block-start right)))
                         (if (= ls rs)
                             (string< (format "%S" (org-journal-grid-block-id left))
                                      (format "%S" (org-journal-grid-block-id right)))
                           (< ls rs)))))))))
    result))

(defun org-journal-grid--goto-block (block)
  "Move the cursor to BLOCK's own first slot to select it.
The lane records which of several blocks sharing that start is meant, so
co-starting entries stay individually reachable."
  (let* ((day (max 0 (min (org-journal-grid--last-day-index)
                          (org-journal-grid-block-day block))))
         (start (org-journal-grid-block-start block)))
    (if (org-journal-grid-block-all-day-p block)
        (let ((laid-out
               (cl-find (org-journal-grid-block-id block) (org-journal-grid--all-day-layout)
                        :key (lambda (candidate) (org-journal-grid-block-id candidate))
                        :test #'equal)))
          (org-journal-grid--set-all-day-cursor
           (floor (or (and laid-out
                           (org-journal-grid-block-rail-start laid-out))
                      0)
                  1440)
           (or (and laid-out (org-journal-grid-block-rail-lane laid-out)) 0))
          ;; Keep selection explicit when the event is hidden by the lane cap.
          (setf (org-journal-grid--calendar-state-selected-id org-journal-grid--state)
                (org-journal-grid-block-id block)))
      (let ((lane (or (cl-position
                       (org-journal-grid-block-id block)
                       (org-journal-grid--blocks-starting-at day start)
                       :key (lambda (candidate) (org-journal-grid-block-id candidate))
                       :test #'equal)
                      0)))
        (org-journal-grid--set-cursor day start lane)))
    (setf (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state) t)
    (org-journal-grid--cursor-moved)
    (org-journal-grid--scroll-cursor-into-view)))

(defun org-journal-grid--move-selection (direction)
  "Select the next block in DIRECTION across the visible week.
DIRECTION is 1 for later or -1 for earlier.  Without a selection the
search starts from the cursor."
  (let* ((blocks (org-journal-grid--ordered-blocks))
         (selected (org-journal-grid--selected-id))
         (index (and selected
                     (cl-position selected blocks
                                  :key (lambda (block) (org-journal-grid-block-id block))
                                  :test #'equal)))
         (from (org-journal-grid--cursor-absolute)))
    (cond
     ;; Walking the ordered list by index, rather than comparing start
     ;; times, is what keeps two blocks sharing a start reachable.
     (index
     (let ((next (+ index direction)))
        (if (and (>= next 0) (< next (length blocks)))
            (org-journal-grid--goto-block (nth next blocks))
          (org-journal-grid--move-selection-across-week direction))))
     ;; With nothing selected, land on the nearest block in that direction,
     ;; including one that starts exactly at the cursor.
     (t
      (let ((candidates
             (if (> direction 0)
                 (seq-filter
                  (lambda (block)
                    (>= (org-journal-grid--block-absolute-start block) from))
                  blocks)
               (nreverse
                (seq-filter
                 (lambda (block)
                   (<= (org-journal-grid--block-absolute-start block) from))
                 blocks)))))
        (if candidates
            (org-journal-grid--goto-block (car candidates))
          (org-journal-grid--move-selection-across-week direction)))))))

(defun org-journal-grid--move-selection-across-week (direction)
  "Move one week in DIRECTION and select its first or last block."
  (let* ((week-start (+ (org-journal-grid--calendar-state-week-start org-journal-grid--state)
                        (* direction org-journal-grid-days)))
         (state (org-journal-grid--load-state week-start))
         (blocks (let ((org-journal-grid--state state))
                   (org-journal-grid--ordered-blocks))))
    ;; Loading the prospective week separately keeps failed navigation a
    ;; no-op: do not replace the current week or manufacture an edge cursor
    ;; when there is no block to select.
    (if blocks
        (progn
          (setq-local org-journal-grid--state state)
          (setq-local org-journal-grid--static-inner nil)
          (org-journal-grid--goto-block
           (if (> direction 0) (car blocks) (car (last blocks)))))
      (message "No further block"))))

(defun org-journal-grid-next-block ()
  "Select the next block by start time, anywhere in the visible week."
  (interactive)
  (org-journal-grid--move-selection 1))

(defun org-journal-grid-previous-block ()
  "Select the previous block by start time, anywhere in the visible week."
  (interactive)
  (org-journal-grid--move-selection -1))

(defun org-journal-grid--block-at-cursor ()
  "Return the committed block the cursor points at, innermost first.
The block the cursor selects wins; failing that, any block overlapping the
cursor's slot counts, innermost first, so a nested child stays reachable
inside its parent.  Overlap rather than containment matters because an Org
range may start at 13:50, which lies inside the 13:45 slot but after it,
and such a block was unreachable while this asked for containment."
  (or (org-journal-grid--block (org-journal-grid--selected-id))
      (let ((cursor (org-journal-grid--cursor)))
        (if (eq (org-journal-grid--cursor-state-surface cursor) 'rail)
            (org-journal-grid--all-day-at (org-journal-grid--cursor-state-day cursor)
                                      (org-journal-grid--cursor-state-lane cursor))
          (let* ((slot-start (org-journal-grid--cursor-absolute))
                 (slot-end (+ slot-start org-journal-grid-slot-minutes)))
            (car (sort (seq-filter
                        (lambda (block)
                          (let* ((start
                                  (org-journal-grid--block-absolute-start block))
                                 (end (+ start
                                         (- (org-journal-grid-block-end block)
                                            (org-journal-grid-block-start block)))))
                            (and (not (org-journal-grid-block-preview block))
                                 (< start slot-end)
                                 (> end slot-start))))
                        (org-journal-grid--timed-blocks))
                       (lambda (left right)
                         (< (- (org-journal-grid-block-end left)
                               (org-journal-grid-block-start left))
                            (- (org-journal-grid-block-end right)
                               (org-journal-grid-block-start right)))))))))))

;;; Keyboard editing
;;
;; Every command below builds the same proposal a drag would and commits it
;; through `org-journal-grid--apply', so snapping, the fifteen-minute minimum,
;; week clamping, stale-marker detection, read-only refusal, and repeating
;; series handling are inherited rather than reimplemented.

(defun org-journal-grid--selected-block ()
  "Return the selected block, or signal a user error."
  (let ((block (org-journal-grid--block (org-journal-grid--selected-id))))
    (unless block
      (user-error "No block selected; press n, or put the cursor on a block's first slot"))
    block))

(defun org-journal-grid--follow-block (id day minute &optional all-day)
  "Put the cursor back on block ID, or on DAY and MINUTE if it is gone.
ALL-DAY selects the date rail when the block cannot be found.
Following by id preserves explicit selection and picks up the block's new
lane after an edit changes its layout."
  (if-let* ((block (org-journal-grid--block id)))
      (org-journal-grid--goto-block block)
    (if all-day
        (org-journal-grid--set-all-day-cursor
         (max 0 (min (org-journal-grid--last-day-index) day)) 0)
      (org-journal-grid--set-cursor day minute 0))
    (setf (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state) t)
    (org-journal-grid--cursor-moved)))

(defun org-journal-grid--keyboard-proposal (block minutes days edge)
  "Return one timed or all-day edit preview for BLOCK.
MINUTES and DAYS form the interval delta; EDGE selects a resize endpoint."
  (org-journal-grid--operation-create
   :kind (if edge 'resize 'move)
   :block (org-journal-grid--transform-block-range
           block (+ minutes (* days 1440)) edge)
   :replace-id (org-journal-grid-block-id block)))

(defun org-journal-grid--all-day-to-timed-proposal (block)
  "Return a proposal converting one-day date-only BLOCK at midnight."
  (let* ((copy (copy-org-journal-grid-block block))
         (start (+ (* (org-journal-grid-block-day block) 1440)
                   (org-journal-grid-block-start block)))
         (duration (- (+ (* (org-journal-grid-block-day block) 1440)
                         (org-journal-grid-block-end block))
                      start)))
    (unless (= duration 1440)
      (user-error "Multi-day blocks cannot move into the time grid"))
    (org-journal-grid--set-absolute-range
     copy start (+ start org-journal-grid-default-duration-minutes))
    (setf (org-journal-grid-block-time-kind copy) 'timed
          (org-journal-grid-block-preview copy) t)
    (org-journal-grid--operation-create
     :kind 'move :block copy :replace-id (org-journal-grid-block-id block))))

(defun org-journal-grid--commit-keyboard-edit (&optional buffer)
  "Commit BUFFER's pending keyboard block edit."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (timerp org-journal-grid--keyboard-edit-timer)
          (cancel-timer org-journal-grid--keyboard-edit-timer))
        (setq-local org-journal-grid--keyboard-edit-timer nil)
        (when-let* ((proposal org-journal-grid--keyboard-edit))
          (setq-local org-journal-grid--keyboard-edit nil)
          (let* ((block (org-journal-grid--operation-block proposal))
                 (id (org-journal-grid-block-id block))
                 (day (org-journal-grid-block-day block))
                 (minute (org-journal-grid-block-start block)))
            (org-journal-grid--apply proposal)
            (org-journal-grid--follow-block
             id day minute (org-journal-grid-block-all-day-p block))))))))

(defun org-journal-grid--schedule-keyboard-commit ()
  "Restart the idle timer that commits keyboard block movement."
  (when (timerp org-journal-grid--keyboard-edit-timer)
    (cancel-timer org-journal-grid--keyboard-edit-timer))
  (setq-local
   org-journal-grid--keyboard-edit-timer
   (run-with-idle-timer
    (max 0 org-journal-grid-keyboard-commit-delay) nil
    #'org-journal-grid--commit-keyboard-edit (current-buffer))))

(defun org-journal-grid--commit-before-unrelated-command ()
  "Commit a keyboard edit before running a command outside its edit family."
  (when (and org-journal-grid--keyboard-edit
             (not (memq this-command
                        '(org-journal-grid-move-later
                          org-journal-grid-move-earlier
                          org-journal-grid-move-next-day
                          org-journal-grid-move-previous-day
                          org-journal-grid-grow-end
                          org-journal-grid-shrink-end
                          org-journal-grid-grow-start
                          org-journal-grid-shrink-start
                          org-journal-grid-grow-all-day-end
                          org-journal-grid-shrink-all-day-end
                          org-journal-grid-grow-all-day-start
                          org-journal-grid-shrink-all-day-start))))
    (org-journal-grid--commit-keyboard-edit)))

(defun org-journal-grid--edit-selected (minutes days edge)
  "Shift the selected block by MINUTES and DAYS.
EDGE nil moves the whole block, `top' changes its start, and `bottom'
changes its end.  The cursor follows, so the key can be held down."
  (let* ((block (or (and org-journal-grid--keyboard-edit
                          (org-journal-grid--operation-block
                           org-journal-grid--keyboard-edit))
                    (org-journal-grid--selected-block)))
         (updater (org-journal-grid-backend-update-function org-journal-grid--backend)))
    (unless (and (org-journal-grid-block-event block) (functionp updater))
      (user-error "This backend cannot move or resize calendar entries"))
    (let* ((all-day (org-journal-grid-block-all-day-p block))
           (to-timed (and all-day (/= minutes 0)))
           (proposal (if to-timed
                         (org-journal-grid--all-day-to-timed-proposal block)
                       (org-journal-grid--keyboard-proposal
                        block minutes days edge)))
           (moved (org-journal-grid--operation-block proposal)))
      (setq-local org-journal-grid--keyboard-edit proposal)
      (if (and all-day (not to-timed))
          (progn
            (setf (org-journal-grid--calendar-state-preview org-journal-grid--state)
                  proposal)
            (let* ((laid-out
                    (cl-find (org-journal-grid-block-id moved)
                             (org-journal-grid--all-day-layout)
                             :key (lambda (candidate)
                                    (org-journal-grid-block-id candidate))
                             :test #'equal))
                   (day (max 0 (min (org-journal-grid--last-day-index) (floor
                                      (or (and laid-out
                                               (org-journal-grid-block-rail-start
                                                laid-out))
                                          0)
                                      1440)))))
              (org-journal-grid--set-all-day-cursor
               day (or (and laid-out
                            (org-journal-grid-block-rail-lane laid-out))
                       0))))
        (progn
          (when all-day
            (setf (org-journal-grid--calendar-state-preview org-journal-grid--state)
                  proposal))
          (org-journal-grid--set-cursor (org-journal-grid-block-day moved)
                                    (org-journal-grid-block-start moved) 0)))
      (setf (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state) t)
      (if (and all-day (not to-timed))
          (org-journal-grid--render-ui-change)
        (if to-timed
            (org-journal-grid--render-ui-change)
          (org-journal-grid--set-preview proposal)))
      (org-journal-grid--scroll-cursor-into-view)
      (org-journal-grid--schedule-keyboard-commit))))

(defun org-journal-grid-copy-to-next-day (&optional count)
  "Copy the selected block COUNT days later, at the same time.
The cursor follows the copy, so holding the key spreads one entry across
consecutive days instead of stacking every copy on the same one."
  (interactive "p")
  (let* ((block (org-journal-grid--selected-block))
         (start (org-journal-grid-block-start block))
         (day (+ (org-journal-grid-block-day block) (or count 1)))
         (title (org-journal-grid-block-title block)))
    (unless (<= 0 day (org-journal-grid--last-day-index))
      (user-error "That day is outside the visible week"))
    (let ((copy (copy-org-journal-grid-block block)))
      (setf (org-journal-grid-block-day copy) day)
      (org-journal-grid--backend-create
       title copy (org-journal-grid-block-event block)
       (org-journal-grid-event-source (org-journal-grid-block-event block))))
    ;; The new timestamp belongs to the same heading, but its event identity
    ;; is not known until the backend data is reloaded, so follow its slot.
    (let* ((candidates (org-journal-grid--blocks-starting-at day start))
           (lane (or (cl-position title candidates
                                  :key (lambda (candidate)
                                         (org-journal-grid-block-title candidate))
                                  :test #'equal)
                     0)))
      (org-journal-grid--set-cursor day start lane)
      (setf (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state) t)
      (org-journal-grid--render-dynamic t)
      (org-journal-grid--scroll-cursor-into-view))))

(defun org-journal-grid-copy-to-previous-day (&optional count)
  "Copy the selected block COUNT days earlier, at the same time."
  (interactive "p")
  (org-journal-grid-copy-to-next-day (- (or count 1))))

(defun org-journal-grid-move-later (&optional count)
  "Move the selected block COUNT fifteen-minute slots later."
  (interactive "p")
  (org-journal-grid--edit-selected
   (* (or count 1) org-journal-grid-slot-minutes) 0 nil))

(defun org-journal-grid-move-earlier (&optional count)
  "Move the selected block COUNT fifteen-minute slots earlier."
  (interactive "p")
  (org-journal-grid-move-later (- (or count 1))))

(defun org-journal-grid-move-next-day (&optional count)
  "Move the selected block COUNT days later."
  (interactive "p")
  (org-journal-grid--edit-selected 0 (or count 1) nil))

(defun org-journal-grid-move-previous-day (&optional count)
  "Move the selected block COUNT days earlier."
  (interactive "p")
  (org-journal-grid-move-next-day (- (or count 1))))

(defun org-journal-grid-grow-end (&optional count)
  "Move the selected block's end COUNT slots later.
This only ever resizes.  Fine cursor motion has its own keys, because
sharing one key made nudging the cursor resize whatever it had selected."
  (interactive "p")
  (org-journal-grid--edit-selected
   (* (or count 1) org-journal-grid-slot-minutes) 0 'bottom))

(defun org-journal-grid-shrink-end (&optional count)
  "Move the selected block's end COUNT slots earlier."
  (interactive "p")
  (org-journal-grid-grow-end (- (or count 1))))

(defun org-journal-grid-grow-start (&optional count)
  "Move the selected block's start COUNT slots earlier, keeping its end."
  (interactive "p")
  (org-journal-grid--edit-selected
   (* (- (or count 1)) org-journal-grid-slot-minutes) 0 'top))

(defun org-journal-grid-shrink-start (&optional count)
  "Move the selected block's start COUNT slots later, keeping its end."
  (interactive "p")
  (org-journal-grid-grow-start (- (or count 1))))

(defun org-journal-grid--require-all-day-selection ()
  "Return the selected date-only block, or signal a user error."
  (let ((block (org-journal-grid--selected-block)))
    (unless (org-journal-grid-block-all-day-p block)
      (user-error "This key resizes date-only blocks in the all-day rail"))
    block))

(defun org-journal-grid-grow-all-day-end (&optional count)
  "Move the selected date-only block's end COUNT days later."
  (interactive "p")
  (org-journal-grid--require-all-day-selection)
  (org-journal-grid--edit-selected 0 (or count 1) 'bottom))

(defun org-journal-grid-shrink-all-day-end (&optional count)
  "Move the selected date-only block's end COUNT days earlier."
  (interactive "p")
  (org-journal-grid-grow-all-day-end (- (or count 1))))

(defun org-journal-grid-grow-all-day-start (&optional count)
  "Move the selected date-only block's start COUNT days earlier."
  (interactive "p")
  (org-journal-grid--require-all-day-selection)
  (org-journal-grid--edit-selected 0 (- (or count 1)) 'top))

(defun org-journal-grid-shrink-all-day-start (&optional count)
  "Move the selected date-only block's start COUNT days later."
  (interactive "p")
  (org-journal-grid-grow-all-day-start (- (or count 1))))

(defun org-journal-grid-create-at-cursor ()
  "Create a block at the cursor.
Rail cells create a one-day date-only entry without a duration prompt;
time-grid cells prompt for the timed duration as usual."
  (interactive)
  (org-journal-grid--reveal-cursor)
  (let* ((cursor (org-journal-grid--ensure-cursor))
         (all-day (eq (org-journal-grid--cursor-state-surface cursor) 'rail))
         (entry (org-journal-grid--read-entry))
         (title (car entry)))
    (if (string-empty-p title)
        (message "Nothing created")
      (let* ((minutes (if all-day
                          1440
                        (org-journal-grid--read-minutes
                         org-journal-grid-default-duration-minutes)))
             (start (if all-day 0 (org-journal-grid--cursor-state-minute cursor)))
             (block (org-journal-grid--make-block
                     'new (org-journal-grid--cursor-state-day cursor) start (+ start minutes)
                     title 'blue nil (if all-day 'all-day 'timed))))
        (org-journal-grid--backend-create title block nil (cdr entry))))))

(defun org-journal-grid-open-at-cursor ()
  "Visit the block under the cursor.  Empty slots do nothing."
  (interactive)
  (org-journal-grid--reveal-cursor)
  (let ((block (org-journal-grid--block-at-cursor)))
    (if (null block)
        (message "No journal entry here")
      (let ((event (org-journal-grid-block-event block))
            (visitor (and org-journal-grid--backend
                          (org-journal-grid-backend-visit-function
                           org-journal-grid--backend))))
        (if (and event (functionp visitor))
            (funcall visitor event)
          (message "No source to visit"))))))

;;; Keyboard copy, cut, and yank

(defvar org-journal-grid--kill nil
  "Plist describing the most recently copied block.
Holds :title, :minutes, :all-day, and the opaque :event needed to
reproduce the entry's content.  :target is the backend record to which a
yank with a prefix adds the copied timestamp.  :cut records whether the
source timestamp was removed and must therefore be restored on yank.")

(defun org-journal-grid-copy-selected ()
  "Copy the selected block for a later yank."
  (interactive)
  (let ((block (org-journal-grid--selected-block)))
    (setq org-journal-grid--kill
          (list :title (org-journal-grid-block-title block)
                :minutes (- (org-journal-grid-block-end block) (org-journal-grid-block-start block))
                :all-day (and (org-journal-grid-block-all-day-p block) t)
                :event (org-journal-grid-block-event block)
                :cut nil
                :target (org-journal-grid-event-source
                         (org-journal-grid-block-event block))))
    (message "Copied %s" (org-journal-grid-block-title block))))

(defun org-journal-grid-cut-selected ()
  "Cut the selected block for a later yank.
Unlike a copy, yanking a cut block adds its time back to the original
backend record instead of duplicating that record."
  (interactive)
  (let* ((block (org-journal-grid--selected-block))
         (event (org-journal-grid-block-event block)))
    (setq org-journal-grid--kill
          (list :title (org-journal-grid-block-title block)
                :minutes (- (org-journal-grid-block-end block) (org-journal-grid-block-start block))
                :all-day (and (org-journal-grid-block-all-day-p block) t)
                :event event
                :cut t
                :target (org-journal-grid-event-source event)))
    (org-journal-grid-remove-selected t)
    (message "Cut %s" (org-journal-grid-block-title block))))

(defun org-journal-grid-yank (&optional add-occurrence)
  "Yank the most recently copied or cut block at the cursor.
Ordinarily a copied block becomes an independent backend entry.  With a
prefix argument ADD-OCCURRENCE, add its timestamp to the original entry
instead.  A cut block always returns to its original entry."
  (interactive "P")
  (unless org-journal-grid--kill
    (user-error "Nothing to yank; select a block and press M-w or C-w"))
  (org-journal-grid--reveal-cursor)
  (let* ((cursor (org-journal-grid--ensure-cursor))
         (rail (eq (org-journal-grid--cursor-state-surface cursor) 'rail))
         (source-all-day (plist-get org-journal-grid--kill :all-day))
         (source-minutes (plist-get org-journal-grid--kill :minutes))
         (_ (when (and source-all-day (not rail) (> source-minutes 1440))
              (user-error "Multi-day blocks can only be pasted in the all-day rail")))
         (start (if rail 0 (org-journal-grid--cursor-state-minute cursor)))
         (minutes (cond
                   (rail (if source-all-day source-minutes 1440))
                   (source-all-day org-journal-grid-default-duration-minutes)
                   (t source-minutes)))
         (block (org-journal-grid--make-block
                 'yank (org-journal-grid--cursor-state-day cursor)
                 start (+ start minutes)
                 (plist-get org-journal-grid--kill :title) 'blue nil
                 (if rail 'all-day 'timed))))
    (org-journal-grid--backend-create
     (plist-get org-journal-grid--kill :title) block
     (plist-get org-journal-grid--kill :event)
     (and (or add-occurrence (plist-get org-journal-grid--kill :cut))
          (plist-get org-journal-grid--kill :target)))))

;;; Keyboard re-timing

(defun org-journal-grid--read-timestamp (absolute-start duration)
  "Read a start minute and duration, prefilled from ABSOLUTE-START.
A backend may supply its own reader, which is how Org's date prompt gets
used without the renderer knowing about Org.  DURATION is passed through
for that reader to prefill.  Returns a cons of start and duration, where
a nil duration means the caller should ask."
  (funcall (or (org-journal-grid-backend-read-timestamp-function
                org-journal-grid--backend)
               #'org-journal-grid-read-timestamp-default)
           absolute-start duration))

(defun org-journal-grid-read-timestamp-default (absolute-start duration)
  "Read a start time textually, prefilled from ABSOLUTE-START.
DURATION is unused: this reader has no range syntax, so it always returns
nil for the duration and leaves the caller to ask."
  (ignore duration)
  (let* ((day (floor absolute-start 1440))
         (minute (% absolute-start 1440))
         (date (calendar-gregorian-from-absolute day))
         (prefill (format "%04d-%02d-%02d %02d:%02d"
                          (nth 2 date) (nth 0 date) (nth 1 date)
                          (/ minute 60) (% minute 60)))
         (answer (read-string "Start (YYYY-MM-DD HH:MM): " prefill))
         (parsed (and (string-match
                       "\\([0-9]\\{4\\}\\)-\\([0-9]+\\)-\\([0-9]+\\)[ \t]+\\([0-9]+\\):\\([0-9]+\\)"
                       answer)
                      (+ (* 1440 (calendar-absolute-from-gregorian
                                  (list (string-to-number (match-string 2 answer))
                                        (string-to-number (match-string 3 answer))
                                        (string-to-number (match-string 1 answer)))))
                         (* 60 (string-to-number (match-string 4 answer)))
                         (string-to-number (match-string 5 answer))))))
    (cons (or parsed absolute-start) nil)))

(defun org-journal-grid--read-minutes (default)
  "Read a duration in minutes, offering DEFAULT."
  (let* ((answer (read-string
                  (format "Duration in minutes (default %d): " default)
                  nil nil (number-to-string default)))
         (minutes (string-to-number answer)))
    (if (>= minutes org-journal-grid-slot-minutes)
        minutes
      org-journal-grid-slot-minutes)))

(defun org-journal-grid-retime-selected ()
  "Re-time the selected block by reading a new start and duration.
The reader is prefilled with the block's current value.  When it reports
a duration, as a typed time range does, that is used directly; otherwise
the duration is asked for separately, prefilled with the current one."
  (interactive)
  (let* ((block (org-journal-grid--selected-block))
         (week-start (org-journal-grid--calendar-state-week-start org-journal-grid--state))
         (absolute-start (+ (* (+ week-start (org-journal-grid-block-day block)) 1440)
                            (org-journal-grid-block-start block)))
         (minutes (- (org-journal-grid-block-end block) (org-journal-grid-block-start block)))
         (answer (org-journal-grid--read-timestamp absolute-start minutes))
         (start (car answer))
         (duration (max org-journal-grid-slot-minutes
                        (or (cdr answer)
                            (org-journal-grid--read-minutes minutes))))
         (updater (org-journal-grid-backend-update-function
                   org-journal-grid--backend)))
    (unless (and (org-journal-grid-block-event block) (functionp updater))
      (user-error "This backend cannot re-time calendar entries"))
    (org-journal-grid--call-update
     updater (org-journal-grid-block-event block) start (+ start duration) nil
     (org-journal-grid-block-time-kind block))
    (org-journal-grid--refresh-data)))

;;; Dates

(defun org-journal-grid-goto-date ()
  "Show the trailing window of calendar days ending on a date read from the user."
  (interactive)
  (let* ((cursor-absolute
          (+ (* (+ (org-journal-grid--calendar-state-week-start org-journal-grid--state)
                   (org-journal-grid--cursor-state-day
                    (org-journal-grid--ensure-cursor)))
                1440)
             (org-journal-grid--cursor-state-minute
              (org-journal-grid--ensure-cursor))))
         (answer (org-journal-grid--read-timestamp
                   cursor-absolute org-journal-grid-slot-minutes))
         (absolute (floor (car answer) 1440)))
    (org-journal-grid--reload-state (org-journal-grid--range-start absolute))
    (org-journal-grid--set-cursor
     (- absolute (org-journal-grid--calendar-state-week-start org-journal-grid--state))
     (% (car answer) 1440))
    (org-journal-grid--refresh)
    (org-journal-grid--scroll-cursor-into-view)))

(defun org-journal-grid-goto-today ()
  "Show the trailing window of calendar days ending today.
A visible cursor moves to the current slot; a hidden one stays hidden,
since jumping dates should not conjure a cursor nobody asked for."
  (interactive)
  (let ((today (calendar-absolute-from-gregorian (calendar-current-date)))
        (visible (and (org-journal-grid--cursor) t)))
    (org-journal-grid--reload-state (org-journal-grid--range-start today))
    (setf (org-journal-grid--calendar-state-cursor org-journal-grid--state)
          (and visible
               (org-journal-grid--default-cursor
                (org-journal-grid--calendar-state-week-start
                 org-journal-grid--state))))
    (org-journal-grid--refresh)
    (org-journal-grid--scroll-cursor-into-view)))

(defun org-journal-grid-remove-or-page-up (&optional count)
  "Remove the selected block, or page the cursor up when none is selected.
COUNT controls the number of pages to move.
Backspace sends DEL, so this key has to serve both the delete people
expect on a selected block and the paging DEL means in a view buffer."
  (interactive "p")
  (if (org-journal-grid--selected-id)
      (org-journal-grid-remove-selected)
    (org-journal-grid-cursor-page-up count)))

(defun org-journal-grid-dismiss ()
  "Hide the cursor and clear the selection and any preview.
The cursor's position is remembered, so a later movement key resumes from
where it was left.  Only an explicit refresh forgets it."
  (interactive)
  (setf (org-journal-grid--calendar-state-preview org-journal-grid--state) nil
        (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state) nil)
  (org-journal-grid--render-ui-change))

(defun org-journal-grid-wheel-up (event)
  "Scroll the SVG upward in response to EVENT."
  (interactive "e")
  (when-let* ((window (org-journal-grid--event-window event)))
    (org-journal-grid-scroll -90 window)))

(defun org-journal-grid-wheel-down (event)
  "Scroll the SVG downward in response to EVENT."
  (interactive "e")
  (when-let* ((window (org-journal-grid--event-window event)))
    (org-journal-grid-scroll 90 window)))

(defun org-journal-grid-precision-scroll (event)
  "Scroll the tiled SVG using the pixel delta carried by EVENT.
This bypasses generic precision scrollers whose text-position assumptions do
not hold for a buffer made of tall image glyphs."
  (interactive "e")
  (when-let* ((window (org-journal-grid--event-window event))
              (delta (cdr-safe (nth 4 event)))
              ((numberp delta)))
    ;; Precision-wheel deltas describe content motion, while this renderer's
    ;; absolute scroll coordinate increases down the calendar.
    (org-journal-grid-scroll (- delta) window)))

(defvar org-journal-grid--precision-scroll-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<remap> <pixel-scroll-precision>"
                #'org-journal-grid-precision-scroll)
    map)
  "Buffer-local override map for generic precision scrolling modes.")

(defun org-journal-grid--install-precision-scroll-override ()
  "Prefer the calendar's image-aware scroller in the current buffer."
  (setq-local minor-mode-overriding-map-alist
              (assq-delete-all 'pixel-scroll-precision-mode
                               minor-mode-overriding-map-alist))
  (push `(pixel-scroll-precision-mode . ,org-journal-grid--precision-scroll-map)
        minor-mode-overriding-map-alist))

(defun org-journal-grid-shift-week (days)
  "Move the SVG week by DAYS.
The window is clamped so it never shows dates after today."
  (let* ((old (org-journal-grid--calendar-state-week-start org-journal-grid--state))
         (new (org-journal-grid--clamp-week-start (+ old days))))
    (if (= new old)
        (message "Cannot show future dates")
      (org-journal-grid--reload-state new)
      (org-journal-grid--refresh))))

(defun org-journal-grid-previous-week ()
  "Show the previous week."
  (interactive)
  (org-journal-grid-shift-week (- org-journal-grid-days)))

(defun org-journal-grid-next-week ()
  "Show the next week."
  (interactive)
  (org-journal-grid-shift-week org-journal-grid-days))

(defun org-journal-grid-backward-day ()
  "Move the seven-day calendar backward by one day."
  (interactive)
  (org-journal-grid-shift-week -1))

(defun org-journal-grid-forward-day ()
  "Move the seven-day calendar forward by one day."
  (interactive)
  (org-journal-grid-shift-week 1))

(defun org-journal-grid--refresh-data ()
  "Reload the displayed week without moving the cursor or viewport."
  (org-journal-grid--reload-state (org-journal-grid--calendar-state-week-start org-journal-grid--state))
  (org-journal-grid--refresh t))

(defun org-journal-grid-reload ()
  "Reload events without resetting cursor or viewport."
  (interactive)
  (org-journal-grid--refresh-data))

(defun org-journal-grid-refresh ()
  "Manually reload the displayed week and reset its cursor and viewport."
  (interactive)
  (org-journal-grid--reload-state (org-journal-grid--calendar-state-week-start org-journal-grid--state))
  (setf (org-journal-grid--calendar-state-cursor org-journal-grid--state) nil
        (org-journal-grid--calendar-state-cursor-visible org-journal-grid--state) nil)
  (setq-local org-journal-grid--saved-vscroll 0)
  (org-journal-grid--refresh)
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (org-journal-grid--set-vscroll window 0)))

(declare-function org-journal-grid-toggle-todo "org-journal-grid")
(declare-function org-journal-grid-increase-days "org-journal-grid")
(declare-function org-journal-grid-decrease-days "org-journal-grid")

(defvar org-journal-grid-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [down-mouse-1]
                #'org-journal-grid-press)
    (define-key map [double-down-mouse-1]
                #'org-journal-grid-ignore-double-press)
    (define-key map [s-down-mouse-1]
                #'org-journal-grid-press)
    (define-key map [S-down-mouse-1]
                #'org-journal-grid-press)
    (define-key map [mouse-1] #'org-journal-grid-click)
    (define-key map [double-mouse-1] #'org-journal-grid-visit)
    (define-key map [mouse-movement] #'org-journal-grid-pointer-feedback)
    (define-key map [header-line mouse-1] #'org-journal-grid-header-click)
    (define-key map [header-line down-mouse-1] #'org-journal-grid-header-press)
    (define-key map [header-line double-down-mouse-1]
                #'org-journal-grid-ignore-double-press)
    (define-key map [header-line s-down-mouse-1] #'org-journal-grid-header-press)
    (define-key map [header-line S-down-mouse-1] #'org-journal-grid-header-press)
    (define-key map [header-line double-mouse-1] #'org-journal-grid-header-visit)
    (dolist (area '(calendar-block calendar-resize))
      (define-key map (vector area 'down-mouse-1)
                  #'org-journal-grid-press)
      (define-key map (vector area 'double-down-mouse-1)
                  #'org-journal-grid-ignore-double-press)
      (define-key map (vector area 's-down-mouse-1)
                  #'org-journal-grid-press)
      (define-key map (vector area 'S-down-mouse-1)
                  #'org-journal-grid-press)
      (define-key map (vector area 'mouse-1)
                  #'org-journal-grid-click)
      (define-key map (vector area 'double-mouse-1)
                  #'org-journal-grid-visit)
      (define-key map (vector area 'mouse-movement)
                  #'org-journal-grid-pointer-feedback)
      (dolist (wheel '(wheel-up double-wheel-up triple-wheel-up))
        (define-key map (vector area wheel)
                    #'org-journal-grid-wheel-up))
      (dolist (wheel '(wheel-down double-wheel-down triple-wheel-down))
        (define-key map (vector area wheel)
                    #'org-journal-grid-wheel-down)))
    (dolist (wheel '(wheel-up double-wheel-up triple-wheel-up))
      (define-key map (vector wheel)
                  #'org-journal-grid-wheel-up))
    (dolist (wheel '(wheel-down double-wheel-down triple-wheel-down))
      (define-key map (vector wheel)
                  #'org-journal-grid-wheel-down))
    (keymap-set map "b" #'org-journal-grid-backward-day)
    (keymap-set map "f" #'org-journal-grid-forward-day)
    (keymap-set map "[" #'org-journal-grid-backward-day)
    (keymap-set map "]" #'org-journal-grid-forward-day)
    (keymap-set map "M-b" #'org-journal-grid-previous-week)
    (keymap-set map "M-f" #'org-journal-grid-next-week)
    (keymap-set map "{" #'org-journal-grid-previous-week)
    (keymap-set map "}" #'org-journal-grid-next-week)
    (keymap-set map "g" #'org-journal-grid-reload)
    ;; In vanilla Emacs the fully modified `C-x C-+' invokes text scaling,
    ;; while `C-x +' balances windows.  The latter is a convenient calendar-
    ;; local alias and matches how this command is commonly described.
    (keymap-set map "C-x +" #'text-scale-adjust)
    ;; Cursor motion, which scrolls the view to follow it.
    (keymap-set map "C-n" #'org-journal-grid-cursor-forward)
    (keymap-set map "C-p" #'org-journal-grid-cursor-backward)
    (keymap-set map "<down>" #'org-journal-grid-cursor-forward)
    (keymap-set map "<up>" #'org-journal-grid-cursor-backward)
    (keymap-set map "C-f" #'org-journal-grid-cursor-forward-day)
    (keymap-set map "C-b" #'org-journal-grid-cursor-backward-day)
    (keymap-set map "<right>" #'org-journal-grid-cursor-forward-day)
    (keymap-set map "<left>" #'org-journal-grid-cursor-backward-day)
    (keymap-set map "C-v" #'org-journal-grid-cursor-page-down)
    (keymap-set map "M-v" #'org-journal-grid-cursor-page-up)
    (keymap-set map "SPC" #'org-journal-grid-cursor-page-down)
    (keymap-set map "DEL" #'org-journal-grid-cursor-page-up)
    (keymap-set map "<backspace>" #'org-journal-grid-cursor-page-up)
    (keymap-set map "C-l" #'org-journal-grid-recenter)
    (keymap-set map "C-a" #'org-journal-grid-cursor-day-start)
    (keymap-set map "C-e" #'org-journal-grid-cursor-day-end)
    ;; Block selection.
    (keymap-set map "n" #'org-journal-grid-next-block)
    (keymap-set map "p" #'org-journal-grid-previous-block)
    (keymap-set map "RET" #'org-journal-grid-open-at-cursor)
    (keymap-set map "C-g" #'org-journal-grid-dismiss)
    (keymap-set map "t" #'org-journal-grid-toggle-todo)
    (keymap-set map "-" #'org-journal-grid-decrease-days)
    (keymap-set map "+" #'org-journal-grid-increase-days)
    (keymap-set map "=" #'org-journal-grid-increase-days)
    (dotimes (i 9)
      (keymap-set map (format "%d" (1+ i)) #'digit-argument))
    ;; Dates and files.
    (keymap-set map "j" #'org-journal-grid-goto-date)
    (keymap-set map "." #'org-journal-grid-goto-today)
    (keymap-set map "q" #'quit-window)
    map))

;; Keep reevaluation in a running Emacs from retaining the previous bindings.
(dolist (key '("M-S-<down>" "M-S-<up>" "M-S-<right>" "M-S-<left>"
               "M-S-s-<right>" "M-S-s-<left>"
               "<remap> <undo>" "<remap> <undo-only>" "<remap> <undo-redo>"
               "C-/" "C-_" "C-x u" "M-_"
               "d" "e" "<delete>"
               "M-<down>" "M-<up>" "M-<right>" "M-<left>"
               "S-<down>" "S-<up>" "C-S-<up>" "C-S-<down>"
               "S-<right>" "S-<left>" "C-S-<left>" "C-S-<right>"
               "M-w" "C-w" "C-y"))
  (keymap-unset org-journal-grid-mode-map key t))
(keymap-set org-journal-grid-mode-map "C-x +" #'text-scale-adjust)
(keymap-unset org-journal-grid-mode-map "M-s-<right>" t)
(keymap-unset org-journal-grid-mode-map "M-s-<left>" t)
(keymap-set org-journal-grid-mode-map "g" #'org-journal-grid-reload)
(keymap-set org-journal-grid-mode-map "t" #'org-journal-grid-toggle-todo)
(keymap-set org-journal-grid-mode-map "-" #'org-journal-grid-decrease-days)
(keymap-set org-journal-grid-mode-map "+" #'org-journal-grid-increase-days)
(keymap-set org-journal-grid-mode-map "=" #'org-journal-grid-increase-days)
(keymap-set org-journal-grid-mode-map "[" #'org-journal-grid-backward-day)
(keymap-set org-journal-grid-mode-map "]" #'org-journal-grid-forward-day)
(keymap-set org-journal-grid-mode-map "{" #'org-journal-grid-previous-week)
(keymap-set org-journal-grid-mode-map "}" #'org-journal-grid-next-week)
(keymap-set org-journal-grid-mode-map "." #'org-journal-grid-goto-today)
(dotimes (i 9)
  (keymap-set org-journal-grid-mode-map (format "%d" (1+ i)) #'digit-argument))
(keymap-set org-journal-grid-mode-map "DEL" #'org-journal-grid-cursor-page-up)
(keymap-set org-journal-grid-mode-map "<backspace>" #'org-journal-grid-cursor-page-up)
;; These live outside the `defvar' initializer so evaluating an updated
;; package installs them in an already-running Emacs as well.
(define-key org-journal-grid-mode-map [s-down-mouse-1]
            #'org-journal-grid-press)
(define-key org-journal-grid-mode-map [S-down-mouse-1]
            #'org-journal-grid-press)
(define-key org-journal-grid-mode-map [double-down-mouse-1]
            #'org-journal-grid-ignore-double-press)
(dolist (area '(calendar-block calendar-resize))
  (define-key org-journal-grid-mode-map (vector area 's-down-mouse-1)
              #'org-journal-grid-press)
  (define-key org-journal-grid-mode-map (vector area 'S-down-mouse-1)
              #'org-journal-grid-press)
  (define-key org-journal-grid-mode-map (vector area 'double-down-mouse-1)
              #'org-journal-grid-ignore-double-press)
  (define-key org-journal-grid-mode-map (vector area 'mouse-movement)
              #'org-journal-grid-pointer-feedback))
(define-key org-journal-grid-mode-map [mouse-movement]
            #'org-journal-grid-pointer-feedback)
(define-key org-journal-grid-mode-map [header-line mouse-1]
            #'org-journal-grid-header-click)
(define-key org-journal-grid-mode-map [header-line down-mouse-1]
            #'org-journal-grid-header-press)
(define-key org-journal-grid-mode-map [header-line double-down-mouse-1]
            #'org-journal-grid-ignore-double-press)
(define-key org-journal-grid-mode-map [header-line s-down-mouse-1]
            #'org-journal-grid-header-press)
(define-key org-journal-grid-mode-map [header-line S-down-mouse-1]
            #'org-journal-grid-header-press)
(define-key org-journal-grid-mode-map [header-line double-mouse-1]
            #'org-journal-grid-header-visit)

(define-derived-mode org-journal-grid-mode special-mode
  "Journal Grid"
  "Major mode for an SVG week calendar.

The calendar owns one explicit cursor and one selected event.  The cursor
can occupy either a timed slot or an all-day rail cell; both mouse and
keyboard changes pass through the same damage-based renderer.

\\{org-journal-grid-mode-map}"
  (setq-local truncate-lines t)
  (setq-local cursor-type nil)
  ;; Image rows otherwise gain one baseline pixel and visible hour seams.
  (setq-local line-spacing -1)
  (setq-local track-mouse t)
  (setq-local mouse-fine-grained-tracking t)
  (setq-local auto-window-vscroll t)
  (org-journal-grid--install-text-scale-hook)
  (org-journal-grid--install-precision-scroll-override)
  (add-hook 'window-size-change-functions
            #'org-journal-grid--window-resized nil t)
  (add-hook 'pre-command-hook
            #'org-journal-grid--commit-before-unrelated-command nil t)
  (add-hook 'kill-buffer-hook
            #'org-journal-grid--cancel-timers nil t))

(defun org-journal-grid--center-now (window)
  "Center the current time vertically in WINDOW."
  (let* ((now (decode-time))
         (minute (+ (* 60 (decoded-time-hour now))
                    (decoded-time-minute now)))
         (start-minute (* 60 org-journal-grid-start-hour))
         (y (+ (org-journal-grid--grid-top-inset)
               (* (- minute start-minute)
                  (org-journal-grid--pixels-per-minute))))
         (target (max 0 (- y (/ (window-body-height window t) 2)))))
    (org-journal-grid--set-vscroll window target)))

;;;###autoload
(defun org-journal-grid-open (backend &optional absolute-date)
  "Open BACKEND on the trailing window of calendar days ending on ABSOLUTE-DATE.
Nil ABSOLUTE-DATE ends the window today.
Revisiting an existing calendar retains its pixel scroll position."
  (unless (org-journal-grid-backend-p backend)
    (user-error "A calendar backend is required"))
  (let* ((existing (get-buffer org-journal-grid-buffer-name))
         (buffer (or existing
                     (get-buffer-create org-journal-grid-buffer-name)))
         (requested-week (org-journal-grid--range-start absolute-date))
         refreshp)
    (if existing
        (with-current-buffer buffer
          (let ((current-week (org-journal-grid--calendar-state-week-start org-journal-grid--state)))
            (when (or org-journal-grid--stale
                      (not (eq org-journal-grid--backend backend))
                      (/= requested-week current-week))
              (setq-local org-journal-grid--backend backend)
              (setq-local org-journal-grid--state
                          (org-journal-grid--load-state requested-week))
              (setq-local org-journal-grid--stale nil)
              (setq refreshp t))))
      (with-current-buffer buffer
        (org-journal-grid-mode)
        (setq-local org-journal-grid--backend backend)
        (setq-local org-journal-grid--state
                    (org-journal-grid--load-state
                     (org-journal-grid--range-start absolute-date)))
        (let ((owner buffer))
          (setq-local
           org-journal-grid--clock-timer
           (run-at-time
            60 60
            (lambda () (org-journal-grid--clock-tick owner))))
          (when (and (numberp org-journal-grid-data-refresh-seconds)
                     (> org-journal-grid-data-refresh-seconds 0))
            (setq-local
             org-journal-grid--data-timer
             (run-at-time
              org-journal-grid-data-refresh-seconds
              org-journal-grid-data-refresh-seconds
              (lambda () (org-journal-grid--data-tick owner))))))))
    (switch-to-buffer buffer)
    (let ((window (get-buffer-window buffer t)))
      (with-current-buffer buffer
        (cond
         ((null existing)
          (org-journal-grid--refresh)
          (when window
            (org-journal-grid--center-now window)))
         ((or refreshp
              (/= (org-journal-grid--window-width)
                  (or org-journal-grid--last-width -1)))
          (org-journal-grid--refresh t)))
        (when window
          (org-journal-grid--schedule-scroll-restore window))))
    buffer))

(add-hook 'enable-theme-functions
          #'org-journal-grid--theme-changed)
(add-hook 'disable-theme-functions
          #'org-journal-grid--theme-changed)
(add-hook 'window-buffer-change-functions
          #'org-journal-grid--restore-frame-calendars)

(provide 'org-journal-grid-render)
;;; org-journal-grid-render.el ends here
