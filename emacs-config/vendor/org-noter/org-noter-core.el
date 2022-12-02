;;; org-noter-core.el --- Core functions of Org-noter       -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Gonçalo Santos

;; Author: Gonçalo Santos (aka. weirdNox@GitHub)
;; Homepage: https://github.com/weirdNox/org-noter
;; Keywords: lisp pdf interleave annotate external sync notes documents org-mode
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6") (org "9.0"))
;; Version: 1.4.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The idea is to let you create notes that are kept in sync when you scroll through the
;; document, but that are external to it - the notes themselves live in an Org-mode file. As
;; such, this leverages the power of Org-mode (the notes may have outlines, latex fragments,
;; babel, etc...) while acting like notes that are made /in/ the document.

;; Also, I must thank Sebastian for the original idea and inspiration!
;; Link to the original Interleave package:
;; https://github.com/rudolfochrist/interleave

;;; Code:
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'pdf-tools)

(declare-function doc-view-goto-page "doc-view")
(declare-function image-display-size "image-mode")
(declare-function image-get-display-property "image-mode")
(declare-function image-mode-window-get "image-mode")
(declare-function image-scroll-up "image-mode")
(declare-function nov-render-document "ext:nov")
(declare-function org-attach-dir "org-attach")
(declare-function org-attach-file-list "org-attach")
(declare-function pdf-info-getannots "ext:pdf-info")
(declare-function pdf-info-gettext "ext:pdf-info")
(declare-function pdf-info-outline "ext:pdf-info")
(declare-function pdf-info-pagelinks "ext:pdf-info")
;; (declare-function pdf-util-tooltip-arrow "ext:pdf-util")
(declare-function pdf-view-active-region "ext:pdf-view")
(declare-function pdf-view-active-region-p "ext:pdf-view")
(declare-function pdf-view-active-region-text "ext:pdf-view")
(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function pdf-view-mode "ext:pdf-view")
(defvar nov-documents-index)
(defvar nov-file-name)

;; --------------------------------------------------------------------------------
;;; User variables
(defgroup org-noter nil
  "A synchronized, external annotator"
  :group 'convenience
  :version "25.3.1")

(defcustom org-noter-supported-modes '(doc-view-mode pdf-view-mode nov-mode djvu-read-mode)
  "Major modes that are supported by org-noter."
  :group 'org-noter
  :type '(repeat symbol))

(defcustom org-noter-property-doc-file "NOTER_DOCUMENT"
  "Name of the property that specifies the document."
  :group 'org-noter
  :type 'string)

(defcustom org-noter-property-note-location "NOTER_PAGE"
  "Name of the property that specifies the location of the current note.
The default value is still NOTER_PAGE for backwards compatibility."
  :group 'org-noter
  :type 'string)

(defcustom org-noter-default-heading-title "Notes for page $p$"
  "The default title for headings created with `org-noter-insert-note'.
$p$ is replaced with the number of the page or chapter you are in
at the moment."
  :group 'org-noter
  :type 'string)

(defcustom org-noter-notes-window-behavior '(start scroll)
  "This setting specifies in what situations the notes window should be created.

When the list contains:
- `start', the window will be created when starting a `org-noter' session.
- `scroll', it will be created when you go to a location with an associated note.
- `only-prev', it will be created when you go to a location without notes, but that
   has previous notes that are shown."
  :group 'org-noter
  :type '(set (const :tag "Session start" start)
              (const :tag "Scroll to location with notes" scroll)
              (const :tag "Scroll to location with previous notes only" only-prev)))

(defcustom org-noter-notes-window-location 'horizontal-split
  "Whether the notes should appear in the main frame (horizontal or vertical split) or in a separate frame.

Note that this will only have effect on session startup if `start'
is member of `org-noter-notes-window-behavior' (which see)."
  :group 'org-noter
  :type '(choice (const :tag "Horizontal" horizontal-split)
                 (const :tag "Vertical" vertical-split)
                 (const :tag "Other frame" other-frame)))

(define-obsolete-variable-alias 'org-noter-doc-split-percentage 'org-noter-doc-split-fraction "1.2.0")
(defcustom org-noter-doc-split-fraction '(0.5 . 0.5)
  "Fraction of the frame that the document window will occupy when split.
This is a cons of the type (HORIZONTAL-FRACTION . VERTICAL-FRACTION)."
  :group 'org-noter
  :type '(cons (number :tag "Horizontal fraction") (number :tag "Vertical fraction")))

(defcustom org-noter-auto-save-last-location nil
  "When non-nil, save the last visited location automatically; when starting a new session, go to that location."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-prefer-root-as-file-level nil
  "When non-nil, org-noter will always try to return the file-level property drawer
even when there are headings.

With the default value nil, org-noter will always use the first heading as root when
there is at least one heading."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-hide-other t
  "When non-nil, hide all headings not related to the command used.
For example, when scrolling to pages with notes, collapse all the
notes that are not annotating the current page."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-always-create-frame t
  "When non-nil, org-noter will always create a new frame for the session.
When nil, it will use the selected frame if it does not belong to any other session."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-disable-narrowing nil
  "Disable narrowing in notes/org buffer."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-use-indirect-buffer t
  "When non-nil, org-noter will create an indirect buffer of the calling
org file as a note buffer of the session.
When nil, it will use the real buffer."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-swap-window nil
  "By default `org-noter' will make a session by setting the buffer of the selected window
to the document buffer then split with the window of the notes buffer on the right.

If this variable is non-nil, the buffers of the two windows will be the other way around."
  :group 'org-noter
  :type 'boolean)


(defcustom org-noter-suggest-from-attachments t
  "When non-nil, org-noter will suggest files from the attachments
when creating a session, if the document is missing."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-separate-notes-from-heading nil
  "When non-nil, add an empty line between each note's heading and content."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-insert-selected-text-inside-note t
  "When non-nil, it will automatically append the selected text into an existing note."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-closest-tipping-point 0.3
  "Defines when to show the closest previous note.

Let x be (this value)*100. The following schematic represents the
view (eg. a page of a PDF):

+----+
|    | -> If there are notes in here, the closest previous note is not shown
+----+--> Tipping point, at x% of the view
|    | -> When _all_ notes are in here, below the tipping point, the closest
|    |    previous note will be shown.
+----+

When this value is negative, disable this feature.

This setting may be overridden in a document with the function
`org-noter-set-closest-tipping-point', which see."
  :group 'org-noter
  :type 'number)

(defcustom org-noter-default-notes-file-names '("Notes.org")
  "List of possible names for the default notes file, in increasing order of priority."
  :group 'org-noter
  :type '(repeat string))

(defcustom org-noter-notes-search-path '("~/Documents")
  "List of paths to check (non recursively) when searching for a notes file."
  :group 'org-noter
  :type '(repeat string))

(defcustom org-noter-arrow-delay 0.2
  "Number of seconds from when the command was invoked until the tooltip arrow appears.

When set to a negative number, the arrow tooltip is disabled.
This is needed in order to keep Emacs from hanging when doing many syncs."
  :group 'org-noter
  :type 'number)

(defcustom org-noter-doc-property-in-notes nil
  "If non-nil, every new note will have the document property too.
This makes moving notes out of the root heading easier."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-insert-note-no-questions nil
  "When non-nil, `org-noter-insert-note' won't ask for a title and will always insert a new note.
The title used will be the default one."
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-kill-frame-at-session-end t
  "If non-nil, `org-noter-kill-session' will delete the frame if others exist on the current display.'"
  :group 'org-noter
  :type 'boolean)

(defcustom org-noter-insert-heading-hook nil
  "Hook being run after inserting a new heading."
  :group 'org-noter
  :type 'hook)

(defcustom org-noter-find-additional-notes-functions nil
  "Functions that when given a document file path as argument, give out
an org note file path.

The functions in this list must accept 1 argument, a file name.
The argument will be given by `org-noter'.

The return value must be a path to an org file. No matter if it's
an absolute or relative path, the file name will be expanded to
each directory set in `org-noter-notes-search-path' to test if it exists.

If it exists, it will be listed as a candidate that `org-noter' will have
the user select to use as the note file of the document."
  :group 'org-noter
  :type 'hook)

(defface org-noter-no-notes-exist-face
  '((t
     :foreground "chocolate"
     :weight bold))
  "Face for modeline note count, when 0."
  :group 'org-noter)

(defface org-noter-notes-exist-face
  '((t
     :foreground "SpringGreen"
     :weight bold))
  "Face for modeline note count, when not 0."
  :group 'org-noter)

;; --------------------------------------------------------------------------------
;;; Integration with other packages
(defcustom org-noter--get-location-property-hook nil
  "The list of functions that will return the note location of an org element.

These functions must accept one argument, an org element.
These functions is used by `org-noter--parse-location-property' and
`org-noter--check-location-property' when they can't find the note location
of the org element given to them, that org element will be passed to
the functions in this list."
  :group 'org-noter
  :type 'hook)

(defcustom org-noter--get-containing-element-hook '(org-noter--get-containing-heading
                                                    org-noter--get-containing-property-drawer)
  "The list of functions that will be called by
`org-noter--get-containing-element' to get the org element of the note
at point."
  :group 'org-noter
  :type 'hook)

(defcustom org-noter-parse-document-property-hook nil
  "The list of functions that return a file name for the value of
the property `org-noter-property-doc-file'

This is used by `org-noter--get-or-read-document-property' and
`org-noter--doc-file-property'.

This is added for integration with other packages.

For example, the module `org-noter-citar' adds the function
`org-noter-citar-find-document-from-refs' to this list which when
the property \"NOTER_DOCUMENT\" (the default value of
`org-noter-property-doc-file') of an org file passed to it is a
citation key, it will return the path to the note file associated
with the citation key and that path will be used for other
operations instead of the real value of the property."
  :group 'org-noter
  :type 'hook)

(defcustom org-noter-get-buffer-file-name-hook '(org-noter-get-buffer-file-name-nov org-noter-get-buffer-file-name-pdf)
  "Functions that when passed a major mode, will return the current buffer file name.

This is used by the `org-noter' command to determine the file name when
user calls `org-noter' on a document buffer.

For example, `nov-mode', a renderer for EPUB documents uses a unique variable
called `nov-file-name' to store the file name of its document while the other
major modes uses the `buffer-file-name' variable."
  :group 'org-noter
  :type 'hook)

(defcustom org-noter-set-up-document-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)

(defcustom org-noter-get-selected-text-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)


(defcustom org-noter--check-location-property-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)

(defcustom org-noter--parse-location-property-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)

(defcustom org-noter--pretty-print-location-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)

(defcustom org-noter--convert-to-location-cons-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)

(defcustom org-noter--doc-goto-location-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)

(defcustom org-noter--note-after-tipping-point-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)

(defcustom org-noter--relative-position-to-view-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)

(defcustom org-noter--get-precise-info-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)


(defcustom org-noter--get-current-view-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)

(defcustom org-noter--doc-approx-location-hook nil
  "TODO"
  :group 'org-noter
  :type 'hook)

(defcustom org-noter-create-skeleton-functions nil
  "Function that inserts a tree of headlines according to the outline of the document.

The functions will be given a major mode of the document and must
return a non-nil value when the outline is created.

Used by `org-noter-create-skeleton'."
  :group 'org-noter
  :type 'hook)

(defcustom org-noter-open-document-functions nil
  "Functions that gives a buffer when passed with a document property.
Used by `org-noter--create-session' when creating a new session."
  :group 'org-noter
  :type 'hook)

;; --------------------------------------------------------------------------------
;;; Private variables or constants
(cl-defstruct org-noter--session
  id frame doc-buffer notes-buffer ast modified-tick doc-mode display-name notes-file-path property-text
  level num-notes-in-view window-behavior window-location doc-split-fraction auto-save-last-location
  hide-other closest-tipping-point)

(defvar org-noter--sessions nil
  "List of `org-noter' sessions.")

(defvar-local org-noter--session nil
  "Session associated with the current buffer.")

(defvar org-noter--inhibit-location-change-handler nil
  "Prevent location change from updating point in notes.")

(defvar org-noter--start-location-override nil
  "Used to open the session from the document in the right page.")

(defvar-local org-noter--nov-timer nil
  "Timer for synchronizing notes after scrolling.")

(defvar org-noter--arrow-location nil
  "A vector [TIMER WINDOW TOP LEFT] that shows where the arrow should appear, when idling.")

(defvar org-noter--completing-read-keymap (make-sparse-keymap)
  "A `completing-read' keymap that let's the user insert spaces.")

(set-keymap-parent org-noter--completing-read-keymap minibuffer-local-completion-map)
(define-key org-noter--completing-read-keymap (kbd "SPC") 'self-insert-command)

(defconst org-noter--property-behavior "NOTER_NOTES_BEHAVIOR"
  "Property for overriding global `org-noter-notes-window-behavior'.")

(defconst org-noter--property-location "NOTER_NOTES_LOCATION"
  "Property for overriding global `org-noter-notes-window-location'.")

(defconst org-noter--property-doc-split-fraction "NOTER_DOCUMENT_SPLIT_FRACTION"
  "Property for overriding global `org-noter-doc-split-fraction'.")

(defconst org-noter--property-auto-save-last-location "NOTER_AUTO_SAVE_LAST_LOCATION"
  "Property for overriding global `org-noter-auto-save-last-location'.")

(defconst org-noter--property-hide-other "NOTER_HIDE_OTHER"
  "Property for overriding global `org-noter-hide-other'.")

(defconst org-noter--property-closest-tipping-point "NOTER_CLOSEST_TIPPING_POINT"
  "Property for overriding global `org-noter-closest-tipping-point'.")

(defconst org-noter--note-search-element-type '(headline)
  "List of elements that should be searched for notes.")

(defconst org-noter--id-text-property 'org-noter-session-id
  "Text property used to mark the headings with open sessions.")

(defvar org-noter--url-regexp
  (concat
   "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
   "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
   "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
   (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
         (punct "!?:;.,"))
     (concat
      "\\(?:"
      ;; Match paired parentheses, e.g. in Wikipedia URLs:
      ;; http://thread.gmane.org/47B4E3B2.3050402@gmail.com
      "[" chars punct "]+" "(" "[" chars punct "]+" ")"
      "\\(?:" "[" chars punct "]+" "[" chars "]" "\\)?"
      "\\|"
      "[" chars punct "]+" "[" chars "]"
      "\\)"))
   "\\)")
  "Regular expression that matches URLs.")

;; --------------------------------------------------------------------------------
;;; Utility functions

(defun org-noter--no-heading-p ()
  "Return nil if the current buffer has atleast one heading.
Otherwise return the maximum value for point."
  (save-excursion
    (and (org-before-first-heading-p) (org-next-visible-heading 1))))

(defun org-noter--get-new-id ()
  (catch 'break
    (while t
      (let ((id (random most-positive-fixnum)))
        (unless (cl-loop for session in org-noter--sessions
                         when (= (org-noter--session-id session) id) return t)
          (throw 'break id))))))

(defmacro org-noter--property-or-default (name)
  (let ((function-name (intern (concat "org-noter--" (symbol-name name) "-property")))
        (variable      (intern (concat "org-noter-"  (symbol-name name)))))
    `(let ((prop-value (,function-name ast)))
       (cond ((eq prop-value 'disable) nil)
             (prop-value)
             (t ,variable)))))

(defun org-noter-parse-link (s)
  (pcase (with-temp-buffer
           (let ((org-inhibit-startup nil))
             (insert s)
             (org-mode)
             (goto-char (point-min))
             (org-element-link-parser)))
    (`nil nil)
    (link link)))

(defun org-noter--create-session (ast document-property-value notes-file-path)
  (let* ((raw-value-not-empty (> (length (org-element-property :raw-value ast)) 0))
         (link-p (or (string-match-p org-bracket-link-regexp document-property-value)
                     (string-match-p org-noter--url-regexp document-property-value)))
         (display-name (if raw-value-not-empty
                           (org-element-property :raw-value ast)
                         (if link-p
                             document-property-value
                           (file-name-nondirectory document-property-value))))

         (frame-name (format "Emacs Org-noter - %s" display-name))
         (document (or (run-hook-with-args-until-success 'org-noter-open-document-functions document-property-value)
                       (if link-p
                           (progn (org-link-open-from-string document-property-value)
                                  (current-buffer))
                         (find-file-noselect document-property-value))))
         (document-major-mode (if (or link-p (eq document (current-buffer)))
                                  document-property-value
                                (buffer-local-value 'major-mode document)))
         (document-buffer-name
          (generate-new-buffer-name (concat (unless raw-value-not-empty "Org-noter: ") display-name)))
         (document-buffer document)

         (notes-buffer
          (progn (when (and org-window-config-before-follow-link link-p)
                   (set-window-configuration org-window-config-before-follow-link))
                 (if org-noter-use-indirect-buffer
                     (make-indirect-buffer
                      (or (buffer-base-buffer)
                          (current-buffer))
                      (generate-new-buffer-name (concat "Notes of " display-name)) t)
                   (current-buffer))))

         (single (eq (or (buffer-base-buffer document-buffer)
                         document-buffer)
                     (or (buffer-base-buffer notes-buffer)
                         notes-buffer)))

         (session
          (make-org-noter--session
           :id (org-noter--get-new-id)
           :display-name display-name
           :frame
           (if (or org-noter-always-create-frame
                   (catch 'has-session
                     (dolist (test-session org-noter--sessions)
                       (when (eq (org-noter--session-frame test-session) (selected-frame))
                         (throw 'has-session t)))))
               (make-frame `((name . ,frame-name) (fullscreen . maximized)))
             (set-frame-parameter nil 'name frame-name)
             (selected-frame))
           :doc-mode document-major-mode
           :property-text document-property-value
           :notes-file-path notes-file-path
           :doc-buffer document-buffer
           :notes-buffer notes-buffer
           :level (or (org-element-property :level ast) 0)
           :window-behavior (org-noter--property-or-default notes-window-behavior)
           :window-location (org-noter--property-or-default notes-window-location)
           :doc-split-fraction (org-noter--property-or-default doc-split-fraction)
           :auto-save-last-location (org-noter--property-or-default auto-save-last-location)
           :hide-other (org-noter--property-or-default hide-other)
           :closest-tipping-point (org-noter--property-or-default closest-tipping-point)
           :modified-tick -1))

         (target-location org-noter--start-location-override)
         (starting-point (point)))

    (add-hook 'delete-frame-functions 'org-noter--handle-delete-frame)
    (push session org-noter--sessions)

    (with-current-buffer document-buffer
      (or (run-hook-with-args-until-success 'org-noter-set-up-document-hook document-major-mode)
          (run-hook-with-args-until-success 'org-noter-set-up-document-hook document-property-value)
          (error "This document handler is not supported :/"))

      (org-noter-doc-mode 1)
      (setq org-noter--session session)
      (add-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer nil t))

    (with-current-buffer notes-buffer
      (org-noter-notes-mode 1)
      ;; NOTE(nox): This is needed because a session created in an indirect buffer would use the point of
      ;; the base buffer (as this buffer is indirect to the base!)
      (goto-char starting-point)
      (setq buffer-file-name notes-file-path
            org-noter--session session
            fringe-indicator-alist '((truncation . nil)))
      (add-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer nil t)
      (add-hook 'window-scroll-functions 'org-noter--set-notes-scroll nil t)
      (org-noter--set-text-properties (org-noter--parse-root (vector notes-buffer document-property-value))
                                      (org-noter--session-id session))
      (unless target-location
        (setq target-location (org-noter--parse-location-property (org-noter--get-containing-element t)))))

    ;; NOTE(nox): This timer is for preventing reflowing too soon.
    (unless single
      (run-with-idle-timer
       0.05 nil
       (lambda ()
         ;; NOTE(ahmed-shariff): setup-window run here to avoid crash when notes buffer not setup in time
         (org-noter--setup-windows session)
         (with-current-buffer document-buffer
           (let ((org-noter--inhibit-location-change-handler t))
             (when target-location (org-noter--doc-goto-location target-location)))
           (org-noter--doc-location-change-handler)))))))

(defun org-noter--valid-session (session)
  (when session
    (if (and (frame-live-p (org-noter--session-frame session))
             (buffer-live-p (org-noter--session-doc-buffer session))
             (buffer-live-p (org-noter--session-notes-buffer session)))
        t
      (org-noter-kill-session session)
      nil)))

(defmacro org-noter--with-valid-session (&rest body)
  (declare (debug (body)))
  `(let ((session org-noter--session))
     (when (org-noter--valid-session session)
       (progn ,@body))))

(defun org-noter--handle-kill-buffer ()
  (org-noter--with-valid-session
   (let ((buffer (current-buffer))
         (notes-buffer (org-noter--session-notes-buffer session))
         (doc-buffer (org-noter--session-doc-buffer session)))
     ;; NOTE(nox): This needs to be checked in order to prevent session killing because of
     ;; temporary buffers with the same local variables
     (when (or (eq buffer notes-buffer)
               (eq buffer doc-buffer))
       (org-noter-kill-session session)))))

(defun org-noter--handle-delete-frame (frame)
  (dolist (session org-noter--sessions)
    (when (eq (org-noter--session-frame session) frame)
      (org-noter-kill-session session))))

(defun org-noter--parse-root (&optional info)
  "Parse and return the root AST.
When used, the INFO argument may be an org-noter session or a vector [NotesBuffer PropertyText].
If nil, the session used will be `org-noter--session'."
  (let* ((arg-is-session (org-noter--session-p info))
         (session (or (and arg-is-session info) org-noter--session))
         root-pos ast)
    (cond
     ((and (not arg-is-session) (vectorp info))
      ;; NOTE(nox): Use arguments to find heading, by trying to find the outermost parent heading with
	  ;; the specified property
      (let ((notes-buffer (aref info 0))
            (wanted-prop  (aref info 1)))
        (unless (and (buffer-live-p notes-buffer) (or (stringp wanted-prop)
                                                      (eq 'link (org-element-type wanted-prop)))
                     (eq (buffer-local-value 'major-mode notes-buffer) 'org-mode))
          (error "Error parsing root with invalid arguments"))

        (with-current-buffer notes-buffer
          (org-with-wide-buffer
           (catch 'break
	     (while t
               (let ((document-property (or (org-entry-get nil org-noter-property-doc-file t)
                                            (cadar (org-collect-keywords (list org-noter-property-doc-file))))))
                 (when (string= (or (run-hook-with-args-until-success 'org-noter-parse-document-property-hook document-property)
                                    document-property)
                                wanted-prop)
                   (setq root-pos (copy-marker (if (and org-noter-prefer-root-as-file-level
                                                        (save-excursion
                                                          (goto-char (point-min))
                                                          (eq 'property-drawer (org-element-type (org-element-at-point)))))
                                                   (point-min)
                                                 (point))))))
               (unless (org-up-heading-safe) (throw 'break t))))))))

     ((org-noter--valid-session session)
      ;; NOTE(nox): Use session to find heading
      (or (and (= (buffer-chars-modified-tick (org-noter--session-notes-buffer session))
                  (org-noter--session-modified-tick session))
               (setq ast (org-noter--session-ast session))) ; NOTE(nox): Cached version!

          ;; NOTE(nox): Find session id text property
          (with-current-buffer (org-noter--session-notes-buffer session)
            (org-with-wide-buffer
             (let ((pos (text-property-any (point-min) (point-max) org-noter--id-text-property
                                           (org-noter--session-id session))))
               (when pos (setq root-pos (copy-marker pos)))))))))

    (unless ast
      (unless root-pos (if (or org-noter-prefer-root-as-file-level (org-noter--no-heading-p))
                           (setq root-pos (copy-marker (point-min)))
                         (org-next-visible-heading 1)
                         (setq root-pos (copy-marker (point)))))
      (with-current-buffer (marker-buffer root-pos)
        (org-with-point-at (marker-position root-pos)
          (org-back-to-heading-or-point-min t)
          (if (org-at-heading-p)
              (org-narrow-to-subtree)
            (org-hide-drawer-toggle 'force))
          (setq ast (car (org-element-contents (org-element-parse-buffer 'greater-element))))
          (when (and (not (vectorp info)) (org-noter--valid-session session))
            (setf (org-noter--session-ast session) ast
                  (org-noter--session-modified-tick session) (buffer-chars-modified-tick))))))
    ast))

(defun org-noter--get-properties-end (ast &optional force-trim)
  (when ast
    (let* ((contents (org-element-contents ast))
           (section (org-element-map contents 'section 'identity nil t 'headline))
           (properties (or (org-element-map section 'property-drawer 'identity nil t)
                           (org-element-map contents 'property-drawer 'identity nil t)))
           properties-end)
      (if (not properties)
          (org-element-property :contents-begin ast)
        (setq properties-end (org-element-property :end properties))
        (when (or force-trim
                  (= (org-element-property :end section) properties-end))
          (while (not (eq (char-before properties-end) ?:))
            (setq properties-end (1- properties-end))))
        properties-end))))

(defun org-noter--set-text-properties (ast id)
  (org-with-wide-buffer
   (when ast
     (let* ((level (or (org-element-property :level ast) 0))
            (begin (org-element-property :begin ast))
            (title-begin (+ 1 level begin))
            (contents-begin (org-element-property :contents-begin ast))
            (properties-end (org-noter--get-properties-end ast t))
            (inhibit-read-only t)
            (modified (buffer-modified-p)))
       (if (= level 0)
           (when properties-end
             (add-text-properties contents-begin properties-end
                                  `(read-only t rear-nonsticky t ,org-noter--id-text-property ,id))
             (set-buffer-modified-p modified))
         (add-text-properties (max 1 (1- begin)) begin '(read-only t))
         (add-text-properties begin (1- title-begin) `(read-only t front-sticky t ,org-noter--id-text-property ,id))
         (add-text-properties (1- title-begin) title-begin '(read-only t rear-nonsticky t))
         ;; (add-text-properties (1- contents-begin) (1- properties-end) '(read-only t))
         (when properties-end
           (add-text-properties (1- properties-end) properties-end
                                '(read-only t rear-nonsticky t)))
         (set-buffer-modified-p modified))))))

(defun org-noter--unset-text-properties (ast)
  (when ast
    (org-with-wide-buffer
     (let* ((begin (org-element-property :begin ast))
            (end (org-noter--get-properties-end ast t))
            (inhibit-read-only t)
            (modified (buffer-modified-p)))
       (when end
         (remove-list-of-text-properties (max 1 (1- begin)) end
                                         `(read-only front-sticky rear-nonsticky ,org-noter--id-text-property))

         (set-buffer-modified-p modified))))))

(defun org-noter--set-notes-scroll (window &rest ignored)
  (when window
    (with-selected-window window
      (org-noter--with-valid-session
       (let* ((level (org-noter--session-level session))
              (goal (* (1- level) 2))
              (current-scroll (window-hscroll)))
         (when (and (bound-and-true-p org-indent-mode) (< current-scroll goal))
           (scroll-right current-scroll)
           (scroll-left goal t)))))))

(defun org-noter--insert-heading (level title &optional newlines-number location)
  "Insert a new heading at LEVEL with TITLE.
The point will be at the start of the contents, after any
properties, by a margin of NEWLINES-NUMBER."
  (setq newlines-number (or newlines-number 1))
  (org-insert-heading nil t)
  (let* ((initial-level (org-element-property :level (org-element-at-point)))
         (changer (if (> level initial-level) 'org-do-demote 'org-do-promote))
         (number-of-times (abs (- level initial-level))))
    (dotimes (_ number-of-times) (funcall changer))
    (insert (org-trim (replace-regexp-in-string "\n" " " title)))

    (org-end-of-subtree)
    (unless (bolp) (insert "\n"))
    (org-N-empty-lines-before-current (1- newlines-number))

    (when location
      (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location))

      (when org-noter-doc-property-in-notes
        (org-noter--with-valid-session
         (org-entry-put nil org-noter-property-doc-file (org-noter--session-property-text session))
         (org-entry-put nil org-noter--property-auto-save-last-location "nil"))))

    (run-hooks 'org-noter-insert-heading-hook)))

(defun org-noter--narrow-to-root (ast)
  (when ast
    (save-excursion
      (goto-char (org-element-property :contents-begin ast))
      (org-show-entry)
      (when (org-at-heading-p) (org-narrow-to-subtree))
      (org-cycle-hide-drawers 'all))))

(defun org-noter--get-doc-window ()
  (org-noter--with-valid-session
   (or (get-buffer-window (org-noter--session-doc-buffer session)
                          (org-noter--session-frame session))
       (org-noter--setup-windows org-noter--session)
       (get-buffer-window (org-noter--session-doc-buffer session)
                          (org-noter--session-frame session)))))

(defun org-noter--get-notes-window (&optional type)
  (org-noter--with-valid-session
   (let ((notes-buffer (org-noter--session-notes-buffer session))
         (window-location (org-noter--session-window-location session))
         (window-behavior (org-noter--session-window-behavior session))
         notes-window)
     (or (get-buffer-window notes-buffer t)
         (when (or (eq type 'force) (memq type window-behavior))
           (if (eq window-location 'other-frame)
               (let ((restore-frame (selected-frame)))
                 (switch-to-buffer-other-frame notes-buffer)
                 (setq notes-window (get-buffer-window notes-buffer t))
                 (x-focus-frame restore-frame)
                 (raise-frame (window-frame notes-window)))

             (with-selected-window (org-noter--get-doc-window)
               (let ((horizontal (eq window-location 'horizontal-split)))
                 (setq
                  notes-window
                  (if (window-combined-p nil horizontal)
                      ;; NOTE(nox): Reuse already existent window
                      (let ((sibling-window (or (window-next-sibling) (window-prev-sibling))))
                        (or (window-top-child sibling-window) (window-left-child sibling-window)
                            sibling-window))

                    (if horizontal
                        (split-window-right (ceiling (* (car (org-noter--session-doc-split-fraction session))
                                                        (window-total-width))))
                      (split-window-below (ceiling (* (cadr (org-noter--session-doc-split-fraction session))
                                                      (window-total-height)))))))))

             (set-window-buffer notes-window notes-buffer))
           notes-window)))))

(defun org-noter--setup-windows (session)
  "Setup windows when starting session, respecting user configuration."
  (when (org-noter--valid-session session)
    (with-selected-frame (org-noter--session-frame session)
      (delete-other-windows)
      (let* ((doc-buffer (org-noter--session-doc-buffer session))
             (doc-window (selected-window))
             (notes-buffer (org-noter--session-notes-buffer session))
             (window-location (org-noter--session-window-location session))
             notes-window)

        (set-window-buffer doc-window doc-buffer)

        (with-current-buffer notes-buffer
          (unless org-noter-disable-narrowing
            (org-noter--narrow-to-root (org-noter--parse-root session)))
          (setq notes-window (org-noter--get-notes-window 'start))
          (org-noter--set-notes-scroll notes-window))

        (when org-noter-swap-window
          (cl-labels ((swap-windows (window1 window2)
                                    "Swap the buffers of WINDOW1 and WINDOW2."
                                    (let ((buffer1 (window-buffer window1))
                                          (buffer2 (window-buffer window2)))
                                      (set-window-buffer window1 buffer2)
                                      (set-window-buffer window2 buffer1)
                                      (select-window window2))))
            (let ((frame (window-frame notes-window)))
              (when (and (frame-live-p frame)
                         (not (eq frame (selected-frame))))
                (select-frame-set-input-focus (window-frame notes-window)))
              (when (and (window-live-p notes-window)
                         (not (eq notes-window doc-window)))
                (swap-windows notes-window doc-window))))

          (if (eq window-location 'horizontal-split)
              (enlarge-window (- (ceiling (* (- 1 (car (org-noter--session-doc-split-fraction session)))
                                             (frame-width)))
                                 (window-total-width)) t)
            (enlarge-window (- (ceiling (* (- 1 (cadr (org-noter--session-doc-split-fraction session)))
                                           (frame-height)))
                               (window-total-height)))))

        (if org-noter-swap-window
            ;; the variable NOTES-WINDOW here is really
            ;; the document window since the two got swapped
            (set-window-dedicated-p notes-window t)
          ;; It's not swapped so set it normally
          (set-window-dedicated-p doc-window t))))))

(defmacro org-noter--with-selected-notes-window (error-str &rest body)
  (declare (debug ([&optional stringp] body)))
  (let ((with-error (stringp error-str)))
    `(org-noter--with-valid-session
      (let ((notes-window (org-noter--get-notes-window)))
        (if notes-window
            (with-selected-window notes-window
              ,(if with-error
                   `(progn ,@body)
                 (if body
                     `(progn ,error-str ,@body)
                   `(progn ,error-str))))
          ,(when with-error `(user-error "%s" ,error-str)))))))

(defun org-noter--notes-window-behavior-property (ast)
  (let ((property (org-element-property (intern (concat ":" org-noter--property-behavior)) ast))
        value)
    (when (and (stringp property) (> (length property) 0))
      (setq value (car (read-from-string property)))
      (when (listp value) value))))

(defun org-noter--notes-window-location-property (ast)
  (let ((property (org-element-property (intern (concat ":" org-noter--property-location)) ast))
        value)
    (when (and (stringp property) (> (length property) 0))
      (setq value (intern property))
      (when (memq value '(horizontal-split vertical-split other-frame)) value))))

(defun org-noter--doc-split-fraction-property (ast)
  (let ((property (org-element-property (intern (concat ":" org-noter--property-doc-split-fraction)) ast))
        value)
    (when (and (stringp property) (> (length property) 0))
      (setq value (car (read-from-string property)))
      (when (consp value) value))))

(defun org-noter--auto-save-last-location-property (ast)
  (let ((property (org-element-property (intern (concat ":" org-noter--property-auto-save-last-location)) ast)))
    (when (and (stringp property) (> (length property) 0))
      (if (intern property) t 'disable))))

(defun org-noter--hide-other-property (ast)
  (let ((property (org-element-property (intern (concat ":" org-noter--property-hide-other)) ast)))
    (when (and (stringp property) (> (length property) 0))
      (if (intern property) t 'disable))))

(defun org-noter--closest-tipping-point-property (ast)
  (let ((property (org-element-property (intern (concat ":" org-noter--property-closest-tipping-point)) ast)))
    (when (and (stringp property) (> (length property) 0))
      (ignore-errors (string-to-number property)))))

(defun org-noter--doc-approx-location (&optional precise-info force-new-ref)
  "TODO"
  (let ((window (if (org-noter--valid-session org-noter--session)
                    (org-noter--get-doc-window)
                  (selected-window))))
    (cl-assert window)
    (with-selected-window window
      (or (run-hook-with-args-until-success
           'org-noter--doc-approx-location-hook major-mode precise-info force-new-ref)
          (error "Unknown document type %s" major-mode)))))

(defun org-noter--location-change-advice (&rest _)
  (org-noter--with-valid-session (org-noter--doc-location-change-handler)))

(defun org-noter--nov-scroll-handler (&rest _)
  (when org-noter--nov-timer (cancel-timer org-noter--nov-timer))
  (unless org-noter--inhibit-location-change-handler
    (setq org-noter--nov-timer (run-with-timer 0.25 nil 'org-noter--doc-location-change-handler))))

(defsubst org-noter--doc-file-property (headline)
  (let ((doc-prop (or (org-element-property (intern (concat ":" org-noter-property-doc-file)) headline)
                      (cadar (org-collect-keywords (list org-noter-property-doc-file)))
                      (org-entry-get nil org-noter-property-doc-file t))))
    (or (run-hook-with-args-until-success 'org-noter-parse-document-property-hook doc-prop)
        doc-prop)))

(defun org-noter--check-location-property (arg)
  (let ((property (if (stringp arg) arg
                    (or (org-element-property
                         (intern (concat ":" org-noter-property-note-location)) arg)
                        (run-hook-with-args-until-success
                         'org-noter--get-location-property-hook arg)))))
    (when (and (stringp property) (> (length property) 0))
      (or (run-hook-with-args-until-success 'org-noter--check-location-property-hook property)
          (let ((value (car (read-from-string property))))
            (or (and (consp value) (integerp (car value)) (numberp (cdr value)))
                (and (consp value) (integerp (car value)) (integerp (cadr value)) (integerp (cddr value)))
                (integerp value)))))))

(defun org-noter--parse-location-property (arg)
  (let ((property (if (stringp arg) arg
                    (or (org-element-property
                         (intern (concat ":" org-noter-property-note-location)) arg)
                        (run-hook-with-args-until-success
                         'org-noter--get-location-property-hook arg)))))
    (when (and (stringp property) (> (length property) 0))
      (or (run-hook-with-args-until-success 'org-noter--parse-location-property-hook property)
          (let ((value (car (read-from-string property))))
            (cond ((and (consp value) (integerp (car value)) (numberp (cdr value))) value)
		  ((and (consp value) (integerp (car value)) (consp (cdr value)) (numberp (cadr value)) (numberp (cddr value))) value)
                  ((integerp value) (cons value 0))))))))

(defun org-noter--pretty-print-location (location)
  (org-noter--with-valid-session
   (run-hook-with-args-until-success
    'org-noter--pretty-print-location-hook location)))

;; TODO: Documentation
(defun org-noter--get-containing-element (&optional include-root)
  (run-hook-with-args-until-success 'org-noter--get-containing-element-hook include-root))

(defun org-noter--get-containing-heading (&optional include-root)
  "Get smallest containing heading that encloses the point and has location property.
If the point isn't inside any heading with location property, return the outer heading.
When INCLUDE-ROOT is non-nil, the root heading is also eligible to be returned."
  (org-noter--with-valid-session
   (org-with-wide-buffer
    (unless (org-before-first-heading-p)
      (org-back-to-heading t)
      (let (previous)
        (catch 'break
          (while t
            (let ((prop (org-noter--check-location-property (org-entry-get nil org-noter-property-note-location)))
                  (at-root (equal (org-noter--session-id session)
                                  (get-text-property (point) org-noter--id-text-property)))
                  (heading (org-element-at-point)))
              (when (and prop (or include-root (not at-root)))
                (throw 'break heading))

              (when (or at-root (not (org-up-heading-safe)))
                (throw 'break (if include-root heading previous)))

              (setq previous heading)))))))))

(defun org-noter--get-containing-property-drawer (&optional include-root)
  "Get smallest containing heading that encloses the point and has location property.
If the point isn't inside any heading with location property, return the outer heading.
When INCLUDE-ROOT is non-nil, the root heading is also eligible to be returned."
  (org-noter--with-valid-session
   (org-with-point-at (point-min)
    (when (org-before-first-heading-p)
      (let ((prop (org-entry-get nil org-noter-property-note-location))
            (at-root (equal (org-noter--session-id session)
                            (get-text-property (point) org-noter--id-text-property))))
        (when (and (org-noter--check-location-property prop) (or include-root (not at-root)))
          prop))))))

(defun org-noter--doc-get-page-slice ()
  "Return (slice-top . slice-height)."
  (let* ((slice (or (image-mode-window-get 'slice) '(0 0 1 1)))
	 (slice-left (float (nth 0 slice)))
         (slice-top (float (nth 1 slice)))
	 (slice-width (float (nth 2 slice)))
         (slice-height (float (nth 3 slice))))
    (when (or (> slice-top 1)
              (> slice-height 1))
      (let ((height (cdr (image-size (image-mode-window-get 'image) t))))
        (setq slice-top (/ slice-top height)
              slice-height (/ slice-height height))))
    (when (or (> slice-width 1)
              (> slice-left 1))
      (let ((width (car (image-size (image-mode-window-get 'image) t))))
        (setq slice-width (/ slice-width height)
              slice-left (/ slice-left height))))
    (list slice-top slice-height slice-left slice-width)))

(defun org-noter--conv-page-scroll-percentage (vscroll &optional hscroll)
  (let* ((slice (org-noter--doc-get-page-slice))
         (display-size (image-display-size (image-get-display-property)))
         (display-percentage-height (/ vscroll (cdr display-size)))
         (hpercentage (max 0 (min 1 (+ (nth 0 slice) (* (nth 1 slice) display-percentage-height))))))
    (if hscroll
	(cons hpercentage (max 0 (min 1 (+ (nth 2 slice) (* (nth 3 slice) (/ vscroll (car display-size)))))))
      (cons hpercentage 0))))

(defun org-noter--conv-page-percentage-scroll (percentage)
  (let* ((slice (org-noter--doc-get-page-slice))
         (display-height (cdr (image-display-size (image-get-display-property))))
         (display-percentage (min 1 (max 0 (/ (- percentage (nth 0 slice)) (nth 1 slice)))))
         (scroll (max 0 (floor (* display-percentage display-height)))))
    scroll))

(defun org-noter--get-precise-info ()
  (org-noter--with-valid-session
   (let ((window (org-noter--get-doc-window))
         (mode (org-noter--session-doc-mode session)))
     (with-selected-window window
       (run-hook-with-args-until-success 'org-noter--get-precise-info-hook mode)))))

(defun org-noter--show-arrow ()
  (when (and org-noter--arrow-location
             (window-live-p (aref org-noter--arrow-location 1)))
    (with-selected-window (aref org-noter--arrow-location 1)
      ;; From `pdf-util-tooltip-arrow'.
      (pdf-util-assert-pdf-window)
      (let* (x-gtk-use-system-tooltips
             (image-top (if (floatp (aref org-noter--arrow-location 2))
                            (round (* (aref org-noter--arrow-location 2)
                                      (cdr (pdf-view-image-size))))))
             (image-left (if (floatp (aref org-noter--arrow-location 3))
                             (round (* (aref org-noter--arrow-location 3) (car (pdf-view-image-size))))))

	     (dx (or image-left
                     (+ (or (car (window-margins)) 0)
                        (car (window-fringes)))))
             (dy (or image-top 0))
             (pos (list dx dy dx (+ dy (* 2 (frame-char-height)))))
             (vscroll (pdf-util-required-vscroll pos))
             (tooltip-frame-parameters
              `((border-width . 0)
                (internal-border-width . 0)
                ,@tooltip-frame-parameters))
             (tooltip-hide-delay 3))

        (when vscroll
          (image-set-window-vscroll vscroll))
        (setq dy (max 0 (- dy
                           (cdr (pdf-view-image-offset))
                           (window-vscroll nil t)
                           (frame-char-height))))
        (when (overlay-get (pdf-view-current-overlay) 'before-string)
          (let* ((e (window-inside-pixel-edges))
                 (xw (pdf-util-with-edges (e) e-width)))
            (cl-incf dx (/ (- xw (car (pdf-view-image-size t))) 2))))
        (pdf-util-tooltip-in-window
         (propertize
          " " 'display (propertize
                        "\u2192" ;; right arrow
                        'display '(height 2)
                        'face `(:foreground
                                "orange red"
                                :background
                                ,(if (bound-and-true-p pdf-view-midnight-minor-mode)
                                     (cdr pdf-view-midnight-colors)
                                   "white"))))
         dx dy))
      (setq org-noter--arrow-location nil))))

(defun org-noter--get-location-top (location)
  "Get the top coordinate given a LOCATION of form (page top . left) or (page . top)."
  (if (listp (cdr location))
      (cadr location)
    (cdr location)))

(defun org-noter--get-location-page (location)
  "Get the page number given a LOCATION of form (page top . left) or (page . top)."
  (car location))

(defun org-noter--get-location-left (location)
  "Get the left coordinate given a LOCATION of form (page top . left) or (page . top). If later form of vector is passed return 0."
  (if (listp (cdr location))
      (if (listp (cddr location))
          (caddr location)
        (cddr location))
    0))

(defun org-noter--doc-goto-location (location)
  "Go to location specified by LOCATION."
  (org-noter--with-valid-session
   (let ((window (org-noter--get-doc-window))
         (mode (org-noter--session-doc-mode session)))
     (with-selected-window window
       (run-hook-with-args-until-success 'org-noter--doc-goto-location-hook mode location)
       (redisplay)))))

(defun org-noter--compare-location-cons (comp l1 l2)
  "Compare L1 and L2, which are location cons.
See `org-noter--compare-locations'"
  (cl-assert (and (consp l1) (consp l2)))
  (cond ((eq comp '=)
         (and (= (org-noter--get-location-page l1) (org-noter--get-location-page l2))
              (= (org-noter--get-location-top l1) (org-noter--get-location-top l2))
              (= (org-noter--get-location-left l1) (org-noter--get-location-left l2))))
        ((eq comp '<)
         (or (< (org-noter--get-location-page l1) (org-noter--get-location-page l2))
             (and (= (org-noter--get-location-page l1) (org-noter--get-location-page l2))
                  (< (org-noter--get-location-top l1) (org-noter--get-location-top l2)))
             (and (= (org-noter--get-location-page l1) (org-noter--get-location-page l2))
                  (= (org-noter--get-location-top l1) (org-noter--get-location-top l2))
                  (< (org-noter--get-location-left l1) (org-noter--get-location-left l2)))))
        ((eq comp '<=)
         (or (< (org-noter--get-location-page l1) (org-noter--get-location-page l2))
             (and (=  (org-noter--get-location-page l1) (org-noter--get-location-page l2))
                  (<= (org-noter--get-location-top l1) (org-noter--get-location-top l2)))
             (and (= (org-noter--get-location-page l1) (org-noter--get-location-page l2))
                  (= (org-noter--get-location-top l1) (org-noter--get-location-top l2))
                  (<= (org-noter--get-location-left l1) (org-noter--get-location-left l2)))))
        ((eq comp '>)
         (or (> (org-noter--get-location-page l1) (org-noter--get-location-page l2))
             (and (= (org-noter--get-location-page l1) (org-noter--get-location-page l2))
                  (> (org-noter--get-location-top l1) (org-noter--get-location-top l2)))
             (and (= (org-noter--get-location-page l1) (org-noter--get-location-page l2))
                  (= (org-noter--get-location-top l1) (org-noter--get-location-top l2))
                  (> (org-noter--get-location-left l1) (org-noter--get-location-left l2)))))
        ((eq comp '>=)
         (or (> (org-noter--get-location-page l1) (org-noter--get-location-page l2))
             (and (= (org-noter--get-location-page l1) (org-noter--get-location-page l2))
                  (>= (org-noter--get-location-top l1) (org-noter--get-location-top l2)))
             (and (= (org-noter--get-location-page l1) (org-noter--get-location-page l2))
                  (= (org-noter--get-location-top l1) (org-noter--get-location-top l2))
                  (>= (org-noter--get-location-left l1) (org-noter--get-location-left l2)))))
        ((eq comp '>f)
         (or (> (org-noter--get-location-page l1) (org-noter--get-location-page l2))
             (and (= (org-noter--get-location-page l1) (org-noter--get-location-page l2))
                  (< (org-noter--get-location-top l1) (org-noter--get-location-top l2)))
             (and (= (org-noter--get-location-page l1) (org-noter--get-location-page l2))
                  (= (org-noter--get-location-top l1) (org-noter--get-location-top l2))
                  (< (org-noter--get-location-left l1) (org-noter--get-location-left l2)))))
        (t (error "Comparison operator %s not known" comp))))

(defun org-noter--compare-locations (comp l1 l2)
  "Compare L1 and L2.
When COMP is '<, '<=, '>, or '>=, it works as expected.
When COMP is '>f, it will return t when L1 is a page greater than
L2 or, when in the same page, if L1 is the _f_irst of the two."
  (cond ((not l1) nil)
        ((not l2) t)
        (t
         (setq l1 (or (run-hook-with-args-until-success 'org-noter--convert-to-location-cons-hook l1) l1)
               l2 (or (run-hook-with-args-until-success 'org-noter--convert-to-location-cons-hook l2) l2))
	 (if (numberp (cdr l2))
             (org-noter--compare-location-cons comp l1 l2)
	   (org-noter--compare-location-cons comp l1 (cons (car l2) (cadr l2)))))))

(defun org-noter--show-note-entry (session note)
  "This will show the note entry and its children.
Every direct subheading _until_ the first heading that doesn't
belong to the same view (ie. until a heading with location or
document property) will be opened."
  (save-excursion
    (goto-char (org-element-property :contents-begin note))
    (org-show-set-visibility t)
    (org-element-map (org-element-contents note) 'headline
      (lambda (headline)
        (let ((doc-file (org-noter--doc-file-property headline)))
          (if (or (and doc-file (not (string= doc-file (org-noter--session-property-text session))))
                  (org-noter--check-location-property headline))
              t
            (goto-char (org-element-property :begin headline))
            (org-show-entry)
            (org-show-children)
            nil)))
      nil t org-element-all-elements)))

(defun org-noter--focus-notes-region (view-info)
  (org-noter--with-selected-notes-window
   (if (org-noter--session-hide-other session)
       (save-excursion
         (goto-char (org-element-property :begin (org-noter--parse-root)))
         (unless (org-before-first-heading-p)
           (outline-hide-subtree)))
     (org-cycle-hide-drawers 'all))

   (let* ((notes-cons (org-noter--view-info-notes view-info))
          (regions (or (org-noter--view-info-regions view-info)
                       (org-noter--view-info-prev-regions view-info)))
          (point-before (point))
          target-region
          point-inside-target-region)
     (cond
      (notes-cons
       (dolist (note-cons notes-cons) (org-noter--show-note-entry session (car note-cons)))

       (setq target-region (or (catch 'result (dolist (region regions)
                                                (when (and (>= point-before (car region))
                                                           (or (save-restriction (goto-char (cdr region)) (eobp))
                                                               (< point-before (cdr region))))
                                                  (setq point-inside-target-region t)
                                                  (throw 'result region))))
                               (car regions)))

       (let ((begin (car target-region)) (end (cdr target-region)) num-lines
             (target-char (if point-inside-target-region
                              point-before
                            (org-noter--get-properties-end (caar notes-cons))))
             (window-start (window-start)) (window-end (window-end nil t)))
         (setq num-lines (count-screen-lines begin end))

         (cond
          ((> num-lines (window-height))
           (goto-char begin)
           (recenter 0))

          ((< begin window-start)
           (goto-char begin)
           (recenter 0))

          ((> end window-end)
           (goto-char end)
           (recenter -2)))

         (goto-char target-char)))

      (t (org-noter--show-note-entry session (org-noter--parse-root)))))

   (org-cycle-show-empty-lines t)))

(defun org-noter--get-current-view ()
  "Return a vector with the current view information."
  (org-noter--with-valid-session
   (let ((mode (org-noter--session-doc-mode session)))
     (with-selected-window (org-noter--get-doc-window)
       (or (run-hook-with-args-until-success 'org-noter--get-current-view-hook mode)
           (error "Unknown document type"))))))

(defun org-noter--note-after-tipping-point (point location view)
  ;; NOTE(nox): This __assumes__ the note is inside the view!
  (let (hook-result)
    (cond
     ((setq hook-result (run-hook-with-args-until-success 'org-noter--note-after-tipping-point-hook
                                                          point location view))
      (cdr hook-result))
     ((eq (aref view 0) 'paged)
      (> (org-noter--get-location-top location) point))
     ((eq (aref view 0) 'nov)
      (> (org-noter--get-location-top location) (+ (* point (- (cdr (aref view 2)) (cdr (aref view 1))))
                                                   (cdr (aref view 1))))))))

(defun org-noter--relative-position-to-view (location view)
  (cond
   ((run-hook-with-args-until-success 'org-noter--relative-position-to-view-hook location view))

   ((eq (aref view 0) 'paged)
    (let ((note-page (org-noter--get-location-page location))
          (view-page (aref view 1)))
      (cond ((< note-page view-page) 'before)
            ((= note-page view-page) 'inside)
            (t                       'after))))

   ((eq (aref view 0) 'nov)
    (let ((view-top (aref view 1))
          (view-bot (aref view 2)))
      (cond ((org-noter--compare-locations '<  location view-top) 'before)
            ((org-noter--compare-locations '<= location view-bot) 'inside)
            (t                                                    'after))))))

(defmacro org-noter--view-region-finish (info &optional terminating-headline)
  `(when ,info
     ,(if terminating-headline
          `(push (cons (aref ,info 1) (min (aref ,info 2) (org-element-property :begin ,terminating-headline)))
                 (gv-deref (aref ,info 0)))
        `(push (cons (aref ,info 1) (aref ,info 2)) (gv-deref (aref ,info 0))))
     (setq ,info nil)))

(defmacro org-noter--view-region-add (info list-name headline)
  `(progn
     (when (and ,info (not (eq (aref ,info 3) ',list-name)))
       (org-noter--view-region-finish ,info ,headline))

     (if ,info
         (setf (aref ,info 2) (max (aref ,info 2) (org-element-property :end ,headline)))
       (setq ,info (vector (gv-ref ,list-name)
                           (org-element-property :begin ,headline) (org-element-property :end ,headline)
                           ',list-name)))))

;; NOTE(nox): notes is a list of (HEADING . HEADING-TO-INSERT-TEXT-BEFORE):
;; - HEADING is the root heading of the note
;; - SHOULD-ADD-SPACE indicates if there should be extra spacing when inserting text to the note (ie. the
;;   note has contents)
(cl-defstruct org-noter--view-info notes regions prev-regions reference-for-insertion)

(defun org-noter--get-view-info (view &optional new-location)
  "Return VIEW related information.

When optional NEW-LOCATION is provided, it will be used to find
the best heading to serve as a reference to create the new one
relative to."
  (when view
    (org-noter--with-valid-session
     (let ((contents (if (= 0 (org-noter--session-level session))
                         (org-element-contents
                          (org-element-property :parent (org-noter--parse-root)))
                       (org-element-contents (org-noter--parse-root))))
           (preamble t)
           notes-in-view regions-in-view
           reference-for-insertion reference-location
           (all-after-tipping-point t)
           (closest-tipping-point (and (>= (org-noter--session-closest-tipping-point session) 0)
                                       (org-noter--session-closest-tipping-point session)))
           closest-notes closest-notes-regions closest-notes-location
           ignore-until-level
           current-region-info) ;; NOTE(nox): [REGIONS-LIST-PTR START MAX-END REGIONS-LIST-NAME]

       (with-current-buffer (or (buffer-base-buffer (org-noter--session-notes-buffer session))
                                (org-noter--session-notes-buffer session))
         (org-element-map contents org-noter--note-search-element-type
           (lambda (element)
             (let ((doc-file (org-noter--doc-file-property element))
                   (location (org-noter--parse-location-property element)))
               (when (and ignore-until-level (<= (org-element-property :level element) ignore-until-level))
                 (setq ignore-until-level nil))

               (cond
                (ignore-until-level) ;; NOTE(nox): This heading is ignored, do nothing

                ((and doc-file (not (string= doc-file (org-noter--session-property-text session))))
                 (org-noter--view-region-finish current-region-info element)
                 (setq ignore-until-level (org-element-property :level element))
                 (when (and preamble new-location
                            (or (not reference-for-insertion)
                                (>= (org-element-property :begin element)
                                    (org-element-property :end (cdr reference-for-insertion)))))
                   (setq reference-for-insertion (cons 'after element))))

                (location
                 (let ((relative-position (org-noter--relative-position-to-view location view)))
                   (cond
                    ((eq relative-position 'inside)
                     (push (cons element nil) notes-in-view)

                     (org-noter--view-region-add current-region-info regions-in-view element)

                     (setq all-after-tipping-point
                           (and all-after-tipping-point (org-noter--note-after-tipping-point
                                                         closest-tipping-point location view))))

                    (t
                     (when current-region-info
                       (let ((note-cons-to-change (cond ((eq (aref current-region-info 3) 'regions-in-view)
                                                         (car notes-in-view))
                                                        ((eq (aref current-region-info 3) 'closest-notes-regions)
                                                         (car closest-notes)))))
                         (when (< (org-element-property :begin element)
                                  (org-element-property :end (car note-cons-to-change)))
                           (setcdr note-cons-to-change element))))

                     (let ((eligible-for-before (and closest-tipping-point all-after-tipping-point
                                                     (eq relative-position 'before))))
                       (cond ((and eligible-for-before
                                   (org-noter--compare-locations '> location closest-notes-location))
                              (setq closest-notes (list (cons element nil))
                                    closest-notes-location location
                                    current-region-info nil
                                    closest-notes-regions nil)
                              (org-noter--view-region-add current-region-info closest-notes-regions element))

                             ((and eligible-for-before (equal location closest-notes-location))
                              (push (cons element nil) closest-notes)
                              (org-noter--view-region-add current-region-info closest-notes-regions element))

                             (t (org-noter--view-region-finish current-region-info element)))))))

                 (when new-location
                   (setq preamble nil)
                   (cond ((and (org-noter--compare-locations '<= location new-location)
                               (or (eq (car reference-for-insertion) 'before)
                                   (org-noter--compare-locations '>= location reference-location)))
                          (setq reference-for-insertion (cons 'after element)
                                reference-location location))

                         ((and (eq (car reference-for-insertion) 'after)
                               (< (org-element-property :begin element)
                                  (org-element-property :end (cdr reference-for-insertion)))
                               (org-noter--compare-locations '>= location new-location))
                          (setq reference-for-insertion (cons 'before element)
                                reference-location location)))))

                (t
                 (when (and preamble new-location
                            (or (not reference-for-insertion)
                                (>= (org-element-property :begin element)
                                    (org-element-property :end (cdr reference-for-insertion)))))
                   (setq reference-for-insertion (cons 'after element)))))))
           nil nil (delete 'headline (append org-element-all-elements nil))))
       
       (org-noter--view-region-finish current-region-info)

       (setf (org-noter--session-num-notes-in-view session) (length notes-in-view))

       (when all-after-tipping-point (setq notes-in-view (append closest-notes notes-in-view)))

       (make-org-noter--view-info
        :notes (nreverse notes-in-view)
        :regions (nreverse regions-in-view)
        :prev-regions (nreverse closest-notes-regions)
        :reference-for-insertion reference-for-insertion)))))

(defun org-noter--make-view-info-for-single-note (session headline)
  (let ((not-belonging-element
         (org-element-map (org-element-contents headline) 'headline
           (lambda (headline)
             (let ((doc-file (org-noter--doc-file-property headline)))
               (and (or (and doc-file (not (string= doc-file (org-noter--session-property-text session))))
                        (org-noter--check-location-property headline))
                    headline)))
           nil t)))

    (make-org-noter--view-info
     ;; NOTE(nox): The cdr is only used when inserting, doesn't matter here
     :notes (list (cons headline nil))
     :regions (list (cons (org-element-property :begin headline)
                          (or (and not-belonging-element (org-element-property :begin not-belonging-element))
                              (org-element-property :end headline)))))))

(defun org-noter--doc-location-change-handler ()
  (org-noter--with-valid-session
   (let ((view-info (org-noter--get-view-info (org-noter--get-current-view))))
     (force-mode-line-update t)
     (unless org-noter--inhibit-location-change-handler
       (org-noter--get-notes-window (cond ((org-noter--view-info-regions view-info) 'scroll)
                                          ((org-noter--view-info-prev-regions view-info) 'only-prev)))
       (org-noter--focus-notes-region view-info)))

   (when (org-noter--session-auto-save-last-location session) (org-noter-set-start-location))))

(defun org-noter--mode-line-text ()
  (org-noter--with-valid-session
   (let* ((number-of-notes (or (org-noter--session-num-notes-in-view session) 0)))
     (cond ((= number-of-notes 0) (propertize " 0 notes " 'face 'org-noter-no-notes-exist-face))
           ((= number-of-notes 1) (propertize " 1 note " 'face 'org-noter-notes-exist-face))
           (t (propertize (format " %d notes " number-of-notes) 'face 'org-noter-notes-exist-face))))))

;; NOTE(nox): From machc/pdf-tools-org
(defun org-noter--pdf-tools-edges-to-region (edges)
  "Get 4-entry region (LEFT TOP RIGHT BOTTOM) from several EDGES."
  (when edges
    (let ((left0 (nth 0 (car edges)))
          (top0 (nth 1 (car edges)))
          (bottom0 (nth 3 (car edges)))
          (top1 (nth 1 (car (last edges))))
          (right1 (nth 2 (car (last edges))))
          (bottom1 (nth 3 (car (last edges)))))
      (list left0
            (+ top0 (/ (- bottom0 top0) 3))
            right1
            (- bottom1 (/ (- bottom1 top1) 3))))))

(defun org-noter--check-if-document-is-annotated-on-file (document-path notes-path)
  ;; NOTE(nox): In order to insert the correct file contents
  (let ((buffer (find-buffer-visiting notes-path)))
    (when buffer (with-current-buffer buffer (save-buffer)))

    (with-temp-buffer
      (insert-file-contents notes-path)
      (catch 'break
        (while (re-search-forward (org-re-property org-noter-property-doc-file) nil t)
          (when (file-equal-p (or (expand-file-name (match-string 3) (file-name-directory notes-path))
                                  (cadar (org-collect-keywords '(org-noter-property-doc-file))))
                                 document-path)
            ;; NOTE(nox): This notes file has the document we want!
            (throw 'break t)))))))

(defsubst org-noter--check-doc-prop (doc-prop)
  (and doc-prop (or (string-match-p org-link-bracket-re doc-prop)
                    (string-match-p org-noter--url-regexp doc-prop)
                    (and (not (file-directory-p doc-prop)) (file-readable-p doc-prop)))))

(defun org-noter--get-or-read-document-property (inherit-prop &optional force-new)
  (let ((doc-prop (and (not force-new) (or (org-entry-get nil org-noter-property-doc-file inherit-prop)
                                           (cadar (org-collect-keywords (list org-noter-property-doc-file)))))))

    (setq doc-prop (or (run-hook-with-args-until-success 'org-noter-parse-document-property-hook doc-prop)
                       doc-prop))
    
    (unless (org-noter--check-doc-prop doc-prop)
      (setq doc-prop nil)

      (when org-noter-suggest-from-attachments
        (require 'org-attach)
        (let* ((attach-dir (org-attach-dir))
               (attach-list (and attach-dir (org-attach-file-list attach-dir))))
          (when (and attach-list (y-or-n-p "Do you want to annotate an attached file?"))
            (setq doc-prop (completing-read "File to annotate: " attach-list nil t))
            (when doc-prop (setq doc-prop (file-relative-name (expand-file-name doc-prop attach-dir)))))))

      (unless (org-noter--check-doc-prop doc-prop)
        (setq doc-prop (expand-file-name
                        (read-file-name
                         (cond
                          ((null doc-prop) "No document property found. Please specify a document path: ")
                          ((file-directory-p doc-prop)
                           (format "Document property (\"%s\") is a directory. Please specify a document file: "
                                   doc-prop))
                          ((not (file-readable-p doc-prop))
                           (format "The file specified by the document property \"%s\" is unreadable. Please specify a new document: "
                                   doc-prop)))
                         nil nil t)))
        (when (or (file-directory-p doc-prop) (not (file-readable-p doc-prop)))
          (user-error "Invalid file path"))
        (when (y-or-n-p "Do you want a relative file name? ")
          (setq doc-prop (file-relative-name doc-prop))))

      (org-entry-put nil org-noter-property-doc-file doc-prop))
    doc-prop))

(defun org-noter--other-frames (&optional this-frame)
  "Returns non-`nil' when there is at least another frame"
  (setq this-frame (or this-frame (selected-frame)))
  (catch 'other-frame
    (dolist (frame (visible-frame-list))
      (unless (or (eq this-frame frame)
                  (frame-parent frame)
                  (frame-parameter frame 'delete-before))
        (throw 'other-frame frame)))))

;; --------------------------------------------------------------------------------
;;; User commands
(defun org-noter-set-start-location (&optional arg)
  "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
  (interactive "P")
  (org-noter--with-valid-session
   (let ((inhibit-read-only t)
         (ast (org-noter--parse-root))
         (location (org-noter--doc-approx-location
                    (when (called-interactively-p 'any) 'interactive))))
     (with-current-buffer (org-noter--session-notes-buffer session)
       (org-with-wide-buffer
        (goto-char (org-element-property :begin ast))
        (if arg
            (org-entry-delete nil org-noter-property-note-location)
          (org-entry-put nil org-noter-property-note-location
                         (org-noter--pretty-print-location location))))))))

(defun org-noter-set-auto-save-last-location (arg)
  "This toggles saving the last visited location for this document.
With a prefix ARG, delete the current setting and use the default."
  (interactive "P")
  (org-noter--with-valid-session
   (let ((inhibit-read-only t)
         (ast (org-noter--parse-root))
         (new-setting (if arg
                          org-noter-auto-save-last-location
                        (not (org-noter--session-auto-save-last-location session)))))
     (setf (org-noter--session-auto-save-last-location session)
           new-setting)
     (with-current-buffer (org-noter--session-notes-buffer session)
       (org-with-wide-buffer
        (goto-char (org-element-property :begin ast))
        (if arg
            (org-entry-delete nil org-noter--property-auto-save-last-location)
          (org-entry-put nil org-noter--property-auto-save-last-location (format "%s" new-setting)))
        (unless new-setting (org-entry-delete nil org-noter-property-note-location)))))))

(defun org-noter-set-hide-other (arg)
  "This toggles hiding other headings for the current session.
- With a prefix \\[universal-argument], set the current setting permanently for this document.
- With a prefix \\[universal-argument] \\[universal-argument], remove the setting and use the default."
  (interactive "P")
  (org-noter--with-valid-session
   (let* ((inhibit-read-only t)
          (ast (org-noter--parse-root))
          (persistent
           (cond ((equal arg '(4)) 'write)
                 ((equal arg '(16)) 'remove)))
          (new-setting
           (cond ((eq persistent 'write) (org-noter--session-hide-other session))
                 ((eq persistent 'remove) org-noter-hide-other)
                 ('other-cases (not (org-noter--session-hide-other session))))))
     (setf (org-noter--session-hide-other session) new-setting)
     (when persistent
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if (eq persistent 'write)
              (org-entry-put nil org-noter--property-hide-other (format "%s" new-setting))
            (org-entry-delete nil org-noter--property-hide-other))))))))

(defun org-noter-set-closest-tipping-point (arg)
  "This sets the closest note tipping point (see `org-noter-closest-tipping-point')
- With a prefix \\[universal-argument], set it permanently for this document.
- With a prefix \\[universal-argument] \\[universal-argument], remove the setting and use the default."
  (interactive "P")
  (org-noter--with-valid-session
   (let* ((ast (org-noter--parse-root))
          (inhibit-read-only t)
          (persistent (cond ((equal arg '(4)) 'write)
                            ((equal arg '(16)) 'remove)))
          (new-setting (if (eq persistent 'remove)
                           org-noter-closest-tipping-point
                         (read-number "New tipping point: " (org-noter--session-closest-tipping-point session)))))
     (setf (org-noter--session-closest-tipping-point session) new-setting)
     (when persistent
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if (eq persistent 'write)
              (org-entry-put nil org-noter--property-closest-tipping-point (format "%f" new-setting))
            (org-entry-delete nil org-noter--property-closest-tipping-point))))))))

(defun org-noter-set-notes-window-behavior (arg)
  "Set the notes window behaviour for the current session.
With a prefix ARG, it becomes persistent for that document.

See `org-noter-notes-window-behavior' for more information."
  (interactive "P")
  (org-noter--with-valid-session
   (let* ((inhibit-read-only t)
          (ast (org-noter--parse-root))
          (possible-behaviors (list '("Default" . default)
                                    '("On start" . start)
                                    '("On scroll" . scroll)
                                    '("On scroll to location that only has previous notes" . only-prev)
                                    '("Never" . never)))
          chosen-behaviors)

     (while (> (length possible-behaviors) 1)
       (let ((chosen-pair (assoc (completing-read "Behavior: " possible-behaviors nil t) possible-behaviors)))
         (cond ((eq (cdr chosen-pair) 'default) (setq possible-behaviors nil))

               ((eq (cdr chosen-pair) 'never) (setq chosen-behaviors (list 'never)
                                                    possible-behaviors nil))

               ((eq (cdr chosen-pair) 'done) (setq possible-behaviors nil))

               (t (push (cdr chosen-pair) chosen-behaviors)
                  (setq possible-behaviors (delq chosen-pair possible-behaviors))
                  (when (= (length chosen-behaviors) 1)
                    (setq possible-behaviors (delq (rassq 'default possible-behaviors) possible-behaviors)
                          possible-behaviors (delq (rassq 'never possible-behaviors) possible-behaviors))
                    (push (cons "Done" 'done) possible-behaviors))))))

     (setf (org-noter--session-window-behavior session)
           (or chosen-behaviors org-noter-notes-window-behavior))

     (when arg
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if chosen-behaviors
              (org-entry-put nil org-noter--property-behavior (format "%s" chosen-behaviors))
            (org-entry-delete nil org-noter--property-behavior))))))))

(defun org-noter-set-notes-window-location (arg)
  "Set the notes window default location for the current session.
With a prefix ARG, it becomes persistent for that document.

See `org-noter-notes-window-behavior' for more information."
  (interactive "P")
  (org-noter--with-valid-session
   (let* ((inhibit-read-only t)
          (ast (org-noter--parse-root))
          (location-possibilities
           '(("Default" . nil)
             ("Horizontal split" . horizontal-split)
             ("Vertical split" . vertical-split)
             ("Other frame" . other-frame)))
          (location
           (cdr (assoc (completing-read "Location: " location-possibilities nil t)
                       location-possibilities)))
          (notes-buffer (org-noter--session-notes-buffer session)))

     (setf (org-noter--session-window-location session)
           (or location org-noter-notes-window-location))

     (let (exists)
       (dolist (window (get-buffer-window-list notes-buffer nil t))
         (setq exists t)
         (with-selected-frame (window-frame window)
           (if (= (count-windows) 1)
               (delete-frame)
             (delete-window window))))
       (when exists (org-noter--get-notes-window 'force)))

     (when arg
       (with-current-buffer notes-buffer
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if location
              (org-entry-put nil org-noter--property-location
                             (format "%s" location))
            (org-entry-delete nil org-noter--property-location))))))))

(defun org-noter-set-doc-split-fraction (arg)
  "Set the fraction of the frame that the document window will occupy when split.
- With a prefix \\[universal-argument], set it permanently for this document.
- With a prefix \\[universal-argument] \\[universal-argument], remove the setting and use the default."
  (interactive "P")
  (org-noter--with-valid-session
   (let* ((ast (org-noter--parse-root))
          (inhibit-read-only t)
          (persistent (cond ((equal arg '(4)) 'write)
                            ((equal arg '(16)) 'remove)))
          (current-setting (org-noter--session-doc-split-fraction session))
          (new-setting
           (if (eq persistent 'remove)
               org-noter-doc-split-fraction
             (cons (read-number "Horizontal fraction: " (car current-setting))
                   (read-number "Vertical fraction: " (cdr current-setting))))))
     (setf (org-noter--session-doc-split-fraction session) new-setting)
     (when (org-noter--get-notes-window)
       (with-current-buffer (org-noter--session-doc-buffer session)
         (delete-other-windows)
         (org-noter--get-notes-window 'force)))

     (when persistent
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if (eq persistent 'write)
              (org-entry-put nil org-noter--property-doc-split-fraction (format "%s" new-setting))
            (org-entry-delete nil org-noter--property-doc-split-fraction))))))))

(defun org-noter-kill-session (&optional session)
  "Kill an `org-noter' session.

When called interactively, if there is no prefix argument and the
buffer has an annotation session, it will kill it; else, it will
show a list of open `org-noter' sessions, asking for which to
kill.

When called from elisp code, you have to pass in the SESSION you
want to kill."
  (interactive "P")
  (when (and (called-interactively-p 'any) (> (length org-noter--sessions) 0))
    ;; NOTE(nox): `session' is representing a prefix argument
    (if (and org-noter--session (not session))
        (setq session org-noter--session)
      (setq session nil)
      (let (collection default doc-display-name notes-file-name display)
        (dolist (session org-noter--sessions)
          (setq doc-display-name (org-noter--session-display-name session)
                notes-file-name (file-name-nondirectory
                                 (org-noter--session-notes-file-path session))
                display (concat doc-display-name " - " notes-file-name))
          (when (eq session org-noter--session) (setq default display))
          (push (cons display session) collection))
        (setq session (cdr (assoc (completing-read "Which session? " collection nil t
                                                   nil nil default)
                                  collection))))))

  (when (and session (memq session org-noter--sessions))
    (setq org-noter--sessions (delq session org-noter--sessions))

    (when (eq (length org-noter--sessions) 0)
      (remove-hook 'delete-frame-functions 'org-noter--handle-delete-frame)
      (advice-remove 'doc-view-goto-page 'org-noter--location-change-advice)
      (advice-remove 'nov-render-document 'org-noter--nov-scroll-handler))

    (let* ((ast   (org-noter--parse-root session))
           (frame (org-noter--session-frame session))
           (notes-buffer (org-noter--session-notes-buffer session))
           (base-buffer (buffer-base-buffer notes-buffer))
           (notes-modified (buffer-modified-p base-buffer))
           (doc-buffer (org-noter--session-doc-buffer session)))

      (dolist (window (get-buffer-window-list notes-buffer nil t))
        (with-selected-frame (window-frame window)
          (if (= (count-windows) 1)
              (when (org-noter--other-frames) (delete-frame))
            (delete-window window))))

      (with-current-buffer notes-buffer
        (remove-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer t)
        (restore-buffer-modified-p nil))
      (unless org-noter-use-indirect-buffer
        (kill-buffer notes-buffer))

      (when base-buffer
        (with-current-buffer base-buffer
          (org-noter--unset-text-properties ast)
          (set-buffer-modified-p notes-modified)))

      (with-current-buffer doc-buffer
        (remove-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer t))
      (kill-buffer doc-buffer)

      (when (frame-live-p frame)
        (if (and (org-noter--other-frames) org-noter-kill-frame-at-session-end)
            (delete-frame frame)
          (progn
            (delete-other-windows)
            (set-frame-parameter nil 'name nil)))))))

(defun org-noter-create-skeleton ()
  "Create notes skeleton based on the outline of the document."
  (interactive)
  (org-noter--with-valid-session
   (or (run-hook-with-args-until-success 'org-noter-create-skeleton-functions
                                         (org-noter--session-doc-mode session))
       (user-error "This command is not supported for %s"
                   (org-noter--session-doc-mode session)))))

(defun org-noter-insert-note (&optional precise-info note-title)
  "Insert note associated with the current location.

This command will prompt for a title of the note and then insert
it in the notes buffer. When the input is empty, a title based on
`org-noter-default-heading-title' will be generated.

If there are other notes related to the current location, the
prompt will also suggest them. Depending on the value of the
variable `org-noter-closest-tipping-point', it may also
suggest the closest previous note.

PRECISE-INFO makes the new note associated with a more
specific location (see `org-noter-insert-precise-note' for more
info).

When you insert into an existing note and have text selected on
the document buffer, the variable `org-noter-insert-selected-text-inside-note'
defines if the text should be inserted inside the note."
  (interactive)
  (org-noter--with-valid-session
   (let* ((ast (org-noter--parse-root)) (contents (org-element-contents ast))
          (window (org-noter--get-notes-window 'force))
          (selected-text
           (run-hook-with-args-until-success
            'org-noter-get-selected-text-hook
            (org-noter--session-doc-mode session)))

          force-new
          (location (org-noter--doc-approx-location (or precise-info 'interactive) (gv-ref force-new)))
          (view-info (org-noter--get-view-info (org-noter--get-current-view) location)))

     (let ((inhibit-quit t))
       (with-local-quit
         (select-frame-set-input-focus (window-frame window))
         (select-window window)

         ;; IMPORTANT(nox): Need to be careful changing the next part, it is a bit
         ;; complicated to get it right...

         (let ((point (point))
               (minibuffer-local-completion-map org-noter--completing-read-keymap)
               collection default default-begin title selection quote-p
               (empty-lines-number (if org-noter-separate-notes-from-heading 2 1)))

           (cond
            ;; NOTE(nox): Both precise and without questions will create new notes
            ((or precise-info force-new)
             (setq quote-p (with-temp-buffer
                             (insert (or selected-text ""))
                             (> (how-many "\n" (point-min)) -1)))
             (setq default (and selected-text
                                (replace-regexp-in-string "\n" " " selected-text))))
            (org-noter-insert-note-no-questions)
            (t
             (dolist (note-cons (org-noter--view-info-notes view-info))
               (let ((display (org-element-property :raw-value (car note-cons)))
                     (begin (org-element-property :begin (car note-cons))))
                 (push (cons display note-cons) collection)
                 (when (and (>= point begin) (> begin (or default-begin 0)))
                   (setq default display
                         default-begin begin))))))

           (setq collection (nreverse collection)
                 title (if (or org-noter-insert-note-no-questions note-title)
                           (or org-noter-default-heading-title note-title)
                         (completing-read "Note: " collection nil nil nil nil default))
                 selection (unless org-noter-insert-note-no-questions (cdr (assoc title collection))))

           (if selection
               ;; NOTE(nox): Inserting on an existing note
               (let* ((note (car selection))
                      (insert-before-element (cdr selection))
                      (has-content
                       (eq (org-element-map (org-element-contents note) org-element-all-elements
                             (lambda (element)
                               (if (org-noter--check-location-property element)
                                   'stop
                                 (not (memq (org-element-type element) '(section property-drawer)))))
                             nil t)
                           t)))
                 (when has-content (setq empty-lines-number 2))
                 (if insert-before-element
                     (goto-char (org-element-property :begin insert-before-element))
                   (goto-char (org-element-property :end note)))


                 (if (org-at-heading-p)
                     (progn
                       (org-N-empty-lines-before-current empty-lines-number)
                       (forward-line -1))
                   (unless (bolp) (insert "\n"))
                   (org-N-empty-lines-before-current (1- empty-lines-number)))

                 (when (and org-noter-insert-selected-text-inside-note selected-text) (insert selected-text)))

             ;; NOTE(nox): Inserting a new note
             (let ((reference-element-cons (org-noter--view-info-reference-for-insertion view-info))
                   level)
               (when (or quote-p (zerop (length title)))
                 (setq title (replace-regexp-in-string (regexp-quote "$p$")
                                                       (org-noter--pretty-print-location location)
                                                       title)))

               (if reference-element-cons
                   (progn
                     (cond
                      ((eq (car reference-element-cons) 'before)
                       (goto-char (org-element-property :begin (cdr reference-element-cons))))
                      ((eq (car reference-element-cons) 'after)
                       (goto-char (org-element-property :end (cdr reference-element-cons)))))

                     ;; NOTE(nox): This is here to make the automatic "should insert blank" work better.
                     (when (org-at-heading-p) (backward-char))

                     (setq level (org-element-property :level (cdr reference-element-cons))))

                 (goto-char (or (org-element-map contents 'section
                                  (lambda (section) (org-element-property :end section))
                                  nil t org-element-all-elements)
                                (point-max))))
               
               (setq level (1+ (or (org-element-property :level ast) 0)))

               ;; NOTE(nox): This is needed to insert in the right place
               (unless (org-noter--no-heading-p) (outline-show-entry))
               (org-noter--insert-heading level title empty-lines-number location)
               (when quote-p
                 (save-excursion
                   (insert "#+BEGIN_QUOTE\n" selected-text "\n#+END_QUOTE")))
               (when (org-noter--session-hide-other session) (org-overview))

               (setf (org-noter--session-num-notes-in-view session)
                     (1+ (org-noter--session-num-notes-in-view session)))))

           (org-show-set-visibility t)
           (org-cycle-hide-drawers 'all)
           (org-cycle-show-empty-lines t)))
       (when quit-flag
         ;; NOTE(nox): If this runs, it means the user quitted while creating a note, so
         ;; revert to the previous window.
         (select-frame-set-input-focus (org-noter--session-frame session))
         (select-window (get-buffer-window (org-noter--session-doc-buffer session))))))))

(defun org-noter-insert-precise-note (&optional toggle-no-questions)
  "Insert note associated with a specific location.
This will ask you to click where you want to scroll to when you
sync the document to this note. You should click on the top of
that part. Will always create a new note.

When text is selected, it will automatically choose the top of
the selected text as the location and the text itself as the
title of the note (you may change it anyway!).

See `org-noter-insert-note' docstring for more."
  (interactive "P")
  (org-noter--with-valid-session
   (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                 (not org-noter-insert-note-no-questions)
                                               org-noter-insert-note-no-questions))
         (precise-info (org-noter--get-precise-info)))
     (message (format "dmitry --- %s" precise-info))
     ;; TODO: This is all wrong.
     (pdf-annot-add-highlight-markup-annotation precise-info)
     (org-noter-insert-note precise-info))))


(defun org-noter-insert-note-toggle-no-questions ()
  "Insert note associated with the current location.
This is like `org-noter-insert-note', except it will toggle `org-noter-insert-note-no-questions'"
  (interactive)
  (org-noter--with-valid-session
   (let ((org-noter-insert-note-no-questions (not org-noter-insert-note-no-questions)))
     (org-noter-insert-note))))

(defmacro org-noter--map-ignore-headings-with-doc-file (contents match-first &rest body)
  `(let (ignore-until-level)
     (org-element-map ,contents 'headline
       (lambda (headline)
         (let ((doc-file (org-noter--doc-file-property headline))
               (location (org-noter--parse-location-property headline)))
           (when (and ignore-until-level (<= (org-element-property :level headline) ignore-until-level))
             (setq ignore-until-level nil))

           (cond
            (ignore-until-level nil) ;; NOTE(nox): This heading is ignored, do nothing
            ((and doc-file (not (string= doc-file (org-noter--session-property-text session))))
             (setq ignore-until-level (org-element-property :level headline)) nil)
            (t ,@body))))
       nil ,match-first org-noter--note-search-no-recurse)))

(defun org-noter-sync-prev-page-or-chapter ()
  "Show previous page or chapter that has notes, in relation to the current page or chapter.
This will force the notes window to popup."
  (interactive)
  (org-noter--with-valid-session
   (let ((this-location (org-noter--doc-approx-location 0))
         (contents (org-element-contents (org-noter--parse-root)))
         target-location)
     (org-noter--get-notes-window 'force)

     (org-noter--map-ignore-headings-with-doc-file
      contents nil
      (when (and (org-noter--compare-locations '<  location this-location)
                 (org-noter--compare-locations '>f location target-location))
        (setq target-location location)))

     (org-noter--get-notes-window 'force)
     (select-window (org-noter--get-doc-window))
     (if target-location
         (org-noter--doc-goto-location target-location)
       (user-error "There are no more previous pages or chapters with notes")))))

(defun org-noter-sync-current-page-or-chapter ()
  "Show current page or chapter notes.
This will force the notes window to popup."
  (interactive)
  (org-noter--with-valid-session
   (let ((window (org-noter--get-notes-window 'force)))
     (select-frame-set-input-focus (window-frame window))
     (select-window window)
     (org-noter--doc-location-change-handler))))

(defun org-noter-sync-next-page-or-chapter ()
  "Show next page or chapter that has notes, in relation to the current page or chapter.
This will force the notes window to popup."
  (interactive)
  (org-noter--with-valid-session
   (let ((this-location (org-noter--doc-approx-location most-positive-fixnum))
         (contents (org-element-contents (org-noter--parse-root)))
         target-location)

     (org-noter--map-ignore-headings-with-doc-file
      contents nil
      (when (and (org-noter--compare-locations '> location this-location)
                 (org-noter--compare-locations '< location target-location))
        (setq target-location location)))

     (org-noter--get-notes-window 'force)
     (select-window (org-noter--get-doc-window))
     (if target-location
         (org-noter--doc-goto-location target-location)
       (user-error "There are no more following pages or chapters with notes")))))

(defun org-noter-sync-prev-note ()
  "Go to the location of the previous note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (let ((org-noter--inhibit-location-change-handler t)
         (contents (org-element-contents (org-noter--parse-root)))
         (current-begin (org-element-property :begin (org-noter--get-containing-element)))
         previous)
     (when current-begin
       (org-noter--map-ignore-headings-with-doc-file
        contents t
        (when location
          (if (= current-begin (org-element-property :begin headline))
              t
            (setq previous headline)
            nil))))

     (if previous
         (progn
           ;; NOTE(nox): This needs to be manual so we can focus the correct note
           (org-noter--doc-goto-location (org-noter--parse-location-property previous))
           (org-noter--focus-notes-region (org-noter--make-view-info-for-single-note session previous)))
       (user-error "There is no previous note"))))
  (select-window (org-noter--get-doc-window)))

(defun org-noter-sync-current-note ()
  "Go the location of the selected note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (if (string= (or (org-noter--get-or-read-document-property t)
                    (cadar (org-collect-keywords (list org-noter-property-doc-file))))

                (org-noter--session-property-text session))
       (let ((location (org-noter--parse-location-property (org-noter--get-containing-element))))
         (if location
             (org-noter--doc-goto-location location)
           (user-error "No note selected")))
     (user-error "You are inside a different document")))
  (let ((window (org-noter--get-doc-window)))
    (select-frame-set-input-focus (window-frame window))
    (select-window window)))

(defun org-noter-sync-next-note ()
  "Go to the location of the next note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (org-noter--with-selected-notes-window
   "No notes window exists"
   (let ((org-noter--inhibit-location-change-handler t)
         (contents (org-element-contents (org-noter--parse-root)))
         next)

     (org-noter--map-ignore-headings-with-doc-file
      contents t
      (when (and location (< (point) (org-element-property :begin headline)))
        (setq next headline)))

     (if next
         (progn
           (org-noter--doc-goto-location (org-noter--parse-location-property next))
           (org-noter--focus-notes-region (org-noter--make-view-info-for-single-note session next)))
       (user-error "There is no next note"))))
  (select-window (org-noter--get-doc-window)))

(define-minor-mode org-noter-doc-mode
  "Minor mode for the document buffer.
Keymap:
\\{org-noter-doc-mode-map}"
  :keymap `((,(kbd   "i")   . org-noter-insert-note)
            (,(kbd "C-i")   . org-noter-insert-note-toggle-no-questions)
            (,(kbd "M-i")   . org-noter-insert-precise-note)
            (,(kbd   "q")   . org-noter-kill-session)
            (,(kbd "M-p")   . org-noter-sync-prev-page-or-chapter)
            (,(kbd "M-.")   . org-noter-sync-current-page-or-chapter)
            (,(kbd "M-n")   . org-noter-sync-next-page-or-chapter)
            (,(kbd "C-M-p") . org-noter-sync-prev-note)
            (,(kbd "C-M-.") . org-noter-sync-current-note)
            (,(kbd "C-M-n") . org-noter-sync-next-note))

  (let ((mode-line-segment '(:eval (org-noter--mode-line-text))))
    (if org-noter-doc-mode
        (if (symbolp (car-safe mode-line-format))
            (setq mode-line-format (list mode-line-segment mode-line-format))
          (push mode-line-segment mode-line-format))
      (setq mode-line-format (delete mode-line-segment mode-line-format)))))

(define-minor-mode org-noter-notes-mode
  "Minor mode for the notes buffer.
Keymap:
\\{org-noter-notes-mode-map}"
  :keymap `((,(kbd "M-p")   . org-noter-sync-prev-page-or-chapter)
            (,(kbd "M-.")   . org-noter-sync-current-page-or-chapter)
            (,(kbd "M-n")   . org-noter-sync-next-page-or-chapter)
            (,(kbd "C-M-p") . org-noter-sync-prev-note)
            (,(kbd "C-M-.") . org-noter-sync-current-note)
            (,(kbd "C-M-n") . org-noter-sync-next-note))
  (if org-noter-doc-mode
      (org-noter-doc-mode -1)))

(provide 'org-noter-core)
;;; org-noter-core.el ends here
