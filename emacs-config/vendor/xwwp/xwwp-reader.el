;;; xwwp-reader.el --- reader mode for `xwwp' browser  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Q. Hong <qhong@mit.edu>

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; Implement reader mode for `xwidget-webkit' sessions.

;;; Code:


(require 'xwwp)

(defconst xwwp-reader-js-content
  (with-temp-buffer
    (insert-file-contents (concat xwwp-source-directory "xwwp-readability.js"))
    (buffer-string)))
(defconst xwwp-reader-css-content
  (with-temp-buffer
    (insert-file-contents (concat xwwp-source-directory "xwwp-readability.css"))
    (buffer-string)))

(defgroup xwwp-reader nil
  "`xwidget-webkit' reader mode customizations."
  :group 'xwwp)
(defcustom xwwp-reader-style "style-newspaper"
  "CSS style for XWWP reader mode."
  :group 'xwwp-reader
  :type '(radio
          (const :tag "Newspaper" "style-newspaper")
          (const :tag "Novel" "style-novel")
          (const :tag "Ebook" "style-ebook")
          (const :tag "Terminal" "style-terminal")))
(defcustom xwwp-reader-convert-links-to-foot-notes nil
  "Convert links to foot notes."
  :group 'xwwp-reader
  :type 'boolean)
(defcustom xwwp-reader-size "size-medium"
  "Font size for XWWP reader mode."
  :group 'xwwp-reader
  :type '(radio
          (const :tag "Very Small" "size-x-small")
          (const :tag "Small" "size-small")
          (const :tag "Medium" "size-medium")
          (const :tag "Large" "size-large")
          (const :tag "Very Large" "size-x-large")))
(defcustom xwwp-reader-margin "margin-medium"
  "Margin size for XWWP reader mode."
  :group 'xwwp-reader
  :type '(radio
          (const :tag "Very Narrow" "margin-x-narrow")
          (const :tag "Narrow" "margin-narrow")
          (const :tag "Medium" "margin-medium")
          (const :tag "Wide" "margin-wide")
          (const :tag "Very Wide" "margin-x-wide")))

(defun xwwp-reader-toggle (&optional xwidget)
  "Toggle reader mode in current XWIDGET session."
  (interactive)
  (let ((xwidget (or xwidget (xwidget-webkit-current-session))))
    (xwwp-html-inject-style xwidget "__xwidget_plus_readability_style" xwwp-reader-css-content)
    (xwidget-webkit-execute-script
     xwidget
     (format "var readStyle = \"%s\";
var readConvertLinksToFootnotes = \"%s\";
var readSize = \"%s\";
var readMargin = \"%s\";"
             xwwp-reader-style
             (if xwwp-reader-convert-links-to-foot-notes "true" "false")
             xwwp-reader-size xwwp-reader-margin))
    (xwidget-webkit-execute-script xwidget xwwp-reader-js-content)))
(provide 'xwwp-reader)
;;; xwwp-reader.el ends here
