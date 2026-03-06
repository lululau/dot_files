;;; buffer-tools-for-gptel.el --- Buffer tools for gptel -*- lexical-binding: t -*-

;; Author: User
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (gptel "0.0"))
;; Keywords: tools, convenience, gptel

;; This file is not part of GNU Emacs

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

;; Buffer tools for gptel using gptel-make-tool.
;; Provides view-buffer, edit-buffer, replace-buffer, buffer-search, and list-buffers.

;;; Code:

(require 'gptel)
(require 'seq)
(require 'cl-lib)

;;; Helper functions

(defun buffer-tools-gptel--view-text (lines offset limit)
  "Process LINES array with OFFSET and LIMIT parameters.
OFFSET is 0-based line number to start from.
LIMIT is maximum number of lines to return.
Returns selected lines joined with newlines."
  (let* ((total-lines (length lines))
         (offset-value (or offset 0)))
    (when (< offset-value 0)
      (error "Offset must be non-negative, got %s" offset-value))
    (when (>= offset-value total-lines)
      (error "Offset %s exceeds line count %s" offset-value total-lines))
    (let* ((start offset-value)
           (end (min (+ start (or limit total-lines)) total-lines))
           (selected-lines (seq-subseq lines start end)))
      (string-join selected-lines "\n"))))

(defun buffer-tools-gptel--make-edit (buffer old-string new-string)
  "Replace exactly one occurrence of OLD-STRING with NEW-STRING in BUFFER."
  (when (string= old-string "")
    (error "`old_string' cannot be empty"))
  (let ((name (concat "buffer " (buffer-name buffer))))
    (with-current-buffer buffer
      (let ((case-fold-search nil))
        (save-excursion
          (goto-char (point-min))
          (let ((count 0)
                (first-match-pos nil))
            (while (search-forward old-string nil 'noerror)
              (setq count (1+ count))
              (unless first-match-pos
                (setq first-match-pos (match-beginning 0))))
            (cond
             ((= count 0)
              (error "Could not find text '%s' to replace in %s"
                     old-string name))
             ((> count 1)
              (error "Found %d matches for '%s' in %s, need exactly one"
                     count old-string name))
             (t
              (goto-char first-match-pos)
              (search-forward old-string nil 'noerror)
              (replace-match new-string 'fixedcase 'literal)
              (format "Successfully edited %s" name)))))))))

(defun buffer-tools-gptel--user-buffer-p (buf)
  "Return t if BUF is a user-relevant buffer."
  (let ((buf-name (buffer-name buf)))
    (and buf-name
         (not (string-prefix-p " " buf-name))
         (not (string-prefix-p "*" buf-name))
         (buffer-live-p buf))))

;;; Tool declarations

(gptel-make-tool
 :name "ViewBuffer"
 :description "View contents of an Emacs buffer with optional OFFSET and LIMIT.

Use this tool to read buffer contents when you need to:
- Inspect the current state of a buffer
- Read specific sections with OFFSET and LIMIT parameters
- Review buffer content before making edits

OFFSET is 0-based line number to start from (default: 0).
LIMIT is maximum number of lines to return (default: all lines).

If you don't know the buffer name, use ListBuffers first to see available buffers."
 :function (lambda (buffer-name &optional offset limit)
             (if-let* ((buf (get-buffer buffer-name)))
                 (with-current-buffer buf
                   (let ((lines (split-string (buffer-string) "\n")))
                     (buffer-tools-gptel--view-text lines offset limit)))
               (error "Buffer not found: %s" buffer-name)))
 :args '(( :name "buffer_name"
           :type string
           :description "Name of the buffer to view.")
         ( :name "offset"
           :type integer
           :description "Line number to start reading from (0-based, default 0)."
           :optional t)
         ( :name "limit"
           :type integer
           :description "Maximum number of lines to return (default: all lines)."
           :optional t))
 :category "buffer-tools"
 :include t)

(gptel-make-tool
 :name "EditBuffer"
 :description "Edit an Emacs buffer by replacing exactly one occurrence of text.

Use this tool for precise, surgical edits to buffer content.
The OLD_STRING must match exactly once (uniquely) in the buffer.
This prevents accidental multi-replacements.

For larger edits, consider using ReplaceBuffer instead.

IMPORTANT: OLD_STRING must match the buffer content EXACTLY, including:
- Whitespace (spaces, tabs, newlines)
- Case sensitivity
- All characters

If you get \"Found N matches\" error, provide more context to make the match unique."
 :function (lambda (buffer-name old-string new-string)
             (let ((buffer (get-buffer buffer-name)))
               (unless buffer
                 (error "Buffer not found: %s" buffer-name))
               (buffer-tools-gptel--make-edit buffer old-string new-string)))
 :args '(( :name "buffer_name"
           :type string
           :description "Name of the buffer to modify.")
         ( :name "old_string"
           :type string
           :description "Text to replace (must match exactly once).")
         ( :name "new_string"
           :type string
           :description "Text to replace old_string with."))
 :category "buffer-tools"
 :confirm t)

(gptel-make-tool
 :name "ReplaceBuffer"
 :description "Completely overwrite the contents of a buffer with new content.

Use this tool when you need to:
- Replace the entire buffer content
- Make large-scale changes
- Rewrite a buffer completely

WARNING: This completely erases the buffer and replaces it with CONTENT.
Use EditBuffer for smaller, targeted edits instead."
 :function (lambda (buffer-name content)
             (if-let* ((buffer (get-buffer buffer-name)))
                 (progn
                   (with-current-buffer buffer
                     (let ((buffer-read-only nil))
                       (erase-buffer)
                       (insert content)))
                   (format "Buffer content replaced: %s" buffer-name))
               (error "Buffer does not exist: %s" buffer-name)))
 :args '(( :name "buffer_name"
           :type string
           :description "Name of the buffer to overwrite.")
         ( :name "content"
           :type string
           :description "Content to write to the buffer."))
 :category "buffer-tools"
 :confirm t)

(gptel-make-tool
 :name "BufferSearch"
 :description "Search for a regex pattern within an Emacs buffer.

Use this tool to:
- Find specific text patterns in a buffer
- Locate function/variable definitions
- Search for keywords or identifiers

The regex uses Emacs regex syntax where parentheses do NOT need to be escaped.
Examples:
- Search for function def: \"(defun\" (not \"\\\\(defun\")
- Search for variable: \"(defvar\"
- Search for word: \"\\\\=foo\\\\=\" (exact word match)

Returns matching lines with line numbers."
 :function (lambda (pattern buffer)
             (let ((buf (get-buffer buffer)))
               (unless buf
                 (error "Buffer '%s' does not exist" buffer))
               (with-current-buffer buf
                 (save-excursion
                   (condition-case err
                       (let ((matched-lines '()))
                         (goto-char (point-min))
                         (while (re-search-forward pattern nil t)
                           (push (line-number-at-pos) matched-lines)
                           (forward-line 1))
                         (setq matched-lines (delete-dups (nreverse matched-lines)))
                         (if matched-lines
                             (mapconcat
                              (lambda (line-num)
                                (save-excursion
                                  (goto-char (point-min))
                                  (forward-line (1- line-num))
                                  (format "%d: %s" line-num
                                          (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position)))))
                              matched-lines
                              "\n")
                           (error "No matches found")))
                     (invalid-regexp
                      (error "Invalid regexp pattern: %s. Remember Emacs regex syntax (e.g. \\\\(group\\\\) not (group)). Error: %s"
                             pattern (error-message-string err))))))))
 :args '(( :name "pattern"
           :type string
           :description "Regex pattern to search for. Uses Emacs regex syntax (parentheses NOT escaped). Example: \"(defun\" not \"\\\\(defun\".")
         ( :name "buffer"
           :type string
           :description "Name of buffer in which to search."))
 :category "buffer-tools"
 :include t)

(gptel-make-tool
 :name "ListBuffers"
 :description "List all active, user-relevant Emacs buffers.

Use this tool to:
- See what buffers are currently open
- Find buffer names before using ViewBuffer or EditBuffer
- Check if a specific buffer exists

Excludes internal buffers (names starting with space or asterisk).
Returns a list of buffer names, with file paths for file-visiting buffers."
 :function (lambda ()
             (let* ((all-buffers (buffer-list))
                    (user-buffers (seq-filter
                                   (lambda (buf)
                                     (buffer-tools-gptel--user-buffer-p buf))
                                   all-buffers))
                    (sorted-buffers (sort user-buffers
                                          (lambda (a b)
                                            (string< (buffer-name a)
                                                     (buffer-name b))))))
               (if sorted-buffers
                   (mapconcat (lambda (buf)
                                (concat
                                 (buffer-name buf)
                                 (when-let* ((file-name (buffer-file-name buf)))
                                   (format " (visiting file: %s)" file-name))))
                              sorted-buffers
                              "\n")
                 (error "No user-relevant buffers found"))))
 :args '()
 :category "buffer-tools"
 :include t)

(provide 'buffer-tools-for-gptel)

;;; buffer-tools-for-gptel.el ends here
