;;; xwwp-yank.el --- Better yanking in `xwidget-webkit' sessions -*- lexical-binding: t; -*-

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

;; Add support for yanking in input/test areas in `xwidget-webkit' sessions.

;;; Code:

(require 'xwwp)

(defgroup xwwp-yank nil
  "`xwidget-webkit' xwwp-yank customizations."
  :group 'xwwp)
(defcustom xwwp-yank-enabled (memq window-system '(mac ns))
  "Enable yanking workaround for NS webkit implementation."
  :group 'xwwp-yank)
(defun xwwp-yank-inject (xwidget)
  (xwidget-webkit-execute-script xwidget (format "
window.__xwidget_plus_yank_cache = '%s';
if(!window.__xwidget_plus_yank_handler){
  window.__xwidget_plus_yank_handler = function (ev) {
    if ( ev.ctrlKey && ev.code == 'KeyY' ){
      let ae = document.activeElement;
      if( ae && ( ae.nodeName == 'INPUT' || ae.nodeName == 'TEXTAREA' )){
        let pos = ae.selectionStart;
        let endpos = ae.selectionEnd;
        ae.value = ae.value.substring(0, pos) + window.__xwidget_plus_yank_cache + ae.value.substring(endpos);
      }
      ev.preventDefault();
    }
  }
}
document.removeEventListener('keydown', window.__xwidget_plus_yank_handler);
document.addEventListener('keydown', window.__xwidget_plus_yank_handler);
null;" (xwwp-js-string-escape (with-temp-buffer (yank) (buffer-string))))))
(defun xwwp-yank-select-window-advice (window &optional norecord)
  "If WINDOW contains an xwidget-webkit session, pass top of yank ring
into its window.__xwidget_plus_yank_current attribute."
  (when xwwp-yank-enabled
    (with-current-buffer (window-buffer window)
      (when (eq major-mode 'xwidget-webkit-mode)
        (let ((xwidget (xwidget-webkit-current-session)))
          (xwwp-yank-inject xwidget))))))
(defun xwwp-yank-xwidget-event-advice ()
  "Pass top of yank ring into current xwidget-webkit session's
window.__xwidget_plus_yank_current attribute."
  (when xwwp-yank-enabled
    (xwwp-yank-inject (xwidget-webkit-current-session))))
(advice-add 'select-window :after #'xwwp-yank-select-window-advice)
(advice-add 'xwidget-event-handler :after #'xwwp-yank-xwidget-event-advice)

(provide 'xwwp-yank)
;;; xwwp-yank.el ends here
