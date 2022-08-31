;;; xwwp-section.el --- Navigate section titles in `xwidget-webkit' sessions -*- lexical-binding: t; -*-

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

;; Add support for navigating section titles in `xwidget-webkit' sessions using fuzzy search.

;;; Code:

(require 'xwwp)
(require 'xwwp-follow-link)

(xwwp-js-def section highlight (ids selected)
  "Highlight IDS as candidate and SELECTED as selected.""
window.__xwidget_plus_section_candidates.forEach((h, id) => {
    h.classList.remove('xwwp-follow-link-candidate', 'xwwp-follow-link-selected');
    if (selected == id) {
        h.classList.add('xwwp-follow-link-selected');
        h.scrollIntoView({behavior: 'smooth', block: 'center'});
    } else if (ids && ids.includes(id)) {
        h.classList.add('xwwp-follow-link-candidate');
    }
});
")

(xwwp-js-def section fetch ()
  "Fetch all visible, non empty titles from the current page.""
var r = {};
window.__xwidget_plus_section_candidates = Array.from(document.querySelectorAll('h1, h2, h3, h4, h5, h6'));
window.__xwidget_plus_section_candidates.forEach((h, id) => {
    if (h.offsetWidth || h.offsetHeight || h.getClientRects().length) {
        if (h.innerText.match(/\\S/))
            r[id] = [h.innerText, '#'];
    }
});
return r;
")
(xwwp-js-def section action (link-id)
  "Select the title identified by LINK-ID""
__xwidget_plus_follow_link_cleanup();
")

(xwwp-js-def section cleanup ()
  "Remove all custom class from titles.""
window.__xwidget_plus_section_candidates.forEach(a => {
    a.classList.remove('xwwp-follow-link-candidate', 'xwwp-follow-link-selected');
});
window.__xwidget_plus_section_candidates = null;
")

(defun xwwp-section-update (xwidget)
  "Highligh TITLES in XWIDGET buffer when updating candidates."
  (let ((titles (xwwp-follow-link-candidates xwwp-follow-link-completion-backend-instance)))
    (when titles
      (let* ((selected (caar titles))
             (candidates (mapcar 'car (cdr titles))))
        (xwwp-section-highlight xwidget candidates selected)))))

(defun xwwp-section-callback (titles)
  "Ask for a TITLE belonging to the alist TITLES."
  (let* ((xwidget (xwidget-webkit-current-session))
         (titles (xwwp-follow-link-prepare-links titles)))
    (oset xwwp-follow-link-completion-backend-instance collection titles)
    (unwind-protect
        (xwwp-follow-link-read xwwp-follow-link-completion-backend-instance
                               "Section: " titles
                               (apply-partially #'xwwp-section-action xwidget)
                               (apply-partially #'xwwp-section-update xwidget))
      (xwwp-section-cleanup xwidget))
    (oset xwwp-follow-link-completion-backend-instance collection nil)))

(defun xwwp-section (&optional xwidget)
  "Ask for a title in the XWIDGET session or the current one and select it."
  (interactive)
  (setq xwwp-follow-link-completion-backend-instance (funcall (xwwp-follow-link-make-backend)))
  (let ((xwidget (or xwidget (xwidget-webkit-current-session))))
    (xwwp-html-inject-style xwidget "__xwidget_plus_follow_link_style" (xwwp-follow-link-style-definition))
    (xwwp-js-execute xwidget 'section)
    (xwwp-section-fetch xwidget #'xwwp-section-callback)))

(provide 'xwwp-section)
;;; xwwp-section.el ends here
