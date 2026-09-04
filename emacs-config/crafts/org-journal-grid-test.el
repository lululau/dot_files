;;; org-journal-grid-test.el --- Tests for org-journal-grid parser -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'org-journal-grid-render)
(require 'org-journal-grid)

(ert-deftest org-journal-grid-parse-clock ()
  (should (equal (org-journal-grid--parse-clock "10:43 压缩 PNG") 643))
  (should (equal (org-journal-grid--parse-clock "09:00 title") 540))
  (should (equal (org-journal-grid--parse-clock "9:00 title") 540))
  (should (null (org-journal-grid--parse-clock "no time here")))
  (should (null (org-journal-grid--parse-clock "Ruby 2 Features")))
  (should (null (org-journal-grid--parse-clock "24:00 too late")))
  (should (null (org-journal-grid--parse-clock "10:99 bad minute"))))

(ert-deftest org-journal-grid-display-title ()
  (should (equal (org-journal-grid--display-title "10:43 压缩 PNG") "压缩 PNG"))
  (should (equal (org-journal-grid--display-title "09:00") "09:00")))

(ert-deftest org-journal-grid-include-todo ()
  (let ((org-not-done-keywords '("TODO" "NEXT"))
        (org-journal-grid-show-todo nil))
    (should-not (org-journal-grid--include-todo-p "TODO"))
    (should-not (org-journal-grid--include-todo-p "NEXT"))
    (should (org-journal-grid--include-todo-p "DONE"))
    (should (org-journal-grid--include-todo-p nil)))
  (let ((org-not-done-keywords '("TODO"))
        (org-journal-grid-show-todo t))
    (should (org-journal-grid--include-todo-p "TODO"))))

(ert-deftest org-journal-grid-clamp-end ()
  ;; 23:50 + 30 minutes must not cross midnight.
  (let* ((day 738000)
         (start (+ (* day 1440) (* 23 60) 50)))
    (should (equal (org-journal-grid--clamp-end start 30)
                   (* (1+ day) 1440)))
    (should (equal (org-journal-grid--clamp-end start 5)
                   (+ start 5)))))

(ert-deftest org-journal-grid-range-start-trailing ()
  (let ((org-journal-grid-days 7))
    (should (equal (org-journal-grid--range-start 10007) 10001))))

(ert-deftest org-journal-grid-file-name ()
  (should (equal (org-journal-grid--file-name
                  (calendar-absolute-from-gregorian '(9 4 2026)))
                 "2026-09-04")))

(provide 'org-journal-grid-test)
;;; org-journal-grid-test.el ends here
