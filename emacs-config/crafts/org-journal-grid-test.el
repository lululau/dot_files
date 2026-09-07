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
  (should (equal (org-journal-grid--display-title "09:00") "09:00"))
  (should (equal (org-journal-grid--display-title
                  "19:50 [[file:/tmp/foo.md][添加 zlib 技能文档. [dir: ~/.agents/] (rev: 8b6b30b)]]")
                 "添加 zlib 技能文档. [dir: ~/.agents/] (rev: 8b6b30b)"))
  (should (equal (org-journal-grid--display-title
                  "[[https://example.com][Example]]")
                 "Example"))
  (should (equal (org-journal-grid--display-title "[[https://example.com]]")
                 "https://example.com"))
  ;; Path contains raw brackets; org-link-display-format cannot parse it.
  (should (equal (org-journal-grid--display-title
                  (concat "19:50 [[file:/tmp/2026-03-24_添加 zlib. "
                          "[dir_ ~/.agents_] (rev_ 8b6b30b).md]"
                          "[添加 zlib 技能文档，封装 Z-Library CLI 实现搜索、"
                          "下载、历史浏览与配额检查. "
                          "[dir: ~/.agents/] (rev: 8b6b30b)]]"))
                 (concat "添加 zlib 技能文档，封装 Z-Library CLI 实现搜索、"
                         "下载、历史浏览与配额检查. "
                         "[dir: ~/.agents/] (rev: 8b6b30b)"))))

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

(ert-deftest org-journal-grid-range-start-keeping-end ()
  (should (equal (org-journal-grid--range-start-keeping-end 10001 7 8) 10000))
  (should (equal (org-journal-grid--range-start-keeping-end 10001 7 6) 10002))
  (should (equal (org-journal-grid--range-start-keeping-end 10001 7 1) 10007)))

(ert-deftest org-journal-grid-never-shows-future-dates ()
  (let* ((org-journal-grid-days 7)
         (today (calendar-absolute-from-gregorian (calendar-current-date))))
    (should (equal (org-journal-grid--range-start (+ today 10))
                   (- today 6)))
    (should (<= (+ (org-journal-grid--clamp-week-start (- today 3) 7) 6)
                today))
    (should (equal (org-journal-grid--clamp-week-start (- today 3) 7)
                   (- today 6)))
    ;; Decrease to 4 days then restore 7 without clamp would leak into the
    ;; future; keeping-end must clamp.
    (let ((after-shrink (org-journal-grid--range-start-keeping-end
                         (- today 6) 7 4)))
      (should (equal after-shrink (- today 3)))
      (should (<= (+ (org-journal-grid--range-start-keeping-end
                      after-shrink 4 7)
                     6)
                  today)))))

(ert-deftest org-journal-grid-file-name ()
  (should (equal (org-journal-grid--file-name
                  (calendar-absolute-from-gregorian '(9 4 2026)))
                 "2026-09-04")))

(ert-deftest org-journal-grid-list-events-filters ()
  (let* ((dir (make-temp-file "ojg-" t))
         (org-journal-grid-directory dir)
         (org-journal-grid-show-todo nil)
         (org-journal-grid-default-duration-minutes 30)
         (day (calendar-absolute-from-gregorian '(9 4 2026)))
         (file (expand-file-name "2026-09-04" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* 2026-09-04\n"
                    "** DONE 10:43 压缩 PNG\n"
                    "** TODO 07:00 研究 timegrid\n"
                    "** 09:00 无关键字\n"
                    "*** DONE 11:00 嵌套不应出现\n"
                    "** DONE 无时刻\n"))
          (let* ((start (* day 1440))
                 (end (* (1+ day) 1440))
                 (events (org-journal-grid--list-events start end))
                 (titles (mapcar #'org-journal-grid-event-title events)))
            (should (equal (sort titles #'string<)
                           '("压缩 PNG" "无关键字")))
            (dolist (event events)
              (should (null (org-journal-grid-event-state event)))
              (should (< (org-journal-grid-event-start event)
                         (org-journal-grid-event-end event))))))
      (delete-directory dir t))))

(ert-deftest org-journal-grid-list-events-local-show-todo ()
  "Buffer-local `org-journal-grid-show-todo' must survive parse buffer switches."
  (let* ((dir (make-temp-file "ojg-" t))
         (org-journal-grid-directory dir)
         (day (calendar-absolute-from-gregorian '(9 4 2026)))
         (file (expand-file-name "2026-09-04" dir))
         (start (* day 1440))
         (end (* (1+ day) 1440))
         (saved org-journal-grid-show-todo))
    (unwind-protect
        (progn
          ;; Match the live grid: nil global default, buffer-local t.
          (setq org-journal-grid-show-todo nil)
          (with-temp-file file
            (insert "* 2026-09-04\n"
                    "** DONE 10:43 压缩 PNG\n"
                    "** TODO 07:00 研究 timegrid\n"))
          (with-temp-buffer
            (setq-local org-journal-grid-show-todo t)
            (let ((titles (mapcar #'org-journal-grid-event-title
                                  (org-journal-grid--list-events start end))))
              (should (equal (sort titles #'string<)
                             '("压缩 PNG" "研究 timegrid"))))))
      (setq org-journal-grid-show-todo saved)
      (delete-directory dir t))))

(ert-deftest org-journal-grid-set-days-updates-global-default ()
  "Setting days must update both buffer-local and global default value."
  (let ((saved-default (default-value 'org-journal-grid-days)))
    (unwind-protect
        (with-temp-buffer
          (setq-default org-journal-grid-days 7)
          (org-journal-grid-mode)
          (setq org-journal-grid--state (org-journal-grid--calendar-state-create
                                         :week-start 738000))
          (cl-letf (((symbol-function 'org-journal-grid--reload-state) #'ignore)
                    ((symbol-function 'org-journal-grid--refresh) #'ignore))
            (org-journal-grid-increase-days 3)
            (should (= org-journal-grid-days 3))
            (should (= (default-value 'org-journal-grid-days) 3))
            (org-journal-grid-decrease-days 5)
            (should (= org-journal-grid-days 5))
            (should (= (default-value 'org-journal-grid-days) 5))))
      (setq-default org-journal-grid-days saved-default))))

(provide 'org-journal-grid-test)
;;; org-journal-grid-test.el ends here
