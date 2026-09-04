# org-journal-grid Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a read-only SVG time grid that shows the last N calendar days of org-journal entries and jumps to the source heading on RET/double-click.

**Architecture:** Port `org-timegrid-model.el` and a read-only slice of `org-timegrid.el` into `emacs-config/crafts/` under the `org-journal-grid-*` prefix. New journal-specific code in `org-journal-grid.el` lists level-2 `HH:MM` headings from `YYYY-MM-DD` files and supplies a list+visit backend. No `require` of org-timegrid, no agenda integration, no calendar mutation.

**Tech Stack:** Emacs Lisp 29+, Org 9.6+, Emacs SVG, `org-element`, Spacemacs crafts autoloads

**Spec:** `docs/superpowers/specs/2026-09-04-org-journal-grid-design.md`

**Source to adapt:** `~/cascode/github.com/org-timegrid/` (`org-timegrid-model.el`, `org-timegrid.el` only)

---

## File map

| File | Role |
|------|------|
| Create: `emacs-config/crafts/org-journal-grid-test.el` | ERT for parser/filter/time helpers |
| Create: `emacs-config/crafts/org-journal-grid.el` | defcustom、解析、backend、`org-journal-grid` 命令（覆盖现有空文件） |
| Create: `emacs-config/crafts/org-journal-grid-model.el` | 改编自 `org-timegrid-model.el` |
| Create: `emacs-config/crafts/org-journal-grid-render.el` | 改编自 `org-timegrid.el`，只读 |
| Modify: `emacs-config/crafts/init.el` | autoload `org-journal-grid` |
| Modify: `emacs-config/key-bindings/spacemacs-set-leader-keys.el` | `"aojg"` |

Do not copy `org-timegrid-org.el`, `org-timegrid-agenda.el`, `org-timegrid-calendar.el`, or `org-timegrid-isearch.el`.

---

### Task 1: Parser ERT (failing)

**Files:**
- Create: `emacs-config/crafts/org-journal-grid-test.el`

- [ ] **Step 1: Write the ERT file**

```elisp
;;; org-journal-grid-test.el --- Tests for org-journal-grid parser -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
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
```

- [ ] **Step 2: Run tests; they must fail because helpers are missing**

```bash
emacs -Q --batch -L /Users/liuxiang/.config/emacs-config/crafts \
  -l org-journal-grid-test.el -f ert-run-tests-batch-and-exit
```

Expected: cannot open `org-journal-grid` or void-function `org-journal-grid--parse-clock`.

- [ ] **Step 3: Commit**

```bash
git add emacs-config/crafts/org-journal-grid-test.el
git commit -m "新增 org-journal-grid 解析辅助函数的 ERT"
```

---

### Task 2: Parser helpers and journal defcustoms

**Files:**
- Create/overwrite: `emacs-config/crafts/org-journal-grid.el`

- [ ] **Step 1: Write helpers and defcustoms (no backend yet)**

Overwrite the empty stub. Do **not** `(require 'org-journal-grid-render)` yet.

```elisp
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

(defun org-journal-grid--last-day-index ()
  "Return the zero-based index of the last visible day."
  (1- org-journal-grid-days))

(defun org-journal-grid--range-start (&optional absolute-date)
  "Return the first visible day of a trailing window ending on ABSOLUTE-DATE.
Unlike org-timegrid, this never snaps to `calendar-week-start-day'."
  (let ((absolute (or absolute-date
                      (calendar-absolute-from-gregorian
                       (calendar-current-date)))))
    (- absolute (org-journal-grid--last-day-index))))

(defun org-journal-grid--file-name (absolute-date)
  "Return the YYYY-MM-DD basename for ABSOLUTE-DATE."
  (let ((date (calendar-gregorian-from-absolute absolute-date)))
    (format "%04d-%02d-%02d" (nth 2 date) (nth 0 date) (nth 1 date))))

(provide 'org-journal-grid)
;;; org-journal-grid.el ends here
```

`org-journal-grid-days` is **not** defined here. Task 4's renderer copy will define it (default 7). Tests that let-bind it are fine.

- [ ] **Step 2: Run ERT**

```bash
emacs -Q --batch -L /Users/liuxiang/.config/emacs-config/crafts \
  -l org-journal-grid.el -l org-journal-grid-test.el \
  -f ert-run-tests-batch-and-exit
```

Expected: all tests PASS. `org-journal-grid-range-start-trailing` let-binds `org-journal-grid-days`; if the variable is void, add this above the range-start test or before `provide` in the implementation:

```elisp
(defvar org-journal-grid-days 7)
```

If the test file failed on void `org-journal-grid-days`, add that `defvar` to `org-journal-grid.el` (the renderer copy later uses `defcustom` of the same name; a preceding `defvar` is overwritten by `defcustom` and is safe).

- [ ] **Step 3: Syntax-check**

```bash
~/.agents/skills/elisp-syntax-check/elisp-check.sh \
  /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid.el \
  /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-test.el
```

Expected: 0 errors. Warnings about `org-journal-grid-days` if any should be fixed with the `defvar` above.

- [ ] **Step 4: Commit**

```bash
git add emacs-config/crafts/org-journal-grid.el
git commit -m "实现 org-journal-grid 解析辅助函数与行为 defcustom"
```

---

### Task 3: Port the model file

**Files:**
- Create: `emacs-config/crafts/org-journal-grid-model.el`

- [ ] **Step 1: Copy and rename the prefix**

```bash
SRC=~/cascode/github.com/org-timegrid/org-timegrid-model.el
DST=/Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-model.el
cp "$SRC" "$DST"
python3 - <<'PY'
from pathlib import Path
p = Path("/Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-model.el")
text = p.read_text()
text = text.replace("org-timegrid", "org-journal-grid")
# File name in the first line / provide
text = text.replace(";;; org-journal-grid-model.el --- Records and backend protocol for org-journal-grid",
                    ";;; org-journal-grid-model.el --- Records and backend protocol for org-journal-grid")
p.write_text(text)
PY
```

- [ ] **Step 2: Edit the header commentary**

Replace the `Commentary` block with:

```elisp
;;; Commentary:

;; Data records and layout helpers shared by the journal grid renderer.
;; Adapted from org-timegrid (https://github.com/Gleek/org-timegrid).
;; Keep the original GPL-3 copyright of Umar Ahmad above.
```

Keep the original `Copyright (C) 2026 Umar Ahmad` block. Add a second line after it:

```elisp
;; Adapted for org-journal-grid by Liu Xiang.
```

Change `:group 'org-journal-grid` on `org-journal-grid-slot-minutes` (already renamed). The group is declared in the renderer; this matches the original pattern.

Confirm `(provide 'org-journal-grid-model)` at the end.

- [ ] **Step 3: Syntax-check the model**

```bash
~/.agents/skills/elisp-syntax-check/elisp-check.sh \
  /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-model.el
```

Expected: 0 errors.

- [ ] **Step 4: Commit**

```bash
git add emacs-config/crafts/org-journal-grid-model.el
git commit -m "移植 org-timegrid-model 为 org-journal-grid-model"
```

---

### Task 4: Port the renderer and fix journal window rules

**Files:**
- Create: `emacs-config/crafts/org-journal-grid-render.el`

- [ ] **Step 1: Copy, rename prefix, drop isearch**

```bash
SRC=~/cascode/github.com/org-timegrid/org-timegrid.el
DST=/Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-render.el
cp "$SRC" "$DST"
python3 - <<'PY'
from pathlib import Path
p = Path("/Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-render.el")
text = p.read_text()
text = text.replace("org-timegrid", "org-journal-grid")
# The file must provide a distinct feature from org-journal-grid.el.
text = text.replace("(provide 'org-journal-grid)\n(require 'org-journal-grid-isearch)",
                    "(provide 'org-journal-grid-render)")
# First-line file name
text = text.replace(";;; org-journal-grid.el ---",
                    ";;; org-journal-grid-render.el ---")
p.write_text(text)
PY
# Confirm isearch require is gone
rg -n "isearch|provide " /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-render.el | tail -20
```

Expected: no `org-journal-grid-isearch`. `(provide 'org-journal-grid-render)` is the last provide. `(require 'org-journal-grid-model)` exists (renamed from model).

If the python replace of `provide` did not match (whitespace), delete the isearch require by hand and change provide to `org-journal-grid-render`.

- [ ] **Step 2: Header, buffer name, trailing-day window**

In the header, after the original copyright, add `;; Adapted for org-journal-grid by Liu Xiang.` and change Commentary to say it is a read-only journal grid renderer adapted from org-timegrid.

Change buffer name default:

```elisp
(defcustom org-journal-grid-buffer-name "*org-journal-grid*"
```

Replace `org-journal-grid--range-start` (the copied week-snap version) with:

```elisp
(defun org-journal-grid--range-start (&optional absolute-date)
  "Return the first visible day of a trailing window ending on ABSOLUTE-DATE."
  (let ((absolute (or absolute-date
                      (calendar-absolute-from-gregorian
                       (calendar-current-date)))))
    (- absolute (org-journal-grid--last-day-index))))
```

There will now be **two** definitions of `org-journal-grid--range-start` if Task 2 already defined it: one in `org-journal-grid.el`, one in the renderer. **Delete the function from `org-journal-grid.el`** and keep the renderer copy (the renderer calls it at open). Keep `org-journal-grid--last-day-index` only in the renderer too if both exist; delete the duplicate from `org-journal-grid.el`.

Update the ERT file: tests still call `org-journal-grid--range-start`, so the test command must load the renderer:

```bash
emacs -Q --batch -L /Users/liuxiang/.config/emacs-config/crafts \
  -l org-journal-grid-model.el -l org-journal-grid-render.el \
  -l org-journal-grid.el -l org-journal-grid-test.el \
  -f ert-run-tests-batch-and-exit
```

If `org-journal-grid.el` is loaded first it may still define range-start until you delete it — delete first.

- [ ] **Step 3: Syntax-check renderer + re-run ERT**

```bash
~/.agents/skills/elisp-syntax-check/elisp-check.sh \
  /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-model.el \
  /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-render.el \
  /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid.el
```

Expected: 0 errors. Byte-compile may warn about unused mutation helpers; leave them until Task 5.

Re-run ERT with the three-file load order above. Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add emacs-config/crafts/org-journal-grid-render.el emacs-config/crafts/org-journal-grid.el emacs-config/crafts/org-journal-grid-test.el
git commit -m "移植 org-timegrid 渲染器并改为连续日历日窗口"
```

---

### Task 5: Make the renderer read-only

**Files:**
- Modify: `emacs-config/crafts/org-journal-grid-render.el`

Mouse drag already degrades to click when create/update are nil (`"Unsupported drag"`). Do not rewrite the drag engine. Change keys and RET.

- [ ] **Step 1: RET visits only**

Replace `org-journal-grid-open-at-cursor` with:

```elisp
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
```

- [ ] **Step 2: Keymap — drop editing, bind `g`/`t`**

In `org-journal-grid-mode-map` initializer (the `defvar` that `keymap-set`s):

1. Change `(keymap-set map "g" #'org-journal-grid-refresh)` to `(keymap-set map "g" #'org-journal-grid--refresh-data)` so `g` keeps cursor and scroll (spec). `--refresh-data` is not interactive; wrap it:

```elisp
(defun org-journal-grid-reload ()
  "Reload events without resetting cursor or viewport."
  (interactive)
  (org-journal-grid--refresh-data))
```

Bind `"g"` to `org-journal-grid-reload`.

2. Remove or unbind these keys (delete the `keymap-set` lines, and the later `keymap-set org-journal-grid-mode-map ...` duplicates after the `defvar`):

- `"<remap> <undo>"`, `"<remap> <undo-only>"`, `"<remap> <undo-redo>"`, `"C-/"`, `"C-_"`, `"C-x u"`, `"M-_"`
- `"d"`, `"e"`, `"<delete>"`
- `"DEL"` and `"<backspace>"` currently call `org-journal-grid-remove-or-page-up`. Bind them to `org-journal-grid-cursor-page-up` instead (page up only).
- `"M-<down>"` `"M-<up>"` `"M-<right>"` `"M-<left>"`
- `"S-<down>"` `"S-<up>"` `"C-S-<up>"` `"C-S-<down>"`
- `"S-<right>"` `"S-<left>"` `"C-S-<left>"` `"C-S-<right>"`
- `"t"` (was retime)
- `"M-w"` `"C-w"` `"C-y"`

3. After those deletions, add:

```elisp
(keymap-set map "t" #'org-journal-grid-toggle-todo)
```

`org-journal-grid-toggle-todo` will be defined in `org-journal-grid.el` in Task 6. For this task, put a stub in the renderer **or** (preferred) define the function in `org-journal-grid.el` now:

```elisp
(defun org-journal-grid-toggle-todo ()
  "Buffer-locally toggle display of unfinished TODO headings."
  (interactive)
  (setq-local org-journal-grid-show-todo (not org-journal-grid-show-todo))
  (when (fboundp 'org-journal-grid--refresh-data)
    (org-journal-grid--refresh-data))
  (message "TODO entries %s"
           (if org-journal-grid-show-todo "shown" "hidden")))
```

Put this in `org-journal-grid.el` so the renderer keymap can reference it. Autoload is not required inside the package.

Also delete the post-`defvar` block that re-sets `M-<down>` etc. (the "Keep reevaluation..." `dolist` / `keymap-set` cluster). Leave mouse `[down-mouse-1]` → `press` (nil create-function already falls back to click).

- [ ] **Step 3: Syntax-check and ERT**

```bash
~/.agents/skills/elisp-syntax-check/elisp-check.sh \
  /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-render.el \
  /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid.el

emacs -Q --batch -L /Users/liuxiang/.config/emacs-config/crafts \
  -l org-journal-grid-model.el -l org-journal-grid-render.el \
  -l org-journal-grid.el -l org-journal-grid-test.el \
  -f ert-run-tests-batch-and-exit
```

Expected: 0 errors, ERT PASS.

- [ ] **Step 4: Commit**

```bash
git add emacs-config/crafts/org-journal-grid-render.el emacs-config/crafts/org-journal-grid.el
git commit -m "将 org-journal-grid 渲染器改为只读并绑定 g/t"
```

---

### Task 6: Journal backend, listing, visit, command

**Files:**
- Modify: `emacs-config/crafts/org-journal-grid.el`
- Modify: `emacs-config/crafts/org-journal-grid-test.el`

- [ ] **Step 1: Add a listing ERT that uses a temp journal dir**

Append to `org-journal-grid-test.el`:

```elisp
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
```

- [ ] **Step 2: Run the new test; it must fail**

```bash
emacs -Q --batch -L /Users/liuxiang/.config/emacs-config/crafts \
  -l org-journal-grid-model.el -l org-journal-grid-render.el \
  -l org-journal-grid.el -l org-journal-grid-test.el \
  -f ert-run-tests-batch-and-exit
```

Expected: FAIL `org-journal-grid-list-events-filters` (void-function `org-journal-grid--list-events`).

- [ ] **Step 3: Implement listing, visit, backend, command**

Add to `org-journal-grid.el` **before** `(provide 'org-journal-grid)`, and add `(require 'org-journal-grid-model)` plus `(require 'org-journal-grid-render)` at the top (after `org`).

```elisp
(defun org-journal-grid--tag-color (tags)
  "Return the colour for the first TAGS member in the colour alist."
  (seq-some (lambda (tag)
              (cdr (assoc tag org-journal-grid-tag-color-alist)))
            tags))

(defun org-journal-grid--headline-event (file headline absolute-date)
  "Build an event from FILE HEADLINE on ABSOLUTE-DATE, or nil."
  (when (= (org-element-property :level headline) 2)
    (let* ((todo (org-element-property :todo-keyword headline))
           (raw (org-element-property :raw-value headline))
           (clock (org-journal-grid--parse-clock raw)))
      (when (and clock (org-journal-grid--include-todo-p todo))
        (let* ((begin (org-element-property :begin headline))
               (start (+ (* absolute-date 1440) clock))
               (end (org-journal-grid--clamp-end
                     start org-journal-grid-default-duration-minutes))
               (tags (org-element-property :tags headline)))
          (org-journal-grid-event-create
           :id (format "%s:%s" file begin)
           :title (org-journal-grid--display-title raw)
           :start start
           :end end
           :all-day nil
           :tags tags
           :state nil
           :color (org-journal-grid--tag-color tags)
           :source (list :file file :position begin)))))))

(defun org-journal-grid--parse-file (file absolute-date)
  "Return events from FILE for ABSOLUTE-DATE."
  (let ((parse
         (lambda ()
           (org-element-map (org-element-parse-buffer 'headline) 'headline
             (lambda (headline)
               (org-journal-grid--headline-event file headline absolute-date))))))
    (condition-case nil
        (if-let ((buf (find-buffer-visiting file)))
            (with-current-buffer buf
              (funcall parse))
          (with-temp-buffer
            (insert-file-contents file)
            (delay-mode-hooks (org-mode))
            (funcall parse)))
      (error nil))))

(defun org-journal-grid--events-for-day (absolute-date)
  "Return events for ABSOLUTE-DATE, or nil if the file is missing."
  (let* ((dir (org-journal-grid--resolved-directory))
         (file (expand-file-name (org-journal-grid--file-name absolute-date) dir)))
    (when (and (file-regular-p file) (file-readable-p file))
      (delq nil (org-journal-grid--parse-file file absolute-date)))))

(defun org-journal-grid--list-events (start end)
  "Return journal events intersecting START and END (absolute minutes)."
  (let* ((start-day (floor start 1440))
         (end-day (floor (1- end) 1440))
         events)
    (cl-loop for day from start-day to end-day
             do (setq events (nconc events (org-journal-grid--events-for-day day))))
    events))

(defun org-journal-grid--visit (event)
  "Jump to EVENT's org-journal heading."
  (let* ((source (org-journal-grid-event-source event))
         (file (plist-get source :file))
         (position (plist-get source :position)))
    (unless (and file (file-exists-p file))
      (user-error "Journal file disappeared: %s" file))
    (find-file file)
    (goto-char (or position (point-min)))
    (org-fold-show-context 'org-goto)))

(defvar org-journal-grid-backend
  (org-journal-grid-backend-create
   :name "org-journal"
   :list-function #'org-journal-grid--list-events
   :visit-function #'org-journal-grid--visit)
  "Read-only backend for `org-journal-grid'.")

(defun org-journal-grid (&optional days)
  "Open a read-only journal time grid.
Without a prefix, show `org-journal-grid-days' ending today.
A numeric prefix DAYS overrides the width for this buffer only."
  (interactive "P")
  (unless (image-type-available-p 'svg)
    (user-error "org-journal-grid requires Emacs SVG support"))
  (let* ((width (if days (prefix-numeric-value days) org-journal-grid-days))
         (dir (org-journal-grid--resolved-directory)))
    (unless (and (integerp width) (> width 0))
      (user-error "org-journal-grid-days must be a positive integer"))
    (unless (file-directory-p dir)
      (user-error "Journal directory does not exist: %s" dir))
    (let ((org-journal-grid-days width))
      (org-journal-grid-open org-journal-grid-backend)
      (when-let ((buf (get-buffer org-journal-grid-buffer-name)))
        (with-current-buffer buf
          (setq-local org-journal-grid-days width))))))
```

`org-journal-grid-tag-color-alist` is defined by the renderer copy (renamed from org-timegrid-org's alist — **check**). The renderer file may **not** have `org-journal-grid-tag-color-alist` because that custom lived in `org-timegrid-org.el`, which we did not copy.

If the variable is missing after the renderer port, add this defcustom to `org-journal-grid.el` next to the other behaviour customs:

```elisp
(defcustom org-journal-grid-tag-color-alist nil
  "Alist of Org tag strings to calendar colours."
  :type '(alist :key-type string :value-type (choice symbol color)))
```

- [ ] **Step 4: Run ERT; all tests PASS**

Same emacs -Q --batch command as Step 2. Expected: PASS including `org-journal-grid-list-events-filters`.

- [ ] **Step 5: Syntax-check**

```bash
~/.agents/skills/elisp-syntax-check/elisp-check.sh \
  /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid.el \
  /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-render.el \
  /Users/liuxiang/.config/emacs-config/crafts/org-journal-grid-model.el
```

Expected: 0 errors.

- [ ] **Step 6: Commit**

```bash
git add emacs-config/crafts/org-journal-grid.el emacs-config/crafts/org-journal-grid-test.el
git commit -m "实现 org-journal-grid 只读 backend 与入口命令"
```

---

### Task 7: Autoload and leader key

**Files:**
- Modify: `emacs-config/crafts/init.el`
- Modify: `emacs-config/key-bindings/spacemacs-set-leader-keys.el`

- [ ] **Step 1: Autoload**

In `emacs-config/crafts/init.el`, after the `proced-enhanced-mode` autoload line, add:

```elisp
(autoload 'org-journal-grid "org-journal-grid" nil t)
```

- [ ] **Step 2: Leader key**

In `emacs-config/key-bindings/spacemacs-set-leader-keys.el`, immediately after `"aojJ" #'org-journal-new-entry`, add:

```elisp
  "aojg" #'org-journal-grid
```

- [ ] **Step 3: Commit**

```bash
git add emacs-config/crafts/init.el emacs-config/key-bindings/spacemacs-set-leader-keys.el
git commit -m "为 org-journal-grid 添加 autoload 与 SPC a o j g"
```

---

### Task 8: Live Emacs verification

**Files:** none unless a bug is found.

- [ ] **Step 1: Load in the running Spacemacs session**

```bash
export TMPDIR=$(getconf DARWIN_USER_TEMP_DIR)
emacsclient -e '(progn
  (require (quote org-journal-grid))
  (list (featurep (quote org-journal-grid))
        (featurep (quote org-timegrid))
        (fboundp (quote org-journal-grid))))'
```

Expected: `(t nil t)` — journal grid loaded, org-timegrid **not** loaded.

- [ ] **Step 2: Open the grid**

```bash
emacsclient -e '(org-journal-grid)'
```

Expected: buffer `*org-journal-grid*` appears. Then inspect with:

```bash
emacsclient -e '(with-current-buffer "*org-journal-grid*"
  (list (buffer-name)
        major-mode
        org-journal-grid-days
        org-journal-grid-show-todo))'
```

Expected: `("*org-journal-grid*" org-journal-grid-mode 7 nil)`.

- [ ] **Step 3: Check events against real journal files**

```bash
emacsclient -e "$(cat <<'EOF'
(let* ((end (calendar-absolute-from-gregorian (calendar-current-date)))
       (start (- end 6))
       (events (org-journal-grid--list-events (* start 1440) (* (1+ end) 1440))))
  (cons (length events)
        (mapcar (lambda (e)
                  (list (org-journal-grid-event-title e)
                        (org-journal-grid-event-state e)
                        (org-journal-grid-event-start e)))
                events)))
EOF
)"
```

Manually confirm against `~/Documents/materials/journal` for the last 7 calendar days:

- Only level-2 headings with `HH:MM`
- No `TODO` / unfinished keywords
- `DONE` titles appear without being marked done
- A day with no `YYYY-MM-DD` file contributes no events (empty column)

- [ ] **Step 4: RET visit and `t` toggle**

In the grid UI: move to a block, press RET — the journal file opens at that heading.

Then in the grid buffer press `t`, and re-run the list-events snippet with `org-journal-grid-show-todo` t in that buffer, or press `g` after `t` and confirm TODO blocks appear.

- [ ] **Step 5: Fix any mismatch, re-run ERT and syntax-check, commit if needed**

If behaviour diverges from the spec, fix in `org-journal-grid.el` (parser/filter) or the renderer (window/keys), then:

```bash
emacs -Q --batch -L /Users/liuxiang/.config/emacs-config/crafts \
  -l org-journal-grid-model.el -l org-journal-grid-render.el \
  -l org-journal-grid.el -l org-journal-grid-test.el \
  -f ert-run-tests-batch-and-exit
```

Commit only if you changed code:

```bash
git add emacs-config/crafts/org-journal-grid.el emacs-config/crafts/org-journal-grid-render.el
git commit -m "修复 org-journal-grid 对照真实 journal 的偏差"
```

---

## Spec coverage

| Spec item | Task |
|-----------|------|
| Read-only SVG grid, no agenda | 5, 6 |
| Port renderer/model, rename prefix, keep GPL | 3, 4 |
| No require org-timegrid | 6, 8 |
| Trailing N calendar days, empty columns | 2, 4, 6 |
| Level-2 + HH:MM only | 1, 2, 6 |
| Timestamp = filename date + heading clock | 6 |
| Fixed duration, midnight clamp | 1, 2, 6 |
| Hide unfinished TODO by default; DONE not dimmed | 1, 2, 6 |
| All behaviour via defcustom | 2, 4, 6 |
| RET / double-click visit | 5, 6 |
| Keys n/p b/f M-b/M-f j . g t q zoom | 4, 5 (copied, then t/g patched) |
| Prefix arg days, buffer-local TODO toggle | 5, 6 |
| Autoload + `SPC a o j g` | 7 |
| Temp-buffer parse for unvisited files | 6 |
| SVG / missing dir / missing file errors | 6 |
| Live emacsclient verification | 8 |
