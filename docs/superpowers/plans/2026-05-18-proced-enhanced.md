# proced-enhanced Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Enhance Emacs built-in proced with incremental filter, pstree, lsof, and SIGKILL features.

**Architecture:** Single-file minor mode (`proced-enhanced-mode`) that auto-activates with `proced-mode-hook`. Filter uses overlay-based line hiding for isearch-like UX. pstree/lsof/sigkill read PIDs from proced buffer and delegate to lx/run-in-ghostel or signal-process.

**Tech Stack:** Emacs Lisp, proced.el (built-in), run-in-ghostel.el, ~/bin/pstree

**Spec:** `docs/superpowers/specs/2026-05-18-proced-enhanced-design.md`

---

### Task 1: Minor Mode Skeleton

**Files:**
- Create: `/Users/liuxiang/.config/emacs-config/crafts/proced-enhanced.el`

- [ ] **Step 1: Create the file with minor mode definition and keymap**

```elisp
;;; proced-enhanced.el --- Enhanced proced with filter, tree, lsof, sigkill  -*- lexical-binding: t; -*-

;; Requires
(require 'proced)

;; Keymap
(defvar-keymap proced-enhanced-mode-map
  :doc "Keymap for proced-enhanced minor mode."
  "f"     #'proced-enhanced-filter
  "C-f"   #'proced-enhanced-filter-clear
  "t"     #'proced-enhanced-pstree
  "l"     #'proced-enhanced-lsof
  "K"     #'proced-enhanced-sigkill)

;; Minor mode
(define-minor-mode proced-enhanced-mode
  "Enhanced proced with incremental filter, pstree, lsof, and sigkill."
  :init-value nil
  :lighter " Proced+"
  :keymap proced-enhanced-mode-map)

;; Auto-activate with proced-mode
(add-hook 'proced-mode-hook #'proced-enhanced-mode)

(provide 'proced-enhanced)
```

- [ ] **Step 2: Test by loading and verifying the minor mode activates**

Run in Emacs: `M-x proced`, then verify `proced-enhanced-mode` is active in mode line (shows "Proced+"). Verify keys `f`, `t`, `l`, `K` are bound via `C-h f proced-enhanced-mode`.

- [ ] **Step 3: Commit**

```bash
git add emacs-config/crafts/proced-enhanced.el
git commit -m "feat: proced-enhanced minor mode skeleton with keymap"
```

---

### Task 2: PID Helper Function

**Files:**
- Modify: `/Users/liuxiang/.config/emacs-config/crafts/proced-enhanced.el`

- [ ] **Step 1: Add the PID helper after the keymap definition**

Insert after `proced-enhanced-mode` definition, before the `(provide ...)`:

```elisp
(defun proced-enhanced--get-pids ()
  "Get target PIDs. Marked processes first, fall back to pid at point."
  (let ((marked (proced-marked-processes)))
    (if marked
        (mapcar #'car marked)
      (let ((pid (proced-pid-at-point)))
        (when pid (list pid))))))
```

- [ ] **Step 2: Test in proced buffer**

Open proced, mark a few processes with `m`, then evaluate `(proced-enhanced--get-pids)` — should return the marked PIDs. Unmark all with `U`, evaluate again — should return the PID at point.

- [ ] **Step 3: Commit**

```bash
git add emacs-config/crafts/proced-enhanced.el
git commit -m "feat: add PID helper for marked/current process"
```

---

### Task 3: pstree Command

**Files:**
- Modify: `/Users/liuxiang/.config/emacs-config/crafts/proced-enhanced.el`

Requires: `run-in-ghostel.el` loaded

- [ ] **Step 1: Add require and pstree command**

Add near top of file, after `(require 'proced)`:

```elisp
(require 'run-in-ghostel)
```

Add command before `(provide ...)`:

```elisp
(defun proced-enhanced-pstree ()
  "Show process tree via ~/bin/pstree for marked or current process."
  (interactive nil proced-mode)
  (let* ((pids (proced-enhanced--get-pids))
         (pid-str (mapconcat #'number-to-string pids " ")))
    (if pids
        (lx/run-in-ghostel (format "~/bin/pstree %s" pid-str) "*pstree*")
      (message "No process at point"))))
```

- [ ] **Step 2: Test in proced buffer**

Open proced, move cursor to a process, press `t`. A ghostel buffer should open showing the pstree output for that PID. Mark multiple processes and press `t` — should show tree for all marked PIDs.

- [ ] **Step 3: Commit**

```bash
git add emacs-config/crafts/proced-enhanced.el
git commit -m "feat: add pstree command via ghostel"
```

---

### Task 4: lsof Command

**Files:**
- Modify: `/Users/liuxiang/.config/emacs-config/crafts/proced-enhanced.el`

- [ ] **Step 1: Add lsof command**

Insert before `(provide ...)`:

```elisp
(defun proced-enhanced-lsof ()
  "Run lsof for the process at point."
  (interactive nil proced-mode)
  (let ((pid (proced-pid-at-point)))
    (if pid
        (lx/run-in-ghostel (format "lsof -Pnp %d" pid)
                           (format "*lsof-%d*" pid))
      (message "No process at point"))))
```

- [ ] **Step 2: Test in proced buffer**

Open proced, move cursor to a process, press `l`. A ghostel buffer should open showing lsof output for that PID.

- [ ] **Step 3: Commit**

```bash
git add emacs-config/crafts/proced-enhanced.el
git commit -m "feat: add lsof command via ghostel"
```

---

### Task 5: SIGKILL Command

**Files:**
- Modify: `/Users/liuxiang/.config/emacs-config/crafts/proced-enhanced.el`

- [ ] **Step 1: Add sigkill command**

Insert before `(provide ...)`:

```elisp
(defun proced-enhanced-sigkill ()
  "Send SIGKILL to marked or current process with confirmation."
  (interactive nil proced-mode)
  (let* ((pids (proced-enhanced--get-pids)))
    (if pids
        (when (y-or-n-p (format "Kill %s? "
                                 (mapconcat #'number-to-string pids " ")))
          (dolist (pid pids)
            (signal-process pid 9))
          (proced-update t))
      (message "No process at point"))))
```

- [ ] **Step 2: Test in proced buffer**

Open proced, move cursor to a non-critical process (e.g., a `sleep` command started in a terminal). Press `K`. Confirm prompt should appear. Press `n` — nothing happens. Press `K` again, press `y` — process should be killed and proced refreshes.

- [ ] **Step 3: Commit**

```bash
git add emacs-config/crafts/proced-enhanced.el
git commit -m "feat: add SIGKILL command with confirmation"
```

---

### Task 6: Incremental Filter — Core

**Files:**
- Modify: `/Users/liuxiang/.config/emacs-config/crafts/proced-enhanced.el`

- [ ] **Step 1: Add filter state variables**

Insert after the keymap definition:

```elisp
(defvar-local proced-enhanced-filter-string nil
  "Current filter string for proced-enhanced.")

(defvar-local proced-enhanced--overlays nil
  "List of filter overlays in current proced buffer.")
```

- [ ] **Step 2: Add overlay apply function**

```elisp
(defun proced-enhanced--apply-filter (filter-str)
  "Apply FILTER-STR to proced buffer using overlays."
  (setq proced-enhanced-filter-string
        (if (string= filter-str "") nil filter-str))
  ;; Remove old overlays
  (dolist (ov proced-enhanced--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq proced-enhanced--overlays nil)
  ;; If empty filter, show all
  (when (and filter-str (not (string= filter-str "")))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-text (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))))
          (unless (string-match-p (regexp-quote filter-str) line-text)
            (let ((ov (make-overlay (line-beginning-position)
                                    (1+ (line-end-position)))))
              (overlay-put ov 'invisible t)
              (push ov proced-enhanced--overlays))))
        (forward-line))))))
```

- [ ] **Step 3: Add filter command (isearch-style via read-from-minibuffer)**

```elisp
(defun proced-enhanced-filter ()
  "Incremental filter proced buffer by process name/args."
  (interactive nil proced-mode)
  (let ((minibuffer-local-map (copy-keymap minibuffer-local-map)))
    ;; C-g cancels and clears filter
    (define-key minibuffer-local-map [remap abort-recursive-edit]
      (lambda ()
        "Cancel filter and show all."
        (interactive)
        (proced-enhanced--apply-filter "")
        (abort-recursive-edit)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'post-command-hook
                    (lambda ()
                      (proced-enhanced--apply-filter
                       (minibuffer-contents)))
                    nil t))
      (read-from-minibuffer "Filter: "))))
```

- [ ] **Step 4: Add filter-clear command**

```elisp
(defun proced-enhanced-filter-clear ()
  "Clear the proced-enhanced filter."
  (interactive nil proced-mode)
  (proced-enhanced--apply-filter ""))
```

- [ ] **Step 5: Test in proced buffer**

Open proced. Press `f`, type `emacs` — only emacs-related processes should be visible. Continue typing to narrow further. Press `RET` — filter stays applied. Press `C-f` — all processes visible again.

Test `C-g`: Press `f`, type something, press `C-g` — filter clears, all processes visible.

- [ ] **Step 6: Commit**

```bash
git add emacs-config/crafts/proced-enhanced.el
git commit -m "feat: add incremental filter with overlay-based line hiding"
```

---

### Task 7: Integration Polish

**Files:**
- Modify: `/Users/liuxiang/.config/emacs-config/crafts/proced-enhanced.el`

- [ ] **Step 1: Clear overlays on proced update/revert**

The proced buffer gets erased and redrawn on each update. Stale overlays are automatically cleaned up when their buffer content is deleted, but we should reset the overlay list. Add advice to clean up:

```elisp
(defun proced-enhanced--clear-overlays-on-update (&rest _args)
  "Reset overlay list when proced updates (buffer is erased)."
  (setq proced-enhanced--overlays nil))

(advice-add 'proced-update :before
            #'proced-enhanced--clear-overlays-on-update)
```

- [ ] **Step 2: Test the full workflow**

1. Open proced
2. Press `f`, type a filter, press `RET` — filter active
3. Wait for proced auto-update (or press `g` to manually revert)
4. Verify overlay list is clean (no stale overlays)
5. Filter should be cleared after update (since buffer is regenerated)
6. Test `t`, `l`, `K` still work correctly
7. Press `C-f` to verify clear works

- [ ] **Step 3: Final commit**

```bash
git add emacs-config/crafts/proced-enhanced.el
git commit -m "feat: clean up overlays on proced update"
```
