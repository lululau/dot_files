# proced-enhanced.el Design

## Overview

Enhance Emacs built-in proced with four features: incremental filter, pstree via ghostel, lsof via ghostel, and direct SIGKILL.

File: `/Users/liuxiang/.config/emacs-config/crafts/proced-enhanced.el`

## Structure

A minor mode `proced-enhanced-mode` auto-activated via `proced-mode-hook`. All keybindings scoped to this minor mode map.

### Keybindings

| Key | Command | Function |
|-----|---------|----------|
| `f` | `proced-enhanced-filter` | Incremental filter |
| `C-f` | `proced-enhanced-filter-clear` | Clear filter |
| `t` | `proced-enhanced-pstree` | pstree via ghostel |
| `l` | `proced-enhanced-lsof` | lsof via ghostel |
| `K` | `proced-enhanced-sigkill` | Send SIGKILL with confirmation |

## Feature 1: Incremental Filter

**State:** Buffer-local `proced-enhanced-filter-string`.

**Implementation:** Overlay-based line hiding.

**Flow:**
1. Press `f`, minibuffer shows `Filter: `.
2. On each keystroke in minibuffer: scan every process line in proced buffer, apply `string-match-p` (case-insensitive) against full line text. Non-matching lines get an `invisible` overlay; matching lines have their overlay removed.
3. Backspace to empty string: clear all overlays (show all).
4. `RET`: exit minibuffer, keep current overlay state.
5. `C-g`: cancel, clear all overlays, show all.

**Mechanism:** `read-from-minibuffer` with a `post-command-hook` in the minibuffer to react to each change. Reuse standard minibuffer editing (backspace, kill-word, etc.) rather than building a custom keymap.

**Overlay management:** All overlays stored in a buffer-local list. Clear function iterates and deletes them all. Each overlay covers the full line including newline.

## Feature 2: pstree

**Flow:**
1. Get PID list: marked processes (via `proced-marked-processes`), or fall back to `proced-pid-at-point`.
2. Build command: `~/bin/pstree PID1 PID2 ...` (the script supports multiple PIDs).
3. Call `(lx/run-in-ghostel command "*pstree*")`.

## Feature 3: lsof

**Flow:**
1. Get single PID: `proced-pid-at-point` only (no multi-select).
2. Build command: `lsof -Pnp <PID>`.
3. Call `(lx/run-in-ghostel command (format "*lsof-%d*" pid))`.

## Feature 4: SIGKILL

**Flow:**
1. Get PID list: marked processes or `proced-pid-at-point` (same logic as pstree).
2. Confirm with `y-or-n-p`: `Kill PID1 PID2 ...?`.
3. On `y`: call `(signal-process pid 9)` for each PID.
4. Call `(proced-update t)` to refresh the listing.

## Dependencies

- `run-in-ghostel.el` (lx/run-in-ghostel)
- `~/bin/pstree` script
- Emacs built-in `proced.el`
