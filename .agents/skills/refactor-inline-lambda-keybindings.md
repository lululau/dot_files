---
name: refactor-inline-lambda-keybindings
description: Refactor inline lambda functions in Emacs keybinding files into named functions with autoload support. Trigger when user wants to clean up global-set-key or similar binding files by extracting anonymous lambdas.
---

Refactor inline lambda functions embedded in Emacs keybinding definitions (e.g., `global-set-key`) into standalone named functions with autoload support.

## When to use

- User asks to refactor, clean up, or extract inline lambda functions from keybinding files
- User points out a keybinding file with many `#'(lambda () ...)` forms and wants them extracted
- User wants to group related keybinding lambdas into a dedicated functions file

## Input

Ask the user (or infer from context):
1. **Source keybinding file** — which `.el` file contains the inline lambdas (e.g., `global-set-key.el`)
2. **Target functions file** — where to extract the named functions (e.g., `term-funcs.el`)
3. **Theme/filter** — which lambdas to extract: all of them, or only those matching a theme (e.g., "ghostel-related", "term-related", "project-related")
4. **Init/autoload file** — the file that registers autoloads (e.g., `funcs/init.el`)

## Steps

### 1. Scan the keybinding file for inline lambdas

Read the source keybinding file. Identify all `(global-set-key ... #'(lambda ...))` or `(global-set-key ... '(lambda ...))` forms. For each lambda, note:

- The key sequence it's bound to
- Whether it calls functions from a specific domain (e.g., `lx/run-in-ghostel`, `helm-zsh-ghostel-ssh-run`)
- Its complexity (single expression vs multi-line with let/if/progn)
- Whether it accepts interactive args (`(interactive "P")`, etc.)

### 2. Filter by theme

If the user specified a theme (e.g., "ghostel-related"), only extract lambdas that call functions matching that theme. Leave unrelated lambdas untouched.

### 3. Generate named functions

For each lambda to extract:

**Naming convention:** Use a descriptive name based on what the function does:
- Toggle/show a UI element: `lx/toggle-<thing>` or `lx/toggle-<thing>-popup`
- Run a CLI tool: `lx/run-<tool-name>`
- SSH to a host: `lx/ssh-to-<host-description>`
- Run a categorized service: `lx/<category>-<descriptor>`

**Function template:**

```elisp
;;;###autoload
(defun lx/<name> ()
  "One-line docstring describing what this does"
  (interactive)
  <body from the lambda>)
```

For lambdas with interactive args:

```elisp
;;;###autoload
(defun lx/<name> (&optional arg)
  "One-line docstring"
  (interactive "P")
  <body from the lambda>)
```

Key rules:
- Every function must have `;;;###autoload` before `defun`
- Every function must have `(interactive)` or `(interactive "...")` 
- Copy the lambda body verbatim — do not change logic, only wrap in `defun`
- The docstring should be a concise one-liner describing the action

### 4. Write the target functions file

Append the new functions to the target `.el` file (create it if needed). Functions should be grouped logically:
- Toggle/window functions first
- Tool runner functions (grouped by category)
- SSH/VRL shortcut functions
- Git/command functions last

### 5. Update the keybinding file

Replace each inline lambda with a function reference:

```
;; Before:
(global-set-key (kbd "s-r j") #'(lambda () (interactive) (lx/run-in-ghostel "..." "*jshell*")))

;; After:
(global-set-key (kbd "s-r j") #'lx/run-jshell)
```

For multi-line lambdas, replace the entire block:

```
;; Before:
(global-set-key (kbd "s-r s-r gs") #'(lambda () (interactive) (let* ((ghostel-kill-buffer-on-exit nil)
                                                                     (root (projectile-project-root))
                                                                     ...)
                                                                (lx/run-in-ghostel "git multi-status" ...))))

;; After:
(global-set-key (kbd "s-r s-r gs") #'lx/run-git-multi-status)
```

### 6. Add autoload declarations

In the autoload/init file (e.g., `funcs/init.el`), add a section for the new functions file following the existing pattern:

```elisp

;;;### (autoloads nil "<filename>" "<filename>.el" (0 0 0 0))
;;; Generated autoloads from <filename>.el

(autoload 'lx/<func-1> "<filename>" nil t)

(autoload 'lx/<func-2> "<filename>" nil t)

...one per function...

;;;***
```

Insert alphabetically among existing autoload sections.

### 7. Verify

After all edits:
- Run `git diff --stat` to confirm the changes (keybinding file should lose lines, functions file should gain lines)
- Spot-check that no lambda body was altered during extraction
- Confirm all keybinding lines that were changed still have a valid function reference

## Example

**Before** (`global-set-key.el`, 90 lines with lambdas):
```elisp
(global-set-key (kbd "s-r j") #'(lambda () (interactive) (lx/run-in-ghostel "/path/to/jshell ..." "*jshell*")))
(global-set-key (kbd "s-r 0") #'(lambda () (interactive) (helm-zsh-ghostel-ssh-run "lx.sd")))
```

**After** (`global-set-key.el`, compact):
```elisp
(global-set-key (kbd "s-r j") #'lx/run-jshell)
(global-set-key (kbd "s-r 0") #'lx/ssh-to-lx-sd)
```

**After** (`term-funcs.el`, new file):
```elisp
;;;###autoload
(defun lx/run-jshell ()
  "Run jshell in ghostel"
  (interactive)
  (lx/run-in-ghostel "/path/to/jshell ..." "*jshell*"))

;;;###autoload
(defun lx/ssh-to-lx-sd ()
  "SSH to lx.sd via ghostel"
  (interactive)
  (helm-zsh-ghostel-ssh-run "lx.sd"))
```

**After** (`init.el`, autoload section added):
```elisp
;;;### (autoloads nil "term-funcs" "term-funcs.el" (0 0 0 0))
;;; Generated autoloads from term-funcs.el

(autoload 'lx/run-jshell "term-funcs" nil t)
(autoload 'lx/ssh-to-lx-sd "term-funcs" nil t)
;;;***
```
