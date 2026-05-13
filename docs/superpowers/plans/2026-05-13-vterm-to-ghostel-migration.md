# Vterm → Ghostel 全面迁移 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 将 Emacs 配置中所有 vterm 依赖替换为 ghostel，包括 8 个自制包、配置文件、和 zshrc。

**Architecture:** 按依赖顺序逐包创建 ghostel 版本文件，同时保留原 vterm 文件备份。核心是 API 映射：`vterm-send-string` → `ghostel-send-string`，`vterm-mode` → `ghostel-mode`，`vterm-eval-cmds` → `ghostel-eval-cmds`（格式完全一致）。

**Tech Stack:** Emacs Lisp, ghostel (libghostty-vt), evil-ghostel, zsh

**Ghostel API 确认要点（已从源码验证）：**
- `ghostel-eval-cmds` 格式与 `vterm-eval-cmds` 完全一致：`(("name" function) ...)`
- `ghostel-send-string` 发送原始字节，不自动加 `\n`
- `ghostel-mode` 派生自 `fundamental-mode`，可用 `define-derived-mode` 继承
- 进程变量：`ghostel--process`（等价于 `vterm--process`）
- `ghostel-send-key` 签名：`(ghostel-send-key key-name &optional mods)`，mods 为 `'(control)`、`'(meta)` 等
- Ghostel 无 `vterm-send-return`/`vterm-send-C-j`/`vterm-send-tab` 等便捷函数，需用 `ghostel-send-key` 或 `ghostel-send-string` 替代
- `ghostel--self-insert` 等价于 `vterm--self-insert`
- `ghostel-buffer-name` 默认 `"*ghostel*"`
- `ghostel-kill-buffer-on-exit` 默认 `t`
- `ghostel-send-C-z` 和 `ghostel-send-C-c` 存在

---

## Phase 0: 备份和准备

### Task 0.1: 备份所有 vterm 相关文件

**Files:**
- Create: `~/.config/emacs-config/crafts/backup-vterm/` (directory)

- [ ] **Step 1: 创建备份目录并复制所有 vterm 文件**

```bash
mkdir -p ~/.config/emacs-config/crafts/backup-vterm
cp ~/.config/emacs-config/crafts/run-in-vterm.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/crafts/zsh-vterm.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/crafts/zsh-vterm-ssh.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/crafts/pry-vterm.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/crafts/vterm-maven.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/crafts/vterm-jenkins.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/crafts/vterm-mitmproxy.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/crafts/vterm-rails.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/crafts/vterm-vrl.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/crafts/vterm-prize.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/crafts/vterm-arql.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/funcs/send-to-vterm.el ~/.config/emacs-config/crafts/backup-vterm/
cp ~/.config/emacs-config/package-hooks/vterm-mode.el ~/.config/emacs-config/crafts/backup-vterm/
```

- [ ] **Step 2: 备份 .spacemacs 和 .zshrc**

```bash
cp ~/.config/emacs-config/.spacemacs ~/.config/emacs-config/crafts/backup-vterm/.spacemacs.bak
cp ~/.zshrc ~/.config/emacs-config/crafts/backup-vterm/.zshrc.bak
cp ~/.config/emacs-config/funcs/core.el ~/.config/emacs-config/crafts/backup-vterm/core.el.bak
cp ~/.config/emacs-config/funcs/init.el ~/.config/emacs-config/crafts/backup-vterm/init.el.bak
cp ~/.config/emacs-config/key-bindings/global-set-key.el ~/.config/emacs-config/crafts/backup-vterm/global-set-key.el.bak
```

- [ ] **Step 3: Commit**

```bash
git add -A ~/.config/emacs-config/crafts/backup-vterm/
git commit -m "backup: 备份所有 vterm 相关文件用于 ghostel 迁移"
```

---

## Phase 1: 核心基础设施

### Task 1.1: 创建 run-in-ghostel.el

**Files:**
- Create: `~/.config/emacs-config/crafts/run-in-ghostel.el`
- Reference: `~/.config/emacs-config/crafts/run-in-vterm.el`

这是最核心的文件，其他所有包都依赖它。

- [ ] **Step 1: 创建 run-in-ghostel.el**

将 `run-in-vterm.el` 的全部内容复制并做以下替换：
- `(require 'vterm)` → `(require 'ghostel)`
- `vterm-kill-buffer-on-exit` → `ghostel-kill-buffer-on-exit`
- `vterm-kill-buffer-on-normal-exit` → 自定义变量 `ghostel-kill-buffer-on-normal-exit`
- `vterm-shell` → `ghostel-shell`
- `(vterm buffer-name)` → `(ghostel buffer-name)`
- `lx/run-in-vterm` → `lx/run-in-ghostel`（所有函数名）
- `ssh-zsh-vterm-mode` → `ssh-zsh-ghostel-mode`
- `ssh-zsh-vterm-ssh-options` → `ssh-zsh-ghostel-ssh-options`
- `vterm-mode` → `ghostel-mode`（在 helm buffer list 过滤中）
- `zsh-vterm-mode` → `zsh-ghostel-mode`
- `pry-vterm-mode` → `pry-ghostel-mode`
- `helm-vterm-` → `helm-ghostel-`（所有 helm 变量和函数名前缀）
- `vterm-send-string` → `ghostel-send-string`
- Buffer 名中的 `vterm` → `ghostel`（如 `*vterm-bandwhich*` → `*ghostel-bandwhich*`）

完整文件内容见下方。注意 `lx/run-in-ghostel/rerun` 中使用 `(ghostel buffer-name)` 替代 `(vterm buffer-name)`。

`run-in-ghostel.el` 中的自定义 sentinel 需要：ghostel 有内置 `ghostel--sentinel`，但需要添加 `ghostel-kill-buffer-on-normal-exit` 逻辑。在 `package-hooks/ghostel-mode.el` 中通过 `ghostel-exit-functions` hook 实现此功能，不在 run-in-ghostel 中覆盖 sentinel。

- [ ] **Step 2: 验证文件可加载**

```bash
emacsclient -e '(progn (require (quote ghostel) nil t) (message "ghostel loadable: %s" (featurep (quote ghostel))))'
```

预期输出：`"ghostel loadable: t"`

- [ ] **Step 3: Commit**

```bash
git add ~/.config/emacs-config/crafts/run-in-ghostel.el
git commit -m "feat: 创建 run-in-ghostel.el 核心运行命令库"
```

### Task 1.2: 创建 send-to-ghostel.el

**Files:**
- Create: `~/.config/emacs-config/funcs/send-to-ghostel.el`
- Reference: `~/.config/emacs-config/funcs/send-to-vterm.el`

- [ ] **Step 1: 创建 send-to-ghostel.el**

基于 `send-to-vterm.el`，做以下替换：
- `lx/vterm-send-` → `lx/ghostel-send-`（所有函数名）
- `lx/find-vterm-buffer` → `lx/find-ghostel-buffer`
- `vterm-send-string` → `ghostel-send-string`
- `vterm-send-return` → `(ghostel-send-string "\n")`（内联替换）
- `pry-vterm-mode` → `pry-ghostel-mode`
- `vterm-mode` → `ghostel-mode`（在 `derived-mode-p` 中）

`lx/find-ghostel-buffer` 需要检查 `pry-ghostel-mode` 和 `ghostel-mode`（含派生）。

完整内容：

```elisp
;;;###autoload
(defun lx/ghostel-send-line ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (with-current-buffer ghostel-buffer
            (ghostel-send-string str))))))

;;;###autoload
(defun lx/ghostel-send-line-and-go ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (with-current-buffer ghostel-buffer
            (ghostel-send-string str))
          (select-window (get-buffer-window ghostel-buffer))))))

;;;###autoload
(defun lx/ghostel-send-paragraph ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))) "")))
          (with-current-buffer ghostel-buffer
            (ghostel-send-string str)
            (ghostel-send-string "\n"))))))

;;;###autoload
(defun lx/ghostel-send-paragraph-and-go ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))) "")))
          (with-current-buffer ghostel-buffer
            (ghostel-send-string str)
            (ghostel-send-string "\n"))
          (select-window (get-buffer-window ghostel-buffer))))))

;;;###autoload
(defun lx/ghostel-send-region ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (region-beginning) (region-end)) "")))
          (with-current-buffer ghostel-buffer
            (ghostel-send-string str)
            (ghostel-send-string "\n"))))))

;;;###autoload
(defun lx/ghostel-send-region-and-go ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (buffer-substring (region-beginning) (region-end)) "")))
          (with-current-buffer ghostel-buffer
            (ghostel-send-string str)
            (ghostel-send-string "\n"))
          (select-window (get-buffer-window ghostel-buffer))))))

;;;###autoload
(defun lx/ghostel-send-babel-block ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (lx/get-babel-src) "\n")))
          (with-current-buffer ghostel-buffer
            (ghostel-send-string str)
            (ghostel-send-string "\n"))))))

;;;###autoload
(defun lx/ghostel-send-babel-block-and-go ()
  (interactive)
  (let ((ghostel-buffer (lx/find-ghostel-buffer)))
    (if ghostel-buffer
        (let ((str (concat (lx/get-babel-src) "\n")))
          (with-current-buffer ghostel-buffer
            (ghostel-send-string str)
            (ghostel-send-string "\n"))
          (select-window (get-buffer-window ghostel-buffer))))))

;;;###autoload
(defun lx/find-ghostel-buffer ()
  (let* ((window-buffers (mapcar #'window-buffer (window-list))))
    (or
      (--find (with-current-buffer it (eq major-mode 'pry-ghostel-mode)) window-buffers)
      (--find (with-current-buffer it (derived-mode-p 'ghostel-mode)) window-buffers))))
```

**重要说明：** `vterm-send-string` 有可选的第二参数 `for-shell-p`，但 `ghostel-send-string` 只接受一个参数（字符串）。所有原代码中的 `(vterm-send-string str t)` 改为 `(ghostel-send-string str)`。

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/funcs/send-to-ghostel.el
git commit -m "feat: 创建 send-to-ghostel.el 代码发送库"
```

---

## Phase 2: 终端模式

### Task 2.1: 创建 zsh-ghostel.el

**Files:**
- Create: `~/.config/emacs-config/crafts/zsh-ghostel.el`
- Reference: `~/.config/emacs-config/crafts/zsh-vterm.el`

这是最复杂的文件，包含自定义 keymap、evil 集成、navigation 函数。

关键 API 映射（用于此文件）：

| vterm | ghostel |
|-------|---------|
| `vterm-send-string` | `ghostel-send-string` |
| `vterm-send-C-j` | `(ghostel-send-key "j" '(control))` |
| `vterm-send-tab` | `(ghostel-send-key "tab")` |
| `vterm-send-backspace` | `(ghostel-send-key "backspace")` |
| `vterm-send-space` | `(ghostel-send-key "space")` |
| `vterm-send-C-z` | `ghostel-send-C-z` |
| `vterm-send-key "C" t t` | `(ghostel-send-key "c" '(control meta))` |
| `vterm-send-key "V" t t` | `(ghostel-send-key "v" '(control meta))` |
| `vterm-send-key "N" t t` | `(ghostel-send-key "n" '(control meta))` |
| `vterm-send-key "P" t t` | `(ghostel-send-key "p" '(control meta))` |
| `vterm--self-insert` | `ghostel--self-insert` |
| `vterm-send-escape` | `(ghostel-send-key "escape")` |

- [ ] **Step 1: 创建 zsh-ghostel.el**

从 `zsh-vterm.el` 复制并替换。完整文件见原始代码做以下系统性替换：

1. `(require 'vterm)` → `(require 'ghostel)`
2. 所有函数/变量名 `zsh-vterm` → `zsh-ghostel`
3. `vterm-buffer-name` → `ghostel-buffer-name`
4. `vterm-shell` → `ghostel-shell`
5. `vterm-kill-buffer-on-exit` → `ghostel-kill-buffer-on-exit`
6. `vterm-mode` → `ghostel-mode`（keymap parent、derived-mode）
7. `vterm-send-string` → `ghostel-send-string`
8. `vterm-send-tab` → `(ghostel-send-key "tab")`（内联）
9. `vterm-send-backspace` → `(ghostel-send-key "backspace")`（内联）
10. `vterm-send-C-j` → `(ghostel-send-key "j" '(control))`（内联）
11. `vterm-send-C-z` → `ghostel-send-C-z`
12. `(vterm-send-key "C" t t)` → `(ghostel-send-key "c" '(control meta))`
13. `(vterm-send-key "V" t t)` → `(ghostel-send-key "v" '(control meta))`
14. `(vterm-send-key "N" t t)` → `(ghostel-send-key "n" '(control meta))`
15. `(vterm-send-key "P" t t)` → `(ghostel-send-key "p" '(control meta))`
16. `vterm--self-insert` → `ghostel--self-insert`
17. `vterm-send-space` → `(ghostel-send-key "space")`（在 evil-yank-for-zsh-ghostel 中）
18. `zsh-vterm-mode-map` → `zsh-ghostel-mode-map`
19. `(define-derived-mode zsh-vterm-mode vterm-mode ...)` → `(define-derived-mode zsh-ghostel-mode ghostel-mode ...)`
20. `(provide 'zsh-vterm)` → `(provide 'zsh-ghostel)`

在 `evil-yank-for-zsh-ghostel` 中：
```elisp
(evil-hybrid-state)
(ghostel-send-string "a")
(ghostel-send-key "backspace")
```

在 `zsh-ghostel-accept-copilot-or-send-tab-to-term` 中：
```elisp
(ghostel-send-key "tab")
```

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/crafts/zsh-ghostel.el
git commit -m "feat: 创建 zsh-ghostel.el 主 zsh 终端模式"
```

### Task 2.2: 创建 pry-ghostel.el

**Files:**
- Create: `~/.config/emacs-config/crafts/pry-ghostel.el`
- Reference: `~/.config/emacs-config/crafts/pry-vterm.el`

- [ ] **Step 1: 创建 pry-ghostel.el**

从 `pry-vterm.el` 复制并替换：
1. `(require 'vterm)` → `(require 'ghostel)`
2. 所有函数名 `pry-vterm` → `pry-ghostel`
3. `vterm-buffer-name` → `ghostel-buffer-name`
4. `vterm-shell` → `ghostel-shell`
5. `vterm-kill-buffer-on-exit` → `ghostel-kill-buffer-on-exit`
6. `vterm-mode-map` → `ghostel-mode-map`（keymap parent）
7. `vterm-send-tab` → `(ghostel-send-key "tab")`（内联）
8. `(define-derived-mode pry-vterm-mode vterm-mode ...)` → `(define-derived-mode pry-ghostel-mode ghostel-mode ...)`
9. `vterm-send-string` → `ghostel-send-string`（setplist 中）
10. `(provide 'pry-vterm)` → `(provide 'pry-ghostel)`

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/crafts/pry-ghostel.el
git commit -m "feat: 创建 pry-ghostel.el Ruby REPL 模式"
```

### Task 2.3: 创建 zsh-ghostel-ssh.el

**Files:**
- Create: `~/.config/emacs-config/crafts/zsh-ghostel-ssh.el`
- Reference: `~/.config/emacs-config/crafts/zsh-vterm-ssh.el`

- [ ] **Step 1: 创建 zsh-ghostel-ssh.el**

从 `zsh-vterm-ssh.el` 复制并替换：
1. `(require 'zsh-vterm)` → `(require 'zsh-ghostel)`
2. 所有函数/变量名 `ssh-zsh-vterm` → `ssh-zsh-ghostel`，`zsh-vterm-ssh` → `zsh-ghostel-ssh`
3. `vterm-buffer-name` → `ghostel-buffer-name`
4. `vterm-shell` → `ghostel-shell`
5. `vterm-kill-buffer-on-exit` → `ghostel-kill-buffer-on-exit`
6. `zsh-vterm-mode-map` → `zsh-ghostel-mode-map`（keymap parent）
7. `(define-derived-mode ssh-zsh-vterm-mode zsh-vterm-mode ...)` → `(define-derived-mode ssh-zsh-ghostel-mode zsh-ghostel-mode ...)`
8. `vterm-send-string` → `ghostel-send-string`（setplist 中）
9. 所有 helm source/class 变量名 `helm-zsh-vterm-ssh` → `helm-zsh-ghostel-ssh`
10. 所有 buffer 名 `*zsh-vterm-ssh-*` → `*zsh-ghostel-ssh-*`
11. `(provide 'zsh-vterm-ssh)` → `(provide 'zsh-ghostel-ssh)`

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/crafts/zsh-ghostel-ssh.el
git commit -m "feat: 创建 zsh-ghostel-ssh.el SSH 终端模式"
```

### Task 2.4: 创建 package-hooks/ghostel-mode.el

**Files:**
- Create: `~/.config/emacs-config/package-hooks/ghostel-mode.el`
- Reference: `~/.config/emacs-config/package-hooks/vterm-mode.el`

这是 ghostel 全局 hook 配置，替代 `vterm-mode.el`。

- [ ] **Step 1: 创建 ghostel-mode.el**

从 `vterm-mode.el` 复制并替换。关键变化：

1. `(with-eval-after-load 'vterm` → `(with-eval-after-load 'ghostel`
2. `vterm-kill-buffer-on-normal-exit` → `ghostel-kill-buffer-on-normal-exit`（自定义变量保留）
3. `vterm--sentinel` → 通过 `ghostel-exit-functions` hook 实现 kill-on-normal-exit 逻辑，不覆盖内置 sentinel
4. 所有 `vterm-mode-map` → `ghostel-mode-map`
5. 所有 `vterm-send-*` 函数按以下映射替换：
   - `vterm-send-C-z` → `ghostel-send-C-z`
   - `vterm-send-string` → `ghostel-send-string`
   - `vterm-send-key "x" nil nil t` → `(ghostel-send-key "x" '(control))`
   - `vterm-send-key "c" nil nil t` → `(ghostel-send-key "c" '(control))`
   - `vterm-send-key "g" nil nil t` → `(ghostel-send-key "g" '(control))`
   - `vterm-send-key "e" nil nil t` → `(ghostel-send-key "e" '(control))`
   - `vterm-send-key "k" nil nil t` → `(ghostel-send-key "k" '(control))`
   - `vterm-send-key "s" nil nil t` → `(ghostel-send-key "s" '(control))`
   - `vterm-send-key "f" nil nil t` → `(ghostel-send-key "f" '(control))`
   - `vterm-send-key "b" nil nil t` → `(ghostel-send-key "b" '(control))`
   - `vterm-send-key "x" nil nil nil` → `(ghostel-send-key "x")`
   - `vterm-send-key "k" nil nil nil` → `(ghostel-send-key "k")`
   - `vterm-send-key "b" nil nil nil` → `(ghostel-send-key "b")`
   - `vterm-send-key "s" nil nil nil` → `(ghostel-send-key "s")`
   - `vterm-send-escape` → `(ghostel-send-key "escape")`
   - `vterm-send-M-p` → `(ghostel-send-key "p" '(meta))`
   - `vterm-send-C-h` → `(ghostel-send-key "h" '(control))`
   - `vterm-send-C-u` → `(ghostel-send-key "u" '(control))`
   - `vterm-send-space` → `(ghostel-send-key "space")`
   - `vterm-send-backspace` → `(ghostel-send-key "backspace")`
6. `vterm--process` → `ghostel--process`（process-send-string 中）
7. `vterm-enter-hybrid-state-decently` 中 `vterm-send-space` + `vterm-send-backspace` → `(ghostel-send-key "space")` + `(ghostel-send-key "backspace")`
8. `zsh-vterm-last-buffer` → `zsh-ghostel-last-buffer`
9. `lx/run-in-vterm/rerun` → `lx/run-in-ghostel/rerun`
10. `vterm--self-insert` → `ghostel--self-insert`
11. `shell-pop-internal-mode "zsh-vterm"` → `"zsh-ghostel"`
12. `lx/get-remote-buffer-host` → 不变（此函数在 core.el 中更新）
13. `ssh-zsh-vterm-mode` → `ssh-zsh-ghostel-mode`（在 dnd 函数中）
14. `vterm-dnd-*` → `ghostel-dnd-*`（所有函数名）
15. `vterm-mode` → `ghostel-mode`（在 derived-mode-p 中）
16. `rvm-activate-corresponding-ruby` → 保留不变

sentinel 替换策略：删除 `vterm--sentinel` 的自定义定义，改用 `ghostel-exit-functions` hook：
```elisp
(defcustom ghostel-kill-buffer-on-normal-exit t
  "Kill buffer on normal exit (finished status)."
  :type 'boolean
  :group 'ghostel)

(add-hook 'ghostel-exit-functions
  (lambda (buf event)
    (when (and ghostel-kill-buffer-on-normal-exit (buffer-live-p buf)
               (string= "finished\n" event))
      (kill-buffer buf))))
```

最后一段 spacemacs hook 替换：
```elisp
(spacemacs|use-package-add-hook ghostel
  :post-config
  (define-key ghostel-mode-map (kbd "M-p") #(lambda () (interactive) (ghostel-send-key "p" '(meta))))
  (define-key ghostel-mode-map (kbd "M-/") 'current-buffer-completion))
```

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/package-hooks/ghostel-mode.el
git commit -m "feat: 创建 ghostel-mode.el 全局 hook 配置"
```

---

## Phase 3: 领域专用包

所有领域专用包遵循相同的迁移模式：
1. `(require 'run-in-vterm)` → `(require 'run-in-ghostel)`
2. `vterm-kill-buffer-on-exit` → `ghostel-kill-buffer-on-exit`
3. `lx/run-in-vterm` → `lx/run-in-ghostel`
4. `vterm-mode` → `ghostel-mode`（在 buffer 过滤中）
5. 所有 buffer 名中 `vterm` → `ghostel`
6. 所有函数/变量名中 `vterm-` → `ghostel-` 或保持原 `helm-` 前缀模式
7. `(provide 'vterm-*)` → `(provide 'ghostel-*)`

### Task 3.1: ghostel-maven.el

**Files:**
- Create: `~/.config/emacs-config/crafts/ghostel-maven.el`

- [ ] **Step 1: 创建文件**

从 `vterm-maven.el` 复制并替换：
- `(require 'run-in-vterm)` → `(require 'run-in-ghostel)`
- `vterm-maven-dir` → `ghostel-maven-dir`
- `vterm-kill-buffer-on-exit` → `ghostel-kill-buffer-on-exit`
- `lx/run-in-vterm` → `lx/run-in-ghostel`
- 所有 `vterm-mode` → `ghostel-mode`
- 所有 buffer 名中 `vterm-maven` → `ghostel-maven`
- 所有 helm 变量名 `helm-vterm-maven` → `helm-ghostel-maven`
- `(provide 'vterm-maven)` → `(provide 'ghostel-maven)`

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/crafts/ghostel-maven.el
git commit -m "feat: 创建 ghostel-maven.el"
```

### Task 3.2: ghostel-jenkins.el

**Files:**
- Create: `~/.config/emacs-config/crafts/ghostel-jenkins.el`

- [ ] **Step 1: 创建文件**

同上模式，`vterm-jenkins` → `ghostel-jenkins`。Buffer 名 `*vterm-jk-*` → `*ghostel-jk-*`。

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/crafts/ghostel-jenkins.el
git commit -m "feat: 创建 ghostel-jenkins.el"
```

### Task 3.3: ghostel-mitmproxy.el

**Files:**
- Create: `~/.config/emacs-config/crafts/ghostel-mitmproxy.el`

- [ ] **Step 1: 创建文件**

从 `vterm-mitmproxy.el` 复制。这个文件最简单，只有 `(require 'run-in-ghostel)` + 函数名替换。

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/crafts/ghostel-mitmproxy.el
git commit -m "feat: 创建 ghostel-mitmproxy.el"
```

### Task 3.4: ghostel-rails.el

**Files:**
- Create: `~/.config/emacs-config/crafts/ghostel-rails.el`

- [ ] **Step 1: 创建文件**

从 `vterm-rails.el` 复制。函数名 `vterm-rails/` → `ghostel-rails/`。

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/crafts/ghostel-rails.el
git commit -m "feat: 创建 ghostel-rails.el"
```

### Task 3.5: ghostel-vrl.el

**Files:**
- Create: `~/.config/emacs-config/crafts/ghostel-vrl.el`

- [ ] **Step 1: 创建文件**

同上模式。`helm-vterm-vrl` → `helm-ghostel-vrl`。Buffer 名 `*vterm-vrl-*` → `*ghostel-vrl-*`。

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/crafts/ghostel-vrl.el
git commit -m "feat: 创建 ghostel-vrl.el"
```

### Task 3.6: ghostel-prize.el

**Files:**
- Create: `~/.config/emacs-config/crafts/ghostel-prize.el`

- [ ] **Step 1: 创建文件**

同上模式。`helm-vterm-prize` → `helm-ghostel-prize`。Buffer 名 `*vterm-prize-*` → `*ghostel-prize-*`。

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/crafts/ghostel-prize.el
git commit -m "feat: 创建 ghostel-prize.el"
```

### Task 3.7: ghostel-arql.el（原 vterm-arql.el）

**Files:**
- Create: `~/.config/emacs-config/crafts/ghostel-arql.el`

- [ ] **Step 1: 创建文件**

特殊：此文件依赖 `pry-vterm`，需改为 `(require 'pry-ghostel)`。
- `lx/run-in-pry-vterm` → `lx/run-in-pry-ghostel`
- `pry-vterm-mode` → `pry-ghostel-mode`
- 所有 helm 变量名 `helm-pry-vterm-arql` → `helm-pry-ghostel-arql`

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/crafts/ghostel-arql.el
git commit -m "feat: 创建 ghostel-arql.el"
```

---

## Phase 4: 配置层更新

### Task 4.1: 更新 funcs/core.el

**Files:**
- Modify: `~/.config/emacs-config/funcs/core.el:56-63`

- [ ] **Step 1: 替换 ssh-zsh-vterm-mode 为 ssh-zsh-ghostel-mode**

将：
```elisp
(defun lx/is-remote-buffer ()
  (or (eq 'ssh-zsh-vterm-mode major-mode)
      (string-prefix-p "/scp:" default-directory)
      (string-prefix-p "/ssh:" default-directory)))

(defun lx/get-remote-buffer-host ()
  (cond ((eq 'ssh-zsh-vterm-mode major-mode) (plist-get ssh-zsh-vterm-ssh-options :host))
        ((or (string-prefix-p "/scp:" default-directory) (string-prefix-p "/ssh:" default-directory))
         (seq--elt-safe (split-string default-directory ":") 1))))
```

替换为：
```elisp
(defun lx/is-remote-buffer ()
  (or (eq 'ssh-zsh-ghostel-mode major-mode)
      (string-prefix-p "/scp:" default-directory)
      (string-prefix-p "/ssh:" default-directory)))

(defun lx/get-remote-buffer-host ()
  (cond ((eq 'ssh-zsh-ghostel-mode major-mode) (plist-get ssh-zsh-ghostel-ssh-options :host))
        ((or (string-prefix-p "/scp:" default-directory) (string-prefix-p "/ssh:" default-directory))
         (seq--elt-safe (split-string default-directory ":") 1))))
```

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/funcs/core.el
git commit -m "feat: 更新 core.el 中远程 buffer 检测为 ghostel 模式"
```

### Task 4.2: 更新 funcs/init.el

**Files:**
- Modify: `~/.config/emacs-config/funcs/init.el:604-625` (autoloads)
- Modify: `~/.config/emacs-config/funcs/init.el:834-849` (minor mode)

- [ ] **Step 1: 替换 autoloads**

将所有 `send-to-vterm` autoload 替换为 `send-to-ghostel`：
```elisp
(autoload 'lx/ghostel-send-line "send-to-ghostel" nil t)
(autoload 'lx/ghostel-send-line-and-go "send-to-ghostel" nil t)
(autoload 'lx/ghostel-send-paragraph "send-to-ghostel" nil t)
(autoload 'lx/ghostel-send-paragraph-and-go "send-to-ghostel" nil t)
(autoload 'lx/ghostel-send-region "send-to-ghostel" nil t)
(autoload 'lx/ghostel-send-region-and-go "send-to-ghostel" nil t)
(autoload 'lx/ghostel-send-babel-block "send-to-ghostel" nil t)
(autoload 'lx/ghostel-send-babel-block-and-go "send-to-ghostel" nil t)
(autoload 'lx/find-ghostel-buffer "send-to-ghostel")

(register-definition-prefixes "send-to-ghostel" '("send-to-ghostel-mode"))
```

- [ ] **Step 2: 替换 minor mode 定义**

将：
```elisp
(defvar send-to-vterm-mode-keymap (make-sparse-keymap))

(define-minor-mode send-to-vterm-mode
  "Send to vterm mode"
  :lighter " Send to vterm"
  :keymap send-to-vterm-mode-keymap
  :group 'send-to-vterm-mode

  (spacemacs/set-leader-keys-for-minor-mode 'send-to-vterm-mode
    "Sl" 'lx/vterm-send-line
    ...
    "SB" 'lx/vterm-send-babel-block-and-go))
```

替换为：
```elisp
(defvar send-to-ghostel-mode-keymap (make-sparse-keymap))

(define-minor-mode send-to-ghostel-mode
  "Send to ghostel mode"
  :lighter " Send to ghostel"
  :keymap send-to-ghostel-mode-keymap
  :group 'send-to-ghostel-mode

  (spacemacs/set-leader-keys-for-minor-mode 'send-to-ghostel-mode
    "Sl" 'lx/ghostel-send-line
    "SL" 'lx/ghostel-send-line-and-go
    "Sr" 'lx/ghostel-send-region
    "SR" 'lx/ghostel-send-region-and-go
    "Sp" 'lx/ghostel-send-paragraph
    "SP" 'lx/ghostel-send-paragraph-and-go
    "Sb" 'lx/ghostel-send-babel-block
    "SB" 'lx/ghostel-send-babel-block-and-go))
```

- [ ] **Step 3: Commit**

```bash
git add ~/.config/emacs-config/funcs/init.el
git commit -m "feat: 更新 autoload 和 minor mode 为 ghostel 版本"
```

### Task 4.3: 更新 key-bindings/global-set-key.el

**Files:**
- Modify: `~/.config/emacs-config/key-bindings/global-set-key.el`

- [ ] **Step 1: 系统性替换所有 vterm 引用**

需要替换的内容：
- `lx/run-in-vterm` → `lx/run-in-ghostel`
- `lx/run-in-zsh-vterm` → `lx/run-in-zsh-ghostel`
- `lx/run-ssh-in-zsh-vterm` → `lx/run-ssh-in-zsh-ghostel`
- `lx/run-in-pry-vterm` → `lx/run-in-pry-ghostel`
- `helm-zsh-vterm-ssh-run` → `helm-zsh-ghostel-ssh-run`
- `helm-zsh-vterm-ssh` → `helm-zsh-ghostel-ssh`
- `helm-pry-vterm-arql` → `helm-pry-ghostel-arql`
- `helm-vterm-vrl-run-auto-function` → `helm-ghostel-vrl-run-auto-function`
- `helm-vterm-vrl` → `helm-ghostel-vrl`
- `helm-vterm-prize` → `helm-ghostel-prize`
- `helm-vterm-maven-*` → `helm-ghostel-maven-*`
- `vterm-maven-*` → `ghostel-maven-*`
- `helm-vterm-jenkins` → `helm-ghostel-jenkins`
- `vterm-mitmproxy-*` → `ghostel-mitmproxy-*`
- `helm-vterm-buffers` → `helm-ghostel-buffers`
- `(vterm "/bin/zsh")` → `(ghostel)`（在 `s-M-'` 绑定中）
- `vterm-kill-buffer-on-exit` → `ghostel-kill-buffer-on-exit`
- Buffer 名中 `vterm` → `ghostel`（如 `*vterm-bandwhich*` → `*ghostel-bandwhich*`）
- `zsh-vterm-mode` → `zsh-ghostel-mode`（在 `s-"` 绑定中）
- `zsh-vterm-ssh-%s` → `zsh-ghostel-ssh-%s`（buffer 名中）
- `*tmux-*` buffer 名不变（这些不是 vterm buffer）

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/key-bindings/global-set-key.el
git commit -m "feat: 更新全局快捷键绑定指向 ghostel 函数"
```

### Task 4.4: 更新 .spacemacs

**Files:**
- Modify: `~/.config/emacs-config/.spacemacs`

- [ ] **Step 1: 替换配置变量**

在 `.spacemacs` 中做以下替换：

**Line ~99:**
```elisp
;; 旧
(setq claude-code-ide-terminal-backend 'vterm)
;; 新
(setq claude-code-ide-terminal-backend 'ghostel)
```

**Lines ~101-112:**
```elisp
;; 旧
(setq vterm-eval-cmds '(("find-file" find-file) ...))
;; 新
(setq ghostel-eval-cmds '(("find-file" find-file)
                          ("message" message)
                          ("download" lx/run-in-ghostel/download)
                          ("upload" lx/run-in-ghostel/upload)
                          ("ghostel-clear-scrollback" ghostel-clear-scrollback)
                          ("lx/run-in-ghostel/set-green-box-cursor" lx/run-in-ghostel/set-green-box-cursor)
                          ("lx/run-in-ghostel/set-blue-bar-cursor" lx/run-in-ghostel/set-blue-bar-cursor)
                          ("update-pwd" lx/run-in-ghostel/set-default-directory)
                          ("find-remote-file" lx/run-in-ghostel/find-remote-file)
                          ("sudo-find-remote-file" lx/run-in-ghostel/sudo-find-remote-file)
                          ("save-zsh-history" lx/run-in-ghostel/save-history-to-ghostel)
                          ("update-zsh-history-outcome" lx/run-in-ghostel/update-history-outcome-to-ghostel)))
```

**Line ~144:**
```elisp
;; 旧
(spacemacs-evil :variable spacemacs-evil-collection-allowed-list '(eww dired quickrun zsh-vterm pry-vterm))
;; 新
(spacemacs-evil :variable spacemacs-evil-collection-allowed-list '(eww dired quickrun zsh-ghostel pry-ghostel))
```

**Lines ~191-195:**
```elisp
;; 旧
(shell :variables
       shell-default-height 38
       shell-default-position 'bottom
       shell-default-shell 'vterm
       shell-default-term-shell ,lx/default-shell)
;; 新
(shell :variables
       shell-default-height 38
       shell-default-position 'bottom
       shell-default-shell 'ghostel
       shell-default-term-shell ,lx/default-shell)
```

**Line ~774:**
```elisp
;; 旧
(make-shell-pop-command "zsh-vterm" zsh-vterm)
;; 新
(make-shell-pop-command "zsh-ghostel" zsh-ghostel)
```

**Lines ~784, 787, 789:**
```elisp
;; 旧
(add-hook 'prog-mode-hook 'send-to-vterm-mode)
(add-hook 'text-mode-hook 'send-to-vterm-mode)
(add-hook 'fundamental-mode-hook 'send-to-vterm-mode)
;; 新
(add-hook 'prog-mode-hook 'send-to-ghostel-mode)
(add-hook 'text-mode-hook 'send-to-ghostel-mode)
(add-hook 'fundamental-mode-hook 'send-to-ghostel-mode)
```

**Lines ~1239-1240 (custom-set-variables):**
```elisp
;; 旧
'(vterm-max-scrollback 10000)
'(vterm-keymap-exceptions (quote ("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-y" "M-y" "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9" "M-0" "M-\\" "M-h" "M-l" "M-k" "M-:")))
;; 新
'(ghostel-max-scrollback 10000)
'(ghostel-keymap-exceptions (quote ("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-y" "M-y" "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9" "M-0" "M-\\" "M-h" "M-l" "M-k" "M-:")))
```

**Line ~1279:**
```elisp
;; 旧
'(docker-run-async-with-buffer-function (quote docker-run-async-with-buffer-vterm))
;; 新 - 需要自制或暂时注释掉
'(docker-run-async-with-buffer-function (quote docker-run-async-with-buffer-ghostel))
```

注：`docker-run-async-with-buffer-ghostel` 需要在 ghostel-mode.el 中定义。如果 docker 包不支持自定义后端，可能需要其他方式处理。此条可以在实现时确认 docker 包的扩展机制后决定。

- [ ] **Step 2: Commit**

```bash
git add ~/.config/emacs-config/.spacemacs
git commit -m "feat: 更新 .spacemacs 配置从 vterm 切换到 ghostel"
```

---

## Phase 5: Shell 侧迁移

### Task 5.1: 更新 .zshrc

**Files:**
- Modify: `~/.zshrc`

- [ ] **Step 1: 删除 vterm_printf 和 vterm_cmd 函数**

删除 `.zshrc` 中以下函数定义（约 lines 349-370）：
- `vterm_printf()` 函数
- `vterm_cmd()` 函数

Ghostel 的 shell integration 会自动注入这些功能。

- [ ] **Step 2: 删除 vterm_set_directory 和相关 hooks**

删除以下内容（约 lines 372-374, 414-416）：
- `vterm_set_directory()` 函数
- `add-zsh-hook -Uz chpwd (){ vterm_set_directory }`
- `add-zsh-hook -Uz precmd (){ vterm_set_directory }`

Ghostel 通过 OSC 7 自动跟踪目录。

- [ ] **Step 3: 适配 me() 和 sme() 函数**

将：
```zsh
me() {
  ... (existing logic)
  vterm_cmd find-remote-file "$file" "$host_name"
}
sme() {
  ... (existing logic)
  vterm_cmd sudo-find-remote-file "$file" "$host_name"
}
```

替换为：
```zsh
me() {
  local file="$1"
  if [ -z "$file" ]; then
    file="$PWD"
  fi
  local host_name=$HOST
  printf "\e]51;E\"find-remote-file\" \"%s\" \"%s\"\e\\" "$file" "$host_name"
}
sme() {
  local file="$1"
  if [ -z "$file" ]; then
    file="$PWD"
  fi
  local host_name=$HOST
  printf "\e]51;E\"sudo-find-remote-file\" \"%s\" \"%s\"\e\\" "$file" "$host_name"
}
```

- [ ] **Step 4: 适配历史记录同步**

将 `HISTDB_VTERM_SESSION` → `HISTDB_GHOSTEL_SESSION`：
```zsh
# 旧
HISTDB_VTERM_SESSION=$RANDOM
function save_history_to_vterm() { ... vterm_cmd save-zsh-history ... }
function update_history_outcome_to_vterm() { ... vterm_cmd update-zsh-history-outcome ... }
add-zsh-hook precmd update_history_outcome_to_vterm
zshaddhistory_functions+=(save_history_to_vterm)

# 新
HISTDB_GHOSTEL_SESSION=$RANDOM
function save_history_to_ghostel() {
  local cmd="${1[0, -2]}"
  for boring in "${_BORING_COMMANDS[@]}"; do
    [[ "$cmd" == "$boring"* ]] && return 0
  done
  local pwd=$PWD
  local started=$(date +%s)
  local host_name=$HOST
  printf "\e]51;E\"save-zsh-history\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\"\e\\" "$HISTDB_GHOSTEL_SESSION" "$host_name" "$cmd" "$pwd" "$started"
  return 0
}
function update_history_outcome_to_ghostel() {
  local retval=$?
  local finished=$(date +%s)
  local host_name=$HOST
  printf "\e]51;E\"update-zsh-history-outcome\" \"%s\" \"%s\" \"%s\" \"%s\"\e\\" "$HISTDB_GHOSTEL_SESSION" "$host_name" "$retval" "$finished"
}
add-zsh-hook precmd update_history_outcome_to_ghostel
zshaddhistory_functions+=(save_history_to_ghostel)
```

- [ ] **Step 5: 适配 download/upload 函数**

将：
```zsh
function download() { ... vterm_cmd download "$file" }
function upload() { ... vterm_cmd upload "$PWD" }
```

替换为：
```zsh
function download() {
  local file=$1
  if [ "$file[1]" != "/" ]; then
    file="$PWD/$file"
  fi
  printf "\e]51;E\"download\" \"%s\"\e\\" "$file"
}
function upload() {
  printf "\e]51;E\"upload\" \"%s\"\e\\" "$PWD"
}
```

- [ ] **Step 6: 删除已注释的光标设置代码**

删除（约 lines 402-412）已注释的 vicmd/viins 光标设置代码。

- [ ] **Step 7: Commit**

```bash
git add ~/.zshrc
git commit -m "feat: 更新 .zshrc 从 vterm_cmd 切换到 ghostel OSC 51"
```

---

## Phase 6: 删除旧文件和验证

### Task 6.1: 删除旧 vterm 文件

**Files:**
- Delete: `~/.config/emacs-config/crafts/run-in-vterm.el`
- Delete: `~/.config/emacs-config/crafts/zsh-vterm.el`
- Delete: `~/.config/emacs-config/crafts/zsh-vterm-ssh.el`
- Delete: `~/.config/emacs-config/crafts/pry-vterm.el`
- Delete: `~/.config/emacs-config/crafts/vterm-maven.el`
- Delete: `~/.config/emacs-config/crafts/vterm-jenkins.el`
- Delete: `~/.config/emacs-config/crafts/vterm-mitmproxy.el`
- Delete: `~/.config/emacs-config/crafts/vterm-rails.el`
- Delete: `~/.config/emacs-config/crafts/vterm-vrl.el`
- Delete: `~/.config/emacs-config/crafts/vterm-prize.el`
- Delete: `~/.config/emacs-config/crafts/vterm-arql.el`
- Delete: `~/.config/emacs-config/funcs/send-to-vterm.el`
- Delete: `~/.config/emacs-config/package-hooks/vterm-mode.el`

- [ ] **Step 1: 删除所有旧文件**

```bash
rm ~/.config/emacs-config/crafts/run-in-vterm.el
rm ~/.config/emacs-config/crafts/zsh-vterm.el
rm ~/.config/emacs-config/crafts/zsh-vterm-ssh.el
rm ~/.config/emacs-config/crafts/pry-vterm.el
rm ~/.config/emacs-config/crafts/vterm-maven.el
rm ~/.config/emacs-config/crafts/vterm-jenkins.el
rm ~/.config/emacs-config/crafts/vterm-mitmproxy.el
rm ~/.config/emacs-config/crafts/vterm-rails.el
rm ~/.config/emacs-config/crafts/vterm-vrl.el
rm ~/.config/emacs-config/crafts/vterm-prize.el
rm ~/.config/emacs-config/crafts/vterm-arql.el
rm ~/.config/emacs-config/funcs/send-to-vterm.el
rm ~/.config/emacs-config/package-hooks/vterm-mode.el
```

- [ ] **Step 2: Commit**

```bash
git add -A ~/.config/emacs-config/
git commit -m "chore: 删除所有旧 vterm 文件（备份在 backup-vterm/ 目录中）"
```

### Task 6.2: 全面验证

- [ ] **Step 1: 验证 ghostel 可加载**

```bash
emacsclient -e '(progn (require (quote ghostel)) (message "ghostel loaded: %s" (featurep (quote ghostel))))'
```

预期：`"ghostel loaded: t"`

- [ ] **Step 2: 验证 ghostel buffer 可创建**

```bash
emacsclient -e '(progn (ghostel) (message "ghostel buffer created: %s" (buffer-name (current-buffer))))'
```

预期：`"ghostel buffer created: *ghostel*"`

- [ ] **Step 3: 验证自定义包可加载**

```bash
emacsclient -e '(progn (require (quote run-in-ghostel)) (require (quote zsh-ghostel)) (require (quote pry-ghostel)) (require (quote zsh-ghostel-ssh)) (message "All custom packages loaded"))'
```

- [ ] **Step 4: 验证 send-to-ghostel-mode**

```bash
emacsclient -e '(progn (send-to-ghostel-mode 1) (message "send-to-ghostel-mode active: %s" send-to-ghostel-mode))'
```

- [ ] **Step 5: 手动功能测试**

在 Emacs 中逐一测试：
1. `s-:` 或 `s-'` 打开 zsh-ghostel 终端
2. `s-r j` 打开 jshell
3. `s-r s-s` 打开 SSH 列表
4. 从代码 buffer 中 `SPC S l` 发送行到终端
5. `s-"` 切换 shell-pop
6. 在 ghostel 中测试 `s-C`/`s-V` 导航
7. 测试 download/upload 函数
8. 测试 me()/sme() 函数

- [ ] **Step 6: 搜索残留 vterm 引用**

```bash
grep -rn "vterm" ~/.config/emacs-config/ --include="*.el" | grep -v "backup-vterm" | grep -v ".bak"
```

预期：0 结果（或只有注释/文档中的引用）

```bash
grep -rn "vterm" ~/.zshrc | grep -v "#"
```

预期：0 结果

---

## 回滚方案

如果迁移出现问题：

```bash
# 恢复备份文件
cp ~/.config/emacs-config/crafts/backup-vterm/*.el ~/.config/emacs-config/crafts/
cp ~/.config/emacs-config/crafts/backup-vterm/send-to-vterm.el ~/.config/emacs-config/funcs/
cp ~/.config/emacs-config/crafts/backup-vterm/vterm-mode.el ~/.config/emacs-config/package-hooks/
cp ~/.config/emacs-config/crafts/backup-vterm/.spacemacs.bak ~/.config/emacs-config/.spacemacs
cp ~/.config/emacs-config/crafts/backup-vterm/.zshrc.bak ~/.zshrc
cp ~/.config/emacs-config/crafts/backup-vterm/core.el.bak ~/.config/emacs-config/funcs/core.el
cp ~/.config/emacs-config/crafts/backup-vterm/init.el.bak ~/.config/emacs-config/funcs/init.el
cp ~/.config/emacs-config/crafts/backup-vterm/global-set-key.el.bak ~/.config/emacs-config/key-bindings/global-set-key.el

# 删除 ghostel 文件
rm ~/.config/emacs-config/crafts/run-in-ghostel.el
rm ~/.config/emacs-config/crafts/zsh-ghostel.el
rm ~/.config/emacs-config/crafts/zsh-ghostel-ssh.el
rm ~/.config/emacs-config/crafts/pry-ghostel.el
rm ~/.config/emacs-config/crafts/ghostel-*.el
rm ~/.config/emacs-config/funcs/send-to-ghostel.el
rm ~/.config/emacs-config/package-hooks/ghostel-mode.el
```
