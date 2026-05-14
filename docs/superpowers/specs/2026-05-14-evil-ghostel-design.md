# evil-ghostel.el 设计文档

## 概述

为 ghostel（基于 libghostty-vt 的终端模拟器）提供 Evil 模式集成，替代 evil-collection-vterm。采用 insert/normal 状态模型，通过发送终端序列实现 normal 状态下的操作符。

## 文件位置

`/Users/liuxiang/config-dev/emacs-config/package-hooks/evil-ghostel.el`

## 状态模型

- `ghostel-mode` 初始状态 = `insert`（与 evil-collection-vterm 一致）
- Insert 状态：semi-char 模式，键直通终端
- Normal 状态：evil 操作符通过终端序列操作输入区域
- ESC：insert → normal
- i/a/I/A：normal → insert

## 核心删除策略

**「定位 + 按键」模式**：

1. 计算 point 与 terminal cursor 的偏移量
2. 发送 Left/Right 箭头定位 terminal cursor
3. 发送 Delete/Backspace 删除字符

特殊场景：
- `D`：定位 + Ctrl+k（readline kill-to-eol）
- `dd` / `S` / `cc`：Ctrl+a + Ctrl+k（清空整行）
- `x`：发送 Delete
- `X`：发送 Backspace
- `r`：定位 + Delete + 替换字符 + Left

## 核心 API 映射

| evil-collection-vterm | evil-ghostel | 说明 |
|---|---|---|
| `vterm--get-prompt-point` | `ghostel-input-start-point` | 输入区域起始位置 |
| `vterm--get-end-of-line` | `line-end-position` at cursor | 输入区域结束位置 |
| `vterm-delete-region` | 定位 + Delete 序列 | 删除文本 |
| `vterm-goto-char` | Left/Right 箭头 | 定位 terminal cursor |
| `vterm-insert` | `ghostel-send-string` | 插入文本 |
| `vterm-yank` | `ghostel-paste-string` | 粘贴 |
| `vterm--self-insert` | `ghostel--self-insert` | 直通按键 |
| `vterm-previous/next-prompt` | `ghostel-previous/next-prompt` | Prompt 导航 |

## 核心辅助函数

### evil-ghostel--input-bounds

```elisp
(defun evil-ghostel--input-bounds ()
  "Return (INPUT-START . INPUT-END) for the current input line."
  (let* ((input-start (ghostel-input-start-point))
         (cursor-pos (ghostel-cursor-point))
         (input-end (when cursor-pos
                      (save-excursion
                        (goto-char cursor-pos)
                        (line-end-position)))))
    (when (and input-start input-end (<= input-start input-end))
      (cons input-start input-end))))
```

### evil-ghostel--point-in-input-p

```elisp
(defun evil-ghostel--point-in-input-p ()
  "Return non-nil if point is within the current input area."
  (when-let* ((bounds (evil-ghostel--input-bounds)))
    (and (>= (point) (car bounds))
         (<= (point) (cdr bounds)))))
```

### evil-ghostel--position-terminal-cursor

```elisp
(defun evil-ghostel--position-terminal-cursor (target-pos)
  "Move the terminal cursor to align with TARGET-POS in the buffer."
  (let ((current (ghostel-cursor-point)))
    (when (and current target-pos (/= current target-pos))
      (let ((diff (- target-pos current)))
        (cond
         ((> diff 0) (dotimes (_ diff) (ghostel-send-key "right")))
         ((< diff 0) (dotimes (_ (- diff)) (ghostel-send-key "left"))))))))
```

### evil-ghostel--delete-region-in-terminal

```elisp
(defun evil-ghostel--delete-region-in-terminal (beg end)
  "Delete text between BEG and END in the terminal via key sequences."
  (let ((count (- end beg)))
    (when (> count 0)
      (evil-ghostel--position-terminal-cursor beg)
      (dotimes (_ count)
        (ghostel-send-key "delete")))))
```

## 操作符实现

### delete (d)

```elisp
(evil-define-operator evil-ghostel-delete (beg end type register yank-handler)
  (let* ((bounds (evil-ghostel--input-bounds))
         (input-start (car bounds))
         (input-end (cdr bounds))
         (beg (max (or beg (point)) input-start))
         (end (min (or end beg) input-end)))
    (when (and bounds (< beg end))
      (evil-yank beg end type register yank-handler)
      (evil-ghostel--delete-region-in-terminal beg end))))
```

### delete-line (D)

定位到 point + Ctrl+k。

### delete-char (x) / delete-backward-char (X)

x: `ghostel-send-key "delete"`
X: `ghostel-send-key "backspace"`

### change (c) / change-line (C) / substitute (s) / substitute-line (S)

删除 + `evil-insert`。

### replace (r)

定位 + Delete + 发送替换字符 + Left。

## Insert/Append 命令

- `i`: 定位 terminal cursor + evil-insert
- `I`: 定位到 `ghostel-input-start-point` + evil-insert
- `a`: 定位到 point+1 + evil-insert
- `A`: 定位到行尾 + evil-insert

## Paste

- `p`: 定位到 point+1 + `ghostel-paste-string`
- `P`: 定位到 point + `ghostel-paste-string`

## 导航

| 键 | 实现 |
|---|---|
| `^` | `ghostel-input-start-point` |
| `[[` | `ghostel--navigate-previous-prompt`（直接调用内部函数，避免触发 emacs-mode 切换） |
| `]]` | `ghostel--navigate-next-prompt`（同上） |
| `j` | `evil-next-line` + guard（不过最后一个 prompt） |
| `G` | `ghostel-cursor-point`（重置到 terminal cursor 位置） |
| `u` | `(ghostel-send-string "\x1f")`（发送 0x1F = Ctrl+_，readline undo） |

## Visual 模式

- `d`: 选中区域删除（定位 + Delete N 次）
- `x`: 同 d

## ESC 切换

```elisp
(defvar-local evil-ghostel-send-escape-to-vterm-p nil)

(defun evil-ghostel-toggle-send-escape ()
  "Toggle where ESC is sent between terminal and Emacs.
Bound to C-c C-z. Needed for programs that use ESC (vim, ssh'd emacs)."
  (interactive)
  (if evil-ghostel-send-escape-to-vterm-p
      (evil-define-key 'insert ghostel-mode-map (kbd "<escape>")
        (lookup-key evil-insert-state-map (kbd "<escape>")))
    (evil-define-key 'insert ghostel-mode-map
      (kbd "<escape>") 'ghostel--self-insert))
  (setq evil-ghostel-send-escape-to-vterm-p
        (not evil-ghostel-send-escape-to-vterm-p))
  (message "Sending ESC to %s."
           (if evil-ghostel-send-escape-to-vterm-p "vterm" "emacs")))
```

## Insert 状态下 C- 键直通

绑定与 evil-collection-vterm 相同的 C- 键到 `ghostel--self-insert`：

C-a, C-d, C-e, C-k, C-n, C-o, C-p, C-r, C-t, C-w, C-y, C-z, <delete>

（ghostel 的 semi-char mode 已处理大部分，这些绑定确保 evil insert state 不拦截它们）

## 光标行为

```elisp
(defcustom evil-ghostel-move-cursor-back nil
  "Whether cursor moves back when exiting insert state."
  :type 'boolean
  :group 'ghostel)

(defun evil-ghostel-escape-stay ()
  (setq-local evil-move-cursor-back evil-ghostel-move-cursor-back))

(add-hook 'ghostel-mode-hook #'evil-ghostel-escape-stay)
```

## 与现有文件的关系

### ghostel-mode.el (package-hooks)

保留非 evil 功能：kill-on-exit hook、DnD 支持、RVM 激活。
**删除所有** `evil-define-key 'hybrid` 绑定（由 evil-ghostel.el 接管）。
**清理** `comint-send-string` / `get-buffer-process` 残留调用（lines 53-56, 70），替换为 ghostel API。

### zsh-ghostel.el (crafts)

`zsh-ghostel-mode-map` 继承 `ghostel-semi-char-mode-map`，evil-ghostel 的绑定通过继承传播。
`zsh-ghostel-mode` 中的 hybrid 状态逻辑需适配为 insert/normal 模型：

1. `evil-define-key 'hybrid` → `evil-define-key 'insert`（所有键绑定）
2. `evil-hybrid-state` → `evil-insert-state`（所有状态切换调用）
3. `evil-yank-for-zsh-ghostel` 中的 `(evil-hybrid-state)` 改为 `(evil-insert-state)`
4. `ghostel-enter-hybrid-state-decently` 改为进入 insert 状态
5. shell-pop 和 s-j/s-k 等导航绑定从 hybrid 迁移到 insert

### evil-collection.el (package-hooks)

已有注释 "ghostel uses evil-ghostel.el instead"，无需改动。

## 不支持的功能

与 evil-collection-vterm 相比，以下功能因 ghostel 架构限制**不实现**：

- `d + block motion`：块选择删除（终端序列无法表达块操作）
- 行级 `d + line motion` 的复杂行处理（只处理当前输入行）
- `vterm-reset-cursor-point` 等效功能（ghostel 使用 `ghostel-cursor-point` 替代）

## 说明

- **yank (y)**：不实现自定义 operator，标准 evil 的 `evil-yank` 可直接使用（buffer 可读）
- **`ghostel-send-key` 签名**：`(ghostel-send-key KEY-NAME &optional MODS)`，MODS 为逗号分隔字符串如 `"ctrl"`、`"shift,ctrl"`

## 约束和边界

1. 操作符只在当前输入行（prompt 到行尾）生效
2. 不在 TUI 应用（vim、less 等）中生效（guard: `ghostel-input-start-point` 返回 nil 时不操作）
3. 多行输入只处理当前光标所在行
4. 删除操作通过终端序列实现，存在极短暂的按键延迟
