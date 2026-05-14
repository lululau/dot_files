# zsh-vterm.el 功能说明

基于 [emacs-libvterm](https://github.com/akermu/emacs-libvterm) (vterm-mode) 构建的 zsh 终端增强模块。通过派生 `vterm-mode` 创建 `zsh-vterm-mode`，在保留 vterm 全部能力的基础上，增加了智能缓冲区管理、CLI 导航、快速复制粘贴、Copilot 集成以及面向 tmux 的快捷会话切换等功能。

---

## 一、核心模式：zsh-vterm-mode

```
(define-derived-mode zsh-vterm-mode vterm-mode "zsh" ...)
```

从 `vterm-mode` 派生，mode-line 显示为 `"zsh"`。关键改动：

- **键映射继承**：`zsh-vterm-mode-map` 以 `vterm-mode-map` 为父键映射，所有 vterm 原生快捷键继续生效。
- **insert 函数替换**：将 `zsh-vterm-mode` 的 insert-function 设为 `vterm-send-string`，使通过 `insert` 插入的内容直接发送到终端进程而非写入 buffer。

---

## 二、智能缓冲区管理

### `lx/run-in-zsh-vterm`

```elisp
(lx/run-in-zsh-vterm command buffer-name &optional directory window-type)
```

在 vterm 中运行指定命令，支持三种窗口打开方式：

| window-type | 行为 |
|---|---|
| `'default` (默认) | 在当前窗口切换 |
| `'split` | 右侧分屏打开 |
| `'popup` | 通过 `shell-pop-split-window` 弹出窗口 |

**缓冲区状态管理**：

- 自动记录每个缓冲区关联的命令、环境变量、退出时是否关闭缓冲区（通过动态符号绑定）
- 若缓冲区已存在，智能切换：
  - 当前窗口仅一个且已在目标缓冲区 → bury-buffer 或 delete-window
  - 否则切换到目标缓冲区，同时记录上一个缓冲区到 `zsh-vterm-last-buffer`
- 缓冲区不存在时，根据 `window-type` 选择分屏/弹出/当前窗口，然后创建新的 `zsh-vterm-mode` 缓冲区

### `zsh-vterm` / `zsh-vterm--internal`

与原生 `vterm` 命令签名完全一致，区别在于启动的是 `zsh-vterm-mode` 而非 `vterm-mode`。支持：

- 无参数 → 创建/切换默认缓冲区
- 数字前缀 → 创建/切换编号缓冲区 (如 `*vterm*<2>`)
- 字符串前缀 → 以该名称创建新缓冲区
- 非数字前缀 → 创建新的匿名缓冲区

---

## 三、CLI 导航系统

基于正则表达式匹配 zsh 提示符，实现命令历史输出的快速浏览和选择。

### 提示符正则

```elisp
zsh-vterm-prompt-regexp          = "^.*\\(❯\\|\\]#\\|\\]\\$\\|➜\\) "
zsh-vterm-prompt-has-previous-regexp = "^.*\\(❯\\|➜\\) "
```

支持识别以下常见提示符风格：
- `❯` — starship / powerlevel10k 等主流主题
- `➜` — agnoster 等主题
- `]$` / `]#` — 传统括号风格

### 导航命令

| 命令 | 快捷键 | 功能 |
|---|---|---|
| `zsh-vterm-previous-cli` | `s-C` | 跳转到上一条 CLI 命令或输出 |
| `zsh-vterm-next-cli` | `s-V` | 跳转到下一条 CLI 命令或输出 |

**行为逻辑**（以 previous 为例）：

1. 退出 visual state，进入 normal state
2. 若当前行匹配提示符正则 → 调用 `zsh-vterm-previous-cli-output`：选中当前提示符到上一个提示符之间的全部输出内容
3. 若当前行不匹配提示符 → 调用 `zsh-vterm-previous-cli-command`：定位到上一条命令文本，visual char 模式选中命令部分（不含提示符）

底层使用 evil-mode 的 `evil-ex-execute` 配合搜索正则实现跳转。

---

## 四、快速复制粘贴到终端

### `evil-yank-for-zsh-vterm`

在 visual state 下按 `<return>` (Enter) 触发：

1. 执行标准的 evil yank（字符/行/块选分别处理）
2. 切换到 hybrid state（终端输入模式）
3. 发送字符 `"a"` 后立即发送退格键

此设计的目的是：yank 将内容放入 kill-ring，同时利用 `"a"` + backspace 的组合唤醒 zsh 的 autosuggestion 插件，使粘贴后的内容立即可见并可被 autosuggestion 补全。

---

## 五、Copilot 集成

| 快捷键 | 命令 | 行为 |
|---|---|---|
| `<tab>` | `zsh-vterm-accept-copilot-or-send-tab-to-term` | 有 Copilot 补全时接受补全，否则发送 Tab 到终端 |
| `<backtab>` | `zsh-vterm-accept-copilot-or-send-shift-tab-to-term` | 有 Copilot 补全时接受补全，否则执行 `vterm--self-insert` |

在终端中同时使用 GitHub Copilot 补全和 shell 补全时，Tab 键被智能分配给两者。

---

## 六、Tmux 会话快捷键

通过 `vterm-send-C-j` 发送 `C-j` 前缀，随后发送对应字符，实现 tmux 会话的快速切换。这组绑定利用了 macOS 的 Super (s-) 修饰键和数字键映射到 tmux 的 window/pane：

### Hybrid / Normal State 通用

| 快捷键 | 发送序列 | 用途 |
|---|---|---|
| `s-z` | `C-j z` | tmux 缩放当前 pane（`resize-pane -Z`） |
| `s-j` | `C-j` | tmux 前缀键 |
| `s-]` | `C-j >` | tmux 下一个 window |
| `s-[` | `C-j <` | tmux 上一个 window |

### Hybrid State (终端输入模式)

| 快捷键 | 发送序列 | tmux window |
|---|---|---|
| `M-!` | `C-j 7` | 切换到 window 7 |
| `M-@` | `C-j 8` | 切换到 window 8 |
| `M-#` | `C-j 9` | 切换到 window 9 |
| `M-$` | `C-j 0` | 切换到 window 0 |

### Hybrid / Normal State（C-M-s- 修饰组合）

| 快捷键 | 发送序列 | tmux window |
|---|---|---|
| `C-M-s-!` | `C-j 1` | 切换到 window 1 |
| `C-M-s-@` | `C-j 2` | 切换到 window 2 |
| `C-M-s-#` | `C-j 3` | 切换到 window 3 |
| `C-M-s-$` | `C-j 4` | 切换到 window 4 |
| `C-M-s-%` | `C-j 5` | 切换到 window 5 |
| `C-M-s-^` | `C-j 6` | 切换到 window 6 |
| `C-M-s-\|` | `C-j C-j` | 双重前缀（自定义功能） |

---

## 七、其他快捷键

### 终端信号发送

| 快捷键 | 命令 | 发送 |
|---|---|---|
| `s-a` | `vterm-send-C-z` | SIGTSTP (挂起进程) |
| `M-C` (hybrid + normal) | lambda | `C-S-C` (Ctrl+Shift+C) |
| `M-V` (hybrid + normal) | lambda | `C-S-V` (Ctrl+Shift+V) |
| `M-N` (hybrid + normal) | lambda | `C-S-N` |
| `M-P` (hybrid + normal) | lambda | `C-S-P` |

### 窗口管理

| 快捷键 | 行为 |
|---|---|
| `s-S-RET` (`<s-S-return>`) | 若当前窗口有父窗口（非全屏），执行 `spacemacs/toggle-maximize-buffer` 最大化；否则发送 `C-j z`（tmux 缩放当前 pane） |
| `M-h` (hybrid) | 执行 `vterm--self-insert`（发送 M-h 到终端） |

### 项目目录跳转

| 快捷键 | 命令 | 行为 |
|---|---|---|
| `s-i s-o` | `zsh-vterm-goto-tmp-dir` | 在终端中 `cd` 到当前 projectile 项目的 `tmp/` 目录；无项目时回退到 `~/tmp` |

---

## 八、辅助函数

### `zsh-vterm-get-current-line`

获取从行首到当前光标位置的文本，并去除提示符前缀。用于获取当前正在输入的命令内容。

### `zsh-vterm-get-current-line-beginning`

检测当前行是否包含 `│`（竖线边框字符，常见于多 panel 终端布局）。若光标在竖线右侧，返回竖线后一位的列号；否则返回 0。用于在有边框的终端布局中正确定位命令起始位置。

---

## 九、与原生 vterm 的关系

| 特性 | vterm | zsh-vterm |
|---|---|---|
| 终端模拟器核心 | libvterm 动态模块 | 继承，无修改 |
| 基础快捷键 | vterm-mode-map | 继承并扩展 |
| 缓冲区创建 | `vterm` / `vterm--internal` | `zsh-vterm` / `zsh-vterm--internal`（启动 zsh-vterm-mode） |
| Copy Mode | 内置 vterm-copy-mode | 继承 |
| Prompt 检测 | 支持 shell-side 和正则两种方式 | 在此基础上增加自定义提示符正则用于 CLI 导航 |
| Evil 集成 | 无 | 深度集成，包括 visual yank、state 切换、ex 搜索 |
| Copilot 集成 | 无 | Tab 键智能分发 |
| Tmux 集成 | 无 | 通过快捷键映射 tmux 命令 |
| 运行命令 | 手动打开终端 | `lx/run-in-zsh-vterm` 一键运行指定命令 |
