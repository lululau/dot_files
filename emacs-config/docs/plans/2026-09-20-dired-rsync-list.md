# Dired Rsync 进程监控与管理列表实现计划

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 基于 `tablist` 为 `dired-rsync` 构建一个完整的后台 rsync 进程监控与管理列表（支持实时进度、速率、耗时、终止、查看日志、重试、Dired跳转及智能刷新）。

**Architecture:** 采用轻量 Advice 拦截 `dired-rsync`、`dired-rsync--do-run`、`dired-rsync--filter` 与 `dired-rsync--sentinel`，记录任务元数据与生命周期；解析 `progress2` 标准输出更新 `dired-rsync-job` 结构体；基于 `tablist-mode` 渲染多列交互表格，并通过按需定时器实现零后台消耗的平滑刷新。

**Tech Stack:** Emacs Lisp, `tablist`, `tabulated-list`, `dired`, `dired-rsync`, `cl-lib`.

---

### Task 1: 数据结构与核心注册表 (Job Struct & Registry)

**Files:**
- Create: `/Users/liuxiang/.config/emacs-config/crafts/dired-rsync-list.el`
- Test: `/Users/liuxiang/.config/emacs-config/crafts/dired-rsync-list.el` (via `emacsclient`)

**Step 1: 编写数据结构与注册表定义**
- 定义 `(cl-defstruct (dired-rsync-job ...))` 包含 `id`, `pid`, `process`, `status`, `source`, `source-files`, `dest`, `cmd`, `start-time`, `end-time`, `exit-code`, `bytes-transferred`, `percent`, `speed`, `eta`, `file-progress`, `log-output`, `dired-buffer`, `proc-buffer-name` 等字段；
- 定义全局变量 `dired-rsync-list--jobs` 与递增计数器 `dired-rsync-list--next-id`；
- 提供辅助函数：`dired-rsync-list--find-job-by-process`, `dired-rsync-list--find-job-by-id`, `dired-rsync-list--active-jobs-p`。

**Step 2: 验证数据结构求值**
- 运行：`TMPDIR=$(getconf DARWIN_USER_TEMP_DIR) emacsclient -e "(load-file \"/Users/liuxiang/.config/emacs-config/crafts/dired-rsync-list.el\")"`
- 预期：加载成功，返回 `t`。

**Step 3: 提交代码**
- `git add crafts/dired-rsync-list.el && git commit -m "feat(dired-rsync-list): add job struct and registry"`

---

### Task 2: progress2 输出解析器与生命周期 Advice

**Files:**
- Modify: `/Users/liuxiang/.config/emacs-config/crafts/dired-rsync-list.el`

**Step 1: 实现 `progress2` 正则解析器与格式化工具**
- 实现 `dired-rsync-list--parse-progress2 (string)`，提取：
  1. `bytes`（如 `1.25G`, `123.45M`）
  2. `percent`（如 `45%`）
  3. `speed`（如 `32.50MB/s`）
  4. `eta`（如 `0:01:23`）
  5. `file-progress`（如 `(xfr#5, to-chk=12/34)`）
- 实现耗时格式化函数 `dired-rsync-list--format-duration (seconds)` 和时间格式化函数。

**Step 2: 挂载生命周期 Advice**
- Advice `dired-rsync` (`:around`)：捕获当前 `sfiles`、`dest`、`current-buffer`；
- Advice `dired-rsync--do-run` (`:around`)：提取 command，生成 `dired-rsync-job`，启动进程并关联 PID，推入 `dired-rsync-list--jobs`；
- Advice `dired-rsync--filter` (`:after`)：实时调用 progress2 解析器写回 job；追加输出到内存 buffer/log；
- Advice `dired-rsync--sentinel` (`:before`)：在原生代码 `kill-buffer` 前读取并保存完整日志，记录 `end-time`、`exit-code` 与终态（`finished`/`killed`/`failed`）。

**Step 3: 验证解析与 Advice 拦截**
- 运行 `emacsclient` 测试用例验证正则对各类 progress2 输出片段的正确解析。

**Step 4: 提交代码**
- `git add crafts/dired-rsync-list.el && git commit -m "feat(dired-rsync-list): add progress2 parser and lifecycle advices"`

---

### Task 3: Tablist UI 模式与表格条目渲染

**Files:**
- Modify: `/Users/liuxiang/.config/emacs-config/crafts/dired-rsync-list.el`

**Step 1: 定义 `dired-rsync-list-mode`**
- 派生自 `tablist-mode`；
- 设置 `tabulated-list-format`：
  - `ID` (4, t)
  - `PID` (7, t)
  - `Status` (10, t)
  - `Progress` (8, t)
  - `Speed` (12, nil)
  - `Transferred` (10, nil)
  - `ETA` (9, nil)
  - `Elapsed` (9, nil)
  - `Source` (24, nil)
  - `Destination` (24, nil)
  - `Start Time` (17, nil)
  - `End Time` (17, nil)
- 实现 `dired-rsync-list--entries` 生成函数，对各列应用着色与格式化（如 running 浅蓝、finished 绿色、failed 红色、killed 黄色）。
- 实现命令 `dired-rsync-list`：打开或切换到 `*dired-rsync-list*` 缓冲区并刷新。

**Step 2: 验证表格渲染**
- 插入模拟 job 数据，调用 `(dired-rsync-list)`，验证列对齐与颜色高亮。

**Step 3: 提交代码**
- `git add crafts/dired-rsync-list.el && git commit -m "feat(dired-rsync-list): implement tablist mode and entries generation"`

---

### Task 4: 交互操作（终止、查看输出、重试、Dired跳转、清理）

**Files:**
- Modify: `/Users/liuxiang/.config/emacs-config/crafts/dired-rsync-list.el`

**Step 1: 实现行级交互命令**
- `dired-rsync-list-view-output` (`RET` / `o`)：运行中切到实时进程 buffer；结束后在只读 `*dired-rsync-log: #<id>*` 中弹出查看；
- `dired-rsync-list-kill` (`k` / `K` / `x`)：支持单行或 `m` 标记批量终止，发送 `SIGTERM` / `SIGKILL`，更新状态；
- `dired-rsync-list-retry` (`r`)：利用原参数重新发起传输；
- `dired-rsync-list-jump-source` (`s`)：在 Dired 中打开源目录并定位；
- `dired-rsync-list-jump-dest` (`d`)：在 Dired 中打开目标目录；
- `dired-rsync-list-clear-finished` (`c`)：清除所有已结束/失败的历史任务。

**Step 2: 绑定快捷键与 Evil 集成**
- 在 `dired-rsync-list-mode-map` 绑定标准键；
- 配置 `evil-define-key` / `evilified-state-evilify-map` 兼容 Vim 按键。

**Step 3: 提交代码**
- `git add crafts/dired-rsync-list.el && git commit -m "feat(dired-rsync-list): add interactive commands and evil bindings"`

---

### Task 5: 智能自动刷新机制

**Files:**
- Modify: `/Users/liuxiang/.config/emacs-config/crafts/dired-rsync-list.el`

**Step 1: 实现定时器生命周期管理**
- 定义 `dired-rsync-list--timer`；
- 实现 `dired-rsync-list--maybe-start-timer` 与 `dired-rsync-list--stop-timer`；
- 刷新时使用 `tablist-with-remembering-entry` 保留光标位置与标记；
- 在 `kill-buffer-hook` 与定时器回调中加入空转检查：无 running 任务或窗口不可见时自动 `cancel-timer`。

**Step 2: 验证定时器启停**
- 验证当有运行中任务且 buffer 可见时定时器触发；任务结束后定时器自动停止。

**Step 3: 提交代码**
- `git add crafts/dired-rsync-list.el && git commit -m "feat(dired-rsync-list): add smart auto-refresh timer"`

---

### Task 6: 系统配置集成（Autoload 与 Leader Key）

**Files:**
- Modify: `/Users/liuxiang/.config/emacs-config/crafts/init.el`
- Modify: `/Users/liuxiang/.config/emacs-config/key-bindings/spacemacs-set-leader-keys.el`

**Step 1: 在 `crafts/init.el` 添加 autoload**
- `(autoload 'dired-rsync-list "dired-rsync-list" nil t)`

**Step 2: 在 leader key 添加便捷入口**
- 在 `spacemacs-set-leader-keys.el` 中为 `dired-mode` 或全局绑定入口，例如 `dired-mode` 下 `"al"` 或 `"aR"`。

**Step 3: 提交代码**
- `git add crafts/init.el key-bindings/spacemacs-set-leader-keys.el && git commit -m "feat(dired-rsync-list): register autoload and leader keys"`

---

### Task 7: 完整验证与排错测试

**Files:**
- Test: 真实本地与远端 rsync 任务

**Step 1: 语法与格式检查**
- 执行 `elisp-syntax-check`，确保无未闭合括号、无未定义变量报警。

**Step 2: 运行时真实链路测试**
- 通过 Dired 触发一次文件 rsync；
- 检查 `*dired-rsync-list*` 中 PID、源/目的、耗时、速率、百分比是否准确更新；
- 测试 `RET` 查看日志；
- 测试 `K` 终止任务；
- 验证任务结束后 buffer 被销毁但列表与日志仍可回溯。

**Step 3: 最终提交与总结**
- 整理提交并输出 Walkthrough。
