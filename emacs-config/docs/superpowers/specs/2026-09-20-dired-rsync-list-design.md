# Dired Rsync 进程监控与管理列表设计规范

## 1. 概述与背景

`dired-rsync` 允许用户在 Emacs Dired 缓冲区中通过 `rsync` 异步复制文件或目录。然而，原生 `dired-rsync` 存在以下局限性：
- 缺乏统一的任务管理列表，无法全面掌握所有后台传输任务的进度与状态；
- 在传输成功完成时，原生实现会直接销毁进程缓冲区（`kill-buffer`），导致停止时间、总耗时及传输日志全部丢失；
- 虽然默认开启 `--info=progress2`，但原生实现仅通过正则抓取粗略的百分比用于更新 mode-line，未解析已传输字节、即时速率、预估剩余时间（ETA）和文件数统计等关键指标。

本项目旨在设计并实现 `dired-rsync-list.el`，基于 `tablist` 提供功能完善、轻量高效的 rsync 任务监控与管理中心。

---

## 2. 目标与范围

### 2.1 目标
1. **代码归属**：独立封装在 `/Users/liuxiang/.config/emacs-config/crafts/dired-rsync-list.el`。
2. **全局任务追踪**：自动捕获所有由 `dired-rsync` 启动的进程元数据（PID、源、目的、启动/停止时间、总耗时、状态、完整命令行等）。
3. **进度深度解析**：实时解析 `rsync --info=progress2` 输出（已传大小、百分比、速率、ETA、文件计数）。
4. **Tablist 交互管理**：
   - 展现清晰的多列结构；
   - 交互操作：查看实时/离线输出日志、终止进程（支持标记多选批量终止）、重新运行（重试）、跳转源/目的目录、清理历史；
   - 支持 tablist 原生快捷键（标记、过滤、排序）。
5. **智能刷新机制**：界面可见且有运行任务时每秒刷新，窗口隐藏或任务结束时自动挂起定时器，零后台空转。
6. **无缝兼容**：向后兼容现有 `dired-rsync` 使用习惯，不破坏原有工作流。

---

## 3. 架构设计

### 3.1 总体架构

```text
  [ Dired Buffer: dired-rsync ]
                │
                ▼
  ┌───────────────────────────────┐
  │ Advice 拦截层                 │
  │ • 拦截参数: sfiles, dest, cmd │
  │ • 生成 dired-rsync-job 实例   │
  │ • 绑定 process 与 pid         │
  └─────────────┬─────────────────┘
                │
        ┌───────┴───────┐
        ▼               ▼
┌──────────────┐ ┌────────────────┐
│ Filter 增强  │ │ Sentinel 增强  │
│ 解析progress2│ │ 捕获结束时间/日志│
│ 实时写回 job │ │ 状态流转与持久化│
└───────┬──────┘ └────────┬───────┘
        │                 │
        └────────┬────────┘
                 ▼
     ┌────────────────────────┐
     │ 任务注册表 (Job List)   │
     │ dired-rsync-list--jobs │
     └───────────┬────────────┘
                 ▼
     ┌────────────────────────┐
     │ Tablist 视图呈现层      │
     │ *dired-rsync-list*     │
     │ (按需定时器平滑刷新)    │
     └────────────────────────┘
```

### 3.2 核心数据结构 (`dired-rsync-job`)

采用 `cl-defstruct` 定义结构体：

```elisp
(cl-defstruct (dired-rsync-job (:constructor dired-rsync-job--create))
  id                ;; 任务唯一自增整型序号 (1, 2, 3...)
  pid               ;; 进程 PID (即使进程结束依然保留展示)
  process           ;; Emacs process 对象 (进程存活时有效)
  status            ;; 'running | 'finished | 'failed | 'killed
  source            ;; 源路径显示简写 (单文件直接显示文件名，多文件如 "[3 files]")
  source-files      ;; 源文件/目录绝对路径完整列表 (用于重试和 Dired 跳转)
  dest              ;; 目标路径 (本地路径或 TRAMP 远端路径)
  cmd               ;; 执行的完整 rsync 命令行
  start-time        ;; 启动时间戳 (float-time)
  end-time          ;; 结束时间戳 (float-time 或 nil)
  exit-code         ;; 退出代码或信号字符串
  ;; progress2 解析字段
  bytes-transferred ;; 已传输大小 (如 "1.25G")
  percent           ;; 完成百分比 (如 "45%")
  speed             ;; 瞬时传输速率 (如 "32.50MB/s")
  eta               ;; 预估剩余耗时 (如 "0:01:23")
  file-progress     ;; 文件传输与待检查信息 (如 "xfr#5, to-chk=12/34")
  ;; 日志与关联
  log-output        ;; 进程结束时从 buffer 中提取保存的完整日志
  dired-buffer      ;; 发起时所在的 Dired buffer
  proc-buffer-name  ;; 原始进程 buffer 名称
  )
```

---

## 4. 核心功能与模块设计

### 4.1 progress2 输出解析器

针对标准输出正则提取：
- **模式**：
  ```elisp
  (rx (group (+ (any digit ",." "KMGTPEZYkmgtpezy"))) ;; 1: bytes
      (+ (any " \t"))
      (group (+ digit) "%")                          ;; 2: percent
      (+ (any " \t"))
      (group (+ (any digit ".KMGTPEZYkmgtpezyB/s" "b/s"))) ;; 3: speed
      (+ (any " \t"))
      (group (+ digit) ":" (+ digit) (? ":" (+ digit)))   ;; 4: eta
      (? (+ (any " \t"))
         (group "(" (+ (not (any ")\n"))) ")")))     ;; 5: files-info
  ```
- **回调逻辑**：
  在 `dired-rsync--filter` 的 `:after` advice 中，每次拿到输出 string 时执行匹配，若命中则立即写回对应 job 的字段。

### 4.2 生命周期 Advice 挂载

1. **`dired-rsync` 拦截**：
   - 提取 `sfiles` 和 `dest`；
   - 动态绑定上下文变量 `dired-rsync-list--current-context`。
2. **`dired-rsync--do-run` 拦截**：
   - 提取 `command` 与上下文信息；
   - 实例化 `job` 并递增 `id`；
   - 执行原始进程创建，关联 `(process-id proc)`；
   - 将 `job` 登记至 `dired-rsync-list--jobs` 列表顶部；
   - 触发一次界面更新。
3. **`dired-rsync--sentinel` 拦截 (`:before`)**：
   - 在原生逻辑 `(kill-buffer proc-buf)` 前，读取 `(with-current-buffer proc-buf (buffer-string))` 缓存至 `job.log-output`；
   - 计算并记录 `end-time` 与 `exit-code`；
   - 状态分支：
     - 若 `s-starts-with-p "finished" desc`，置为 `'finished`，并将 `percent` 补为 `"100%"`，`eta` 补为 `"0:00:00"`；
     - 若被发送信号终止，置为 `'killed`；
     - 其余异常退出置为 `'failed`；
   - 触发一次界面更新。

---

## 5. Tablist 视图与用户交互

### 5.1 表格列设计

模式名：`dired-rsync-list-mode`，派生自 `tablist-mode`。
Buffer 名称：`*dired-rsync-list*`。

| 列名 | 宽度 | 格式化规范与视觉渲染 |
| :--- | :--- | :--- |
| **ID** | 4 | 右对齐数字排序 |
| **PID** | 7 | 进程号（运行态普通字体，已结束灰色） |
| **Status** | 10 | 颜色标识：<br>• `running`: 浅蓝/青色 (`font-lock-keyword-face`)<br>• `finished`: 绿色 (`success`)<br>• `failed`: 红色 (`error`)<br>• `killed`: 黄色 (`warning`) |
| **Progress** | 8 | 右对齐，按数值排序 |
| **Speed** | 12 | 传输速率 |
| **Transferred** | 10 | 已传大小 |
| **ETA** | 9 | 预计剩余时间（完成显示 `--:--`） |
| **Elapsed** | 9 | 运行中：`当前时间 - 启动时间`；已结束：`停止时间 - 启动时间` |
| **Source** | 24 | 源文件简写（支持截断，光标停留可在 minibuffer 或 tooltip 显示全路径） |
| **Destination** | 24 | 目的目录（支持截断） |
| **Start Time** | 17 | `MM-DD HH:MM:SS` |
| **End Time** | 17 | `MM-DD HH:MM:SS`（未结束显示 `--:--:--`） |

### 5.2 交互命令映射

| 按键 (Evil Normal / Evilified) | 按键 (Emacs State) | 函数 | 功能描述 |
| :--- | :--- | :--- | :--- |
| `RET` / `o` | `RET` / `o` | `dired-rsync-list-view-output` | 查看详细输出（运行中跳至实时 buffer，已结束弹窗显示离线 log） |
| `K` / `x` | `k` | `dired-rsync-list-kill` | 终止进程（支持 `m` 标记批量 kill，弹窗确认） |
| `r` | `r` | `dired-rsync-list-retry` | 重新运行选中的任务 |
| `s` | `s` | `dired-rsync-list-jump-source` | 在 Dired 中跳转到该任务源文件 |
| `d` | `d` | `dired-rsync-list-jump-dest` | 在 Dired 中跳转到该任务目标目录 |
| `c` | `c` | `dired-rsync-list-clear-finished` | 清理已结束的任务历史 |
| `g` / `gr` | `g` | `dired-rsync-list-refresh` | 手动刷新列表 |
| `m` / `u` / `U` | `m` / `u` / `U` | (tablist 原生) | 标记 / 取消标记 / 全部取消 |
| `/` | `/` | (tablist 原生) | 交互式条件过滤 |
| `S` | `S` | (tablist 原生) | 交互式列排序 |
| `q` | `q` | `quit-window` | 退出并关闭列表 |

### 5.3 智能平滑刷新
- 利用 `tablist-with-remembering-entry` 执行 `tabulated-list-print t`，刷新过程不丢光标位置，不重置 mark。
- 启动 1 秒定时器，仅在 `(get-buffer-window "*dired-rsync-list*")` 且存在 `'running` 状态任务时运转；无运行任务或离开窗口时立即停用。

---

## 6. 异常与边界处理

1. **多文件传输源路径展示**：若单次传输标记了数十个文件，`Source` 列自动合并显示为首文件名 + `(+N files)`，避免列表横向撑爆。
2. **TRAMP 远程路径兼容**：目标为远端路径（如 `/ssh:user@host:/data/`）时，`jump-dest` 通过 `dired` 正确打开 TRAMP 目录。
3. **孤儿进程与异常终止**：若 rsync 外部崩溃或被终端强制 kill，sentinel 能捕获到非正常退出状态，不会导致任务永久悬挂在 `running` 状态。
4. **内存防泄漏**：限制历史任务最大条数（默认 `dired-rsync-list-max-jobs` 为 50），超额时自动出队最早的已完成记录。

---

## 7. 验证计划

1. **静态检查**：通过 `elisp-syntax-check` 校验 `dired-rsync-list.el` 的语法与语义。
2. **单元测试与运行时求值**：
   - 使用 `emacsclient -e` 动态载入 `crafts/dired-rsync-list.el` 并确认特性加载；
   - 验证 progress2 正则表达式匹配多种边界输出（KB/MB/GB, 单多文件，ETA 格式）；
   - 发起本地与模拟远程传输，验证列表是否准确捕获 PID、耗时和进度；
   - 测试 `K` 终止进程、`RET` 查看输出、`r` 重新发起及自动定时器按需唤醒与休眠。
