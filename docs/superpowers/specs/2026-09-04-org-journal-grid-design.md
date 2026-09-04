# org-journal-grid Design

只读 SVG 时间网格，用来看 org-journal 里最近 N 天的条目。点块跳到原文。不接 org-agenda，不在网格里改 journal。

渲染从 `~/cascode/github.com/org-timegrid` 裁出只读子集，改前缀为 `org-journal-grid-*`，放进 `emacs-config/crafts/`。运行时不 `require` org-timegrid。

## 已确认的决策

| 决策项 | 选择 |
|--------|------|
| 交互 | 只读。单击选中，RET / 双击跳转。无拖拽创建/移动/缩放，无改 heading |
| 实现 | 裁一份 org-timegrid 只读渲染器进 crafts，不依赖外部包 |
| 时间窗口 | 日历连续 N 天（默认 7），右端默认今天；没文件的日子留空列 |
| 条目来源 | 只收 level-2 heading，且标题在 TODO 关键字之后以 `HH:MM` 开头 |
| 时间戳 | 文件名日期 + heading 里的时刻。不用 SCHEDULED / CLOSED |
| 时长 | 固定，默认 30 分钟，`defcustom` |
| TODO | 默认不显示未完成 TODO（`org-not-done-keywords`）。`DONE` 和无关键字都显示 |
| DONE 外观 | 普通彩色块，不淡化、不划掉 |
| 可配置项 | 全部 `defcustom`，组名 `org-journal-grid` |

## 非目标

- org-agenda strip、org-timegrid 包本身
- 嵌套 heading、没有 `HH:MM` 的旧条目、all-day rail
- 在网格里改时间、改 TODO、clock、refile、archive、复制/剪切
- isearch 日历块（org-timegrid-isearch）

## 文件

| 文件 | 职责 |
|------|------|
| `emacs-config/crafts/org-journal-grid.el` | 入口：全部行为用 defcustom、扫目录、解析 heading、backend、`org-journal-grid` 命令 |
| `emacs-config/crafts/org-journal-grid-model.el` | 从 `org-timegrid-model.el` 改编：event/block 记录、只读 backend（list + visit）、重叠车道 |
| `emacs-config/crafts/org-journal-grid-render.el` | 从 `org-timegrid.el` 裁出：SVG 周网格、导航/缩放/主题、光标。删除全部编辑手势 |
| `emacs-config/crafts/init.el` | autoload `org-journal-grid` |
| `emacs-config/key-bindings/spacemacs-set-leader-keys.el` | `"aojg"` → `org-journal-grid` |

三个新 `.el` 都开 `lexical-binding`。符号一律 `org-journal-grid-*`，避免以后安装 org-timegrid 时冲突。

改编文件保留原 GPL-3 版权头，并注明改编自 [org-timegrid](https://github.com/Gleek/org-timegrid)。不复制：

- `org-timegrid-org.el`
- `org-timegrid-agenda.el`
- `org-timegrid-calendar.el`
- `org-timegrid-isearch.el`

## 渲染器裁剪

从 `org-timegrid.el` **保留**：SVG 绘制、重叠车道、日期列头、now 线、主题重绘、光标、单击选中、双击/RET 调用 visit、`n`/`p` 块间移动、方向键与 `C-n`/`C-p` 移光标、滚轮滚动、缩放、数据刷新定时器。

**删除**：鼠标拖拽创建/移动/缩放、super-drag 复制、shift-drag 加时间、`M-up/down` 移动、`S-up/down` 改时长、`e` 改名、`d` 删除、kill/yank、undo、以及所有写回 Org 的命令。

all-day 事件行不画。日期列头保留。`t` 不用于输入时间，改为本 buffer 切换是否显示 TODO。

跨午夜：`start + duration` 若越过当天 24:00，把 `end` 截在当天结束，不画到下一列。

## defcustom

组：`org-journal-grid`。行为项：

| 变量 | 默认 | 含义 |
|------|------|------|
| `org-journal-grid-directory` | `nil` | journal 根目录。`nil` 表示：若已绑定 `org-journal-dir` 则用它，否则 `~/Documents/materials/journal/` |
| `org-journal-grid-days` | `7` | 连续日历日数，必须是正整数 |
| `org-journal-grid-show-todo` | `nil` | 为 nil 时隐藏未完成 TODO 条目 |
| `org-journal-grid-default-duration-minutes` | `30` | 无结束时间时的占位时长 |

外观项从 org-timegrid 对应变量改前缀搬过来，同样 `defcustom`，默认与源项目相同：

| 变量 | 默认 |
|------|------|
| `org-journal-grid-start-hour` / `end-hour` | `0` / `24` |
| `org-journal-grid-pixels-per-minute` | `0.9` |
| `org-journal-grid-default-zoom` | `0.7` |
| `org-journal-grid-slot-minutes` | `15` |
| `org-journal-grid-default-color` | `blue` |
| `org-journal-grid-tag-color-alist` | `nil`（全部用默认色） |
| `org-journal-grid-data-refresh-seconds` | `300`（`0` 或 `nil` 表示不自动刷新） |

## 解析规则

窗口：锚点日（默认今天）往回连续 `org-journal-grid-days` 个日历日，含锚点日。列从左到右按日期升序。

文件：目录下**恰好**名为 `YYYY-MM-DD` 的文件。忽略 `2023-12-20.md`、`YYYY-MM-DD-media/`、`*.man` 等。某天没有文件则该列为空。

读文件：已访问的 buffer 用 live 内容（含未保存修改）；未打开的文件在临时 buffer 里解析，不留在 buffer list，不抢窗口。

只处理 **level-2** heading。用 `org-element` 取 `:todo-keyword`、`:raw-value` / 标题、tags、位置。去掉 TODO 关键字后，标题必须匹配开头的 `HH:MM`（`[0-9]{1,2}:[0-9]{2}`，其后为空格或结尾）。对不上则丢弃。

时间戳：`start` = 该文件名对应公历日的绝对分钟 + `HH:MM`；`end` = `start + org-journal-grid-default-duration-minutes`，再按「跨午夜截断」处理。

TODO 过滤：`org-journal-grid-show-todo` 为 nil 时，若 `:todo-keyword` 属于 `org-not-done-keywords`（`TODO`、`NEXT` 等）则丢弃。`DONE`（`org-done-keywords`）和无关键字都保留。保留的条目**不**把 renderer 的 `done` 槽设为真，避免发灰/划线。

块标题：去掉开头的 `HH:MM` 和其后空格，避免和纵轴时刻重复。tags 不画在块上；若 `org-journal-grid-tag-color-alist` 非空，用 heading 上第一个命中的 tag 上色。

event `source` 保存可跳转的 marker（文件 + 位置）。`id` 用 `file:position`，足够在一次刷新内唯一。

## 命令与交互

`M-x org-journal-grid` 打开网格，buffer 名 `*org-journal-grid*`。无前缀时窗口右端为今天。数字前缀覆盖天数（只影响这一次，不改 defcustom）：`C-u 14 M-x org-journal-grid`。

入口文件 `(require 'org)` / `org-element`。不 `(require 'org-timegrid)`。

Leader：`SPC a o j g`。

网格 keymap：

| 键 | 行为 |
|----|------|
| 单击 | 选中格子或块 |
| RET / 双击 | `visit`：打开 journal 文件并跳到 heading |
| `n` / `p` | 下一块 / 上一块 |
| `C-n` `C-p` 方向键 | 移动光标（粒度 `org-journal-grid-slot-minutes`） |
| `b` / `f` | 窗口平移 1 天 |
| `M-b` / `M-f` | 平移一整窗（`org-journal-grid-days`） |
| `j` | `org-read-date` 跳到某日为右端的窗口 |
| `.` | 回到以今天为右端的窗口 |
| `g` | 重新从文件读，保持滚动和光标尽量不动 |
| `t` | 本 buffer `setq-local` 切换 `org-journal-grid-show-todo` 并刷新；不改 defcustom 默认值 |
| `q` | 关掉 buffer |
| `C-x +` / `C-x C--` / `C-x C-0` | 缩放 |

网格不修改任何 journal 文件。

## 错误处理

| 情况 | 行为 |
|------|------|
| `(image-type-available-p 'svg)` 为 nil | `user-error`，不打开 |
| `org-journal-grid-directory` 不是目录 | `user-error`，带路径 |
| `org-journal-grid-days` 不是正整数 | `user-error` |
| 某一天文件不可读 | 该列留空，不中断整张网格 |
| visit 时文件已不存在或 marker 失效 | `user-error` |

## 数据流

1. 命令算出锚点日和天数，创建/复用网格 buffer，挂上只读 backend。
2. list-function 对每一天找 `YYYY-MM-DD` 文件，解析 level-2 heading，过滤 TODO，生成 event。
3. model 把 event 转成相对窗口的 block，处理重叠车道。
4. renderer 画 SVG。RET / 双击把 event 的 source marker 交给 visit-function。
5. 定时刷新（若启用）和 `g` 走同一条 list 路径。

## 验证

- 对三个新 `.el` 做 elisp syntax-check（括号、自由变量、参数个数）。
- 在当前 Spacemacs 会话里 `emacsclient` 加载后执行 `org-journal-grid`。
- 对照 `~/Documents/materials/journal` 最近几天：只出现 level-2 且带 `HH:MM` 的非 TODO 条目；时间为文件名日期 + heading 时刻；TODO 条目默认不出现；`t` 之后出现；没有文件的日子空列；RET 跳到原文。
- 不打开 org-agenda，确认无 agenda 副作用。
- 确认未 `(require 'org-timegrid)`。
