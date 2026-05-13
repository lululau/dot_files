# Vterm → Ghostel 全面迁移设计

## 背景

将 Emacs 配置中的所有 vterm 依赖替换为 ghostel（基于 libghostty-vt 的现代终端模拟器）。涉及 30+ 文件、8 个自制包、3 个主模式、50+ 快捷键、以及 zshrc shell 集成。

## 已确认的决策

| 决策项 | 选择 |
|--------|------|
| Evil 集成 | evil-ghostel.el 为基础 + 自定义扩展 |
| 迁移策略 | 一刀切（不保留 vterm 双轨） |
| 执行路径 | 按依赖顺序逐包移植 |
| 命名 | 保留原名模式（zsh-ghostel、pry-ghostel 等） |
| 范围 | 全部包 + zshrc + Docker |
| 多终端 | ghostel 自带机制（prefix arg + ghostel-other） |
| Spacemacs 层 | 在 package-hooks/ 下创建 ghostel-mode.el 覆盖 |
| 旧文件 | 备份到 backup/ 目录 |

## API 映射表

### Emacs Lisp 侧

| vterm API | ghostel API | 备注 |
|-----------|-------------|------|
| `vterm-mode` | `ghostel-mode` | 主模式 |
| `vterm-mode-map` | `ghostel-mode-map` | 键映射 |
| `vterm-send-string` | `ghostel-send-string` | 发送文本到终端 |
| `vterm-send-key` | `ghostel-send-key` | 发送按键 |
| `vterm-eval-cmds` | `ghostel-eval-cmds` | OSC 51 eval 命令列表 |
| `vterm-clear-scrollback` | `ghostel-clear-scrollback` | 清除滚动区 |
| `vterm-max-scrollback` | `ghostel-max-scrollback` | 滚动缓冲区大小 |
| `vterm-keymap-exceptions` | `ghostel-keymap-exceptions` | Emacs 保留键 |
| `vterm-kill-buffer-on-exit` | `ghostel-kill-buffer-on-exit` | 退出时杀缓冲区 |
| `vterm-term` | `ghostel-term` | TERM 环境变量 |
| `vterm-shell` | `ghostel-shell` | Shell 程序 |
| `vterm-copy-mode` | `ghostel-copy-mode` | 复制模式 |
| `vterm-buffer-name` | `ghostel-buffer-name` | Buffer 名称 |

### Shell 侧（zsh）

| vterm 机制 | ghostel 机制 | 备注 |
|------------|-------------|------|
| `vterm_printf` | ghostel shell integration（自动注入） | 不需要手动定义 |
| `vterm_cmd` | `ghostel_cmd`（OSC 51） | 参数格式需检查 |
| `vterm_set_directory` | OSC 7（自动注入） | 不需要 chpwd hook |
| `vterm_cmd find-remote-file` | ghostel 文件检测 + TRAMP | 需在 eval-cmds 注册 |
| `vterm_cmd download/upload` | 需在 `ghostel-eval-cmds` 注册 | 自定义命令 |
| `vterm_cmd save-zsh-history` | 需在 `ghostel-eval-cmds` 注册 | 自定义命令 |
| `vterm_cmd update-zsh-history-outcome` | 需在 `ghostel-eval-cmds` 注册 | 自定义命令 |

## 包迁移详细设计

### 核心包

#### run-in-ghostel.el（原 run-in-vterm.el）

功能：在专用 ghostel buffer 中运行命令。

迁移要点：
- `lx/run-in-vterm` → `lx/run-in-ghostel`
- 使用 `ghostel-send-string` 发送命令
- Buffer 命名：`*run-in-ghostel:<cmd>*`
- 光标设置函数（set-green-box-cursor / set-blue-bar-cursor）→ 使用 ghostel 的 cursor escape sequence 或 `ghostel-eval-cmds` 机制
- `lx/run-in-vterm/rerun` → `lx/run-in-ghostel/rerun`
- 下载/上传功能：通过 `ghostel-eval-cmds` 注册 `download` 和 `upload` 命令
- 远程文件查找：通过 `ghostel-eval-cmds` 注册 `find-remote-file` 和 `sudo-find-remote-file`

#### send-to-ghostel.el（原 send-to-vterm.el）

功能：从编辑 buffer 发送代码到终端。

迁移要点：
- `lx/vterm-send-line` → `lx/ghostel-send-line`
- `lx/vterm-send-line-and-go` → `lx/ghostel-send-line-and-go`
- `lx/vterm-send-paragraph` → `lx/ghostel-send-paragraph`
- `lx/vterm-send-paragraph-and-go` → `lx/ghostel-send-paragraph-and-go`
- `lx/vterm-send-region` → `lx/ghostel-send-region`
- `lx/vterm-send-region-and-go` → `lx/ghostel-send-region-and-go`
- `lx/vterm-send-babel-block` → `lx/ghostel-send-babel-block`
- `lx/vterm-send-babel-block-and-go` → `lx/ghostel-send-babel-block-and-go`
- `lx/find-vterm-buffer` → `lx/find-ghostel-buffer`：检查 major mode 为 `ghostel-mode` 及其派生模式（`zsh-ghostel-mode`、`pry-ghostel-mode`、`ssh-zsh-ghostel-mode`）
- `send-to-vterm-mode` → `send-to-ghostel-mode`：minor mode，lighter 改为 `" Send to ghostel"`

#### zsh-ghostel.el（原 zsh-vterm.el）

功能：自定义 zsh 终端主模式。

迁移要点：
- `zsh-vterm-mode` → `zsh-ghostel-mode`（派生自 `ghostel-mode`）
- `lx/run-in-zsh-vterm` → `lx/run-in-zsh-ghostel`
- `zsh-vterm` → `zsh-ghostel`：创建 zsh ghostel session
- `zsh-vterm--internal` → `zsh-ghostel--internal`
- Buffer 名：`*zsh-ghostel*`
- Navigation 函数（previous/next command/output）→ 利用 ghostel 的 `ghostel-next-prompt` / `ghostel-previous-prompt`（OSC 133），保留自定义逻辑作为补充
- 自定义 yank operator → 基于 evil-ghostel 的 operator 框架扩展
- Copilot tab completion → 适配 ghostel 的 line-mode
- `zsh-vterm-get-current-line` → `zsh-ghostel-get-current-line`
- CLI 导航函数全部重命名

### 次核心包

#### zsh-ghostel-ssh.el（原 zsh-vterm-ssh.el）

迁移要点：
- `ssh-zsh-vterm-mode` → `ssh-zsh-ghostel-mode`（派生自 `ghostel-mode`）
- `ssh-zsh-vterm` → `ssh-zsh-ghostel`
- `lx/run-ssh-in-zsh-vterm` → `lx/run-ssh-in-zsh-ghostel`
- 利用 ghostel 内置 TRAMP 支持 + 自动 terminfo 安装
- Helm 集成中的 buffer 过滤改为匹配 `ssh-zsh-ghostel-mode`

#### pry-ghostel.el（原 pry-vterm.el）

迁移要点：
- `pry-vterm-mode` → `pry-ghostel-mode`（派生自 `ghostel-mode`）
- `lx/run-in-pry-vterm` → `lx/run-in-pry-ghostel`
- `pry-vterm` → `pry-ghostel`
- `pry-vterm-get-current-line` → `pry-ghostel-get-current-line`
- Copilot 集成适配

### 领域专用包

统一迁移模式：
1. 文件重命名：`vterm-*.el` → `ghostel-*.el`
2. `vterm-send-string` → `ghostel-send-string`
3. `vterm-mode` 派生 → `ghostel-mode` 派生
4. Buffer 命名中 `vterm` → `ghostel`
5. `lx/run-in-vterm` 调用 → `lx/run-in-ghostel` 调用

涉及文件：
- `vterm-maven.el` → `ghostel-maven.el`
- `vterm-jenkins.el` → `ghostel-jenkins.el`
- `vterm-mitmproxy.el` → `ghostel-mitmproxy.el`
- `vterm-rails.el` → `ghostel-rails.el`
- `vterm-vrl.el` → `ghostel-vrl.el`
- `vterm-prize.el` → `ghostel-prize.el`
- `vterm-arql.el` → `ghostel-arql.el`

## 配置层迁移

### .spacemacs

| 原配置 | 新配置 |
|--------|--------|
| `shell-default-shell 'vterm` | `shell-default-shell 'ghostel` |
| `claude-code-ide-terminal-backend 'vterm` | `claude-code-ide-terminal-backend 'ghostel` |
| `vterm-eval-cmds '(...)` | `ghostel-eval-cmds '(...)` |
| `vterm-max-scrollback 10000` | `ghostel-max-scrollback 10000` |
| `vterm-keymap-exceptions '(...)` | `ghostel-keymap-exceptions '(...)` |
| `(make-shell-pop-command "zsh-vterm" zsh-vterm)` | `(make-shell-pop-command "zsh-ghostel" zsh-ghostel)` |
| `(add-hook 'prog-mode-hook 'send-to-vterm-mode)` | `(add-hook 'prog-mode-hook 'send-to-ghostel-mode)` |
| `spacemacs-evil-collection-allowed-list '(... zsh-vterm pry-vterm)` | `'(... zsh-ghostel pry-ghostel)` |
| `docker-run-async-with-buffer-vterm` | 自制 `docker-run-async-with-buffer-ghostel` |

### package-hooks/ghostel-mode.el（新建）

替代 `vterm-mode.el`，包含：
- `ghostel-mode` 的自定义键绑定
- 进程 sentinel
- 拖拽支持
- Evil 状态管理（配合 evil-ghostel）
- RVM 激活 hook
- `ghostel-kill-buffer-on-normal-exit` 变量

### funcs/ 变更

- `funcs/core.el`：`lx/is-remote-buffer` 中 `ssh-zsh-vterm-mode` → `ssh-zsh-ghostel-mode`
- `funcs/init.el`：autoload 从 `send-to-vterm` → `send-to-ghostel`
- `send-to-vterm-mode` → `send-to-ghostel-mode` 定义
- `send-to-vterm-mode-keymap` → `send-to-ghostel-mode-keymap`

### 快捷键

`key-bindings/global-set-key.el` 中所有 `lx/run-in-vterm` 调用 → `lx/run-in-ghostel`。键位本身不变。

## Shell 侧迁移（.zshrc）

### 删除的函数

```zsh
vterm_printf() {...}       # ghostel shell integration 自动注入
vterm_cmd() {...}           # 替换为 ghostel_cmd 或通过 ghostel-eval-cmds
vterm_set_directory() {...} # ghostel OSC 7 自动跟踪
```

### 删除的 hooks

```zsh
add-zsh-hook -Uz chpwd (){ vterm_set_directory }
add-zsh-hook -Uz precmd (){ vterm_set_directory }
```

### 保留并适配的函数

```zsh
# me() / sme() → 改用 ghostel_cmd
me() { ghostel_cmd find-remote-file "$PWD/$file" "$HOST" }
sme() { ghostel_cmd sudo-find-remote-file "$PWD/$file" "$HOST" }

# download / upload → 改用 ghostel_cmd
download() { ghostel_cmd download "$PWD/$file" }
upload() { ghostel_cmd upload "$PWD" }
```

### 历史记录同步适配

```zsh
HISTDB_VTERM_SESSION → HISTDB_GHOSTEL_SESSION
save_history_to_vterm → save_history_to_ghostel（通过 ghostel_cmd）
update_history_outcome_to_vterm → update_history_outcome_to_ghostel（通过 ghostel_cmd）
```

## ghostel-eval-cmds 注册清单

需要在 `.spacemacs` 的 `ghostel-eval-cmds` 中注册以下命令：

```elisp
(setq ghostel-eval-cmds '(("find-file" find-file)
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

注：ghostel 的 eval-cmds 使用 OSC 51 协议，与 vterm 相同。命令名中的 `vterm-clear-scrollback` 改为 `ghostel-clear-scrollback`。需要在 ghostel 源码中确认 `ghostel-eval-cmds` 的确切变量名和格式。

## 执行顺序

### 阶段一：核心基础设施

1. **备份**：将所有 vterm 相关文件复制到 `~/.config/emacs-config/crafts/backup-vterm/`
2. **run-in-ghostel.el**：创建核心运行命令库
3. **send-to-ghostel.el**：创建代码发送库
4. **ghostel-eval-cmds 注册**：在 `.spacemacs` 中配置所有自定义命令
5. **验证**：`emacsclient -e` 测试基本功能

### 阶段二：终端模式

6. **zsh-ghostel.el**：主 zsh 终端模式
7. **pry-ghostel.el**：Ruby REPL 模式
8. **zsh-ghostel-ssh.el**：SSH 终端模式
9. **package-hooks/ghostel-mode.el**：全局 hook 配置
10. **验证**：测试所有终端模式

### 阶段三：领域专用包

11. **ghostel-maven.el**
12. **ghostel-jenkins.el**
13. **ghostel-mitmproxy.el**
14. **ghostel-rails.el**
15. **ghostel-vrl.el**
16. **ghostel-prize.el**
17. **ghostel-arql.el**

### 阶段四：配置层

18. **funcs/core.el** 更新
19. **funcs/init.el** autoload 更新
20. **key-bindings/global-set-key.el** 更新
21. **.spacemacs** 变量更新
22. **shell-pop 集成** 适配

### 阶段五：Shell 侧

23. **.zshrc** 更新（删除 vterm 函数，适配 ghostel_cmd）
24. **验证**：在 ghostel 中测试所有 shell 功能

## 潜在问题和兼容性风险

### 已知风险

| 风险项 | 严重程度 | 说明 | 缓解方案 |
|--------|----------|------|----------|
| 光标样式自定义 | 中 | vterm 中通过 vterm_printf 设置光标。Ghostel 的 cursor API 不同 | 使用 ghostel 的 cursor 样式设置或 escape sequence |
| shell-pop 弹窗 | 中 | `make-shell-pop-command` 绑定到 vterm。Ghostel 无原生 shell-pop | 在 ghostel-mode.el 中定义 ghostel-pop 函数 |
| Docker vterm 后端 | 低 | `docker-run-async-with-buffer-vterm` 是 docker.el 内置选项 | 自定义 `docker-run-async-with-buffer-ghostel` |
| vterm history search | 低 | Spacemacs 的 helm-vterm-search-history / counsel-vterm-search-history | 利用 ghostel 的 line-mode history 或自制 |
| multi-vterm | 低 | 多终端管理 | ghostel 自带 prefix arg + ghostel-other |
| claude-code-ide-terminal-backend | 低 | Claude Code IDE 可能不支持 ghostel 后端 | 确认 claude-code-ide 是否支持自定义后端 |
| ghostel-eval-cmds 格式 | 中 | 需确认与 vterm-eval-cmds 格式是否完全兼容 | 阅读 ghostel 源码确认 |

### 需要源码确认的功能

1. **ghostel-eval-cmds 变量名和格式**：确认是否与 vterm-eval-cmds 格式一致
2. **ghostel-send-string 行为差异**：是否需要 `\n` 后缀（vterm-send-string 不自动添加换行）
3. **ghostel-mode 派生模式支持**：确认 `define-derived-mode` 从 ghostel-mode 派生是否可行
4. **ghostel process sentinel**：确认是否有等价于 vterm 的 sentinel 机制
5. **ghostel buffer-local 变量**：确认有哪些 buffer-local 变量可用于自定义

### 如果迁移失败

由于旧文件已备份到 `backup-vconfig/`，可以快速回滚：
1. 恢复备份文件
2. 恢复 `.spacemacs` 和 `.zshrc` 的 git 版本
3. 重新加载 Emacs 配置
