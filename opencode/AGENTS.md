## Shell 工具使用规范

### 使用 man 命令查看手册页时要将 PAGER 环境变量设置为 `cat`

## 搜索工具使用规范

### 对于系统范围的搜索任务使用 mdfind SKILL

- 搜索系统范围内的文件和内容时，如需要对系统的所有文件进行全盘搜索时使用 `mdfind` SKILL, 不要使用 find/fd/grep/ripgrep
- 搜索整个用户目录范围的文件和内容时，使用 `mdfind` SKILL, 不要使用 find/fd/grep/ripgrep

## MCP 工具使用规范

### iMCP_reminders_create - 创建提醒事项

**日期时间参数格式要求**：
- `due` 参数必须使用带时区的 ISO 8601 格式
- 固定时区：+08:00
- 正确格式：`YYYY-MM-DDTHH:mm:ss.sss+08:00`

