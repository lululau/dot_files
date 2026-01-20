---
name: commit
description: Create git commit following best practices
arguments:
  - name: message
    description: Custom commit message (optional - if not provided, will draft one based on changes)
    required: false
---

# Git Commit

{{- if .Args }}
用户提供了自定义提交信息：{{ .Args }}
{{- else }}
用户未提供自定义提交信息，需要根据更改自动生成。
{{- end }}

## 操作步骤

### 1. 检查 git 状态
运行 `git status` 查看所有未跟踪的文件和修改。

### 2. 查看更改内容
运行以下命令查看更改：
- `git diff` - 查看未暂存的更改
- `git diff --staged` - 查看已暂存的更改（如果有）

### 3. 查看最近的提交历史
运行 `git log -10 --oneline` 查看最近的提交信息，以遵循仓库的提交信息风格。

### 4. 分析更改并起草提交信息

{{- if .Args }}
使用用户提供的提交信息：{{ .Args }}
{{- else }}
根据更改分析并起草提交信息：
- 总结更改的性质（新功能、改进、bug 修复、重构、测试、文档等）
- 确保信息准确反映更改及其目的
- 起草简洁的（1-2 句话）提交信息
- **不要提交可能包含密钥的文件**（如 .env、credentials.json 等），如果发现此类文件请警告用户
{{- end }}

### 5. 暂存相关文件
将相关的未跟踪文件添加到暂存区。

### 6. 创建提交
使用分析后的提交信息创建提交。

### 7. 验证成功
运行 `git status` 确认提交成功。

## 重要注意事项

- **仅在用户明确要求时才创建提交**
- 遵循 Git 安全协议：
  - 永不更新 git config
  - 永不运行破坏性/不可逆的 git 命令（如 push --force、hard reset 等），除非用户明确要求
  - 永不跳过钩子（--no-verify、--no-gpg-sign 等），除非用户明确要求
  - 永不推送到 main/master，警告用户如果他们要求
  - 仅在所有条件满足时使用 git commit --amend：
    1. 用户明确要求 amend，或提交成功但 pre-commit hook 自动修改了需要包含的文件
    2. HEAD 提交是由你在本次对话中创建的（验证：git log -1 --format='%an %ae'）
    3. 提交尚未推送到远程（验证：git status 显示 "Your branch is ahead"）
  - **如果提交失败或被钩子拒绝，切勿 amend - 修复问题并创建新提交**
  - **如果已经推送到远程，切勿 amend，除非用户明确要求（需要强制推送）**
- **如果没有可提交的更改（即没有未跟踪文件和修改），不要创建空提交**
- 如果提交由于 pre-commit hook 失败，修复问题并创建新提交
