---
description: 为软件项目生成描述性英文名称（基于功能）
arguments:
  - name: description
    description: 对项目功能的简要描述
    required: true
---

请根据以下命名原则，为用户的软件项目生成10个合适的英文名称，每个名称都要有简短的解释。

## 核心命名原则

### 1. 功能可识别性
名称应直接或间接体现项目核心功能，让用户看到名字能大致推测用途

### 2. 创意缩写（Portmanteau）
将多个词融合创造新词，既保持含义又提高独特性
- `finsight` (financial + insight) - 金融洞察
- `synchro` (sync + chrono) - 时间同步
- `parsify` (parse + simplify) - 解析简化
- `logvent` (log + event) - 日志事件
- `datamine` (data + mine) - 数据挖掘

### 3. 同义词替换
使用不常用的同义词替代直白词汇，增加独特性：
| 直白词 | 替代词 |
|--------|--------|
| watch | sentinel, warden, guard, scout |
| send | dispatch, courier, relay, ferry |
| store | archive, vault, cache, hoard |
| find | detect, locate, spot, discover |
| manage | orchestrate, govern, steer, pilot |

### 4. 唯一性优先
避免过于普遍的组合（如 `log-monitor`, `error-notifier`），通过以下方式增强独特性：
- 添加独特前缀/后缀：`chronolog`, `errata`
- 使用拉丁/希腊词根：`omniscan`, `pathfinder`
- 组合意外词汇：`ironlog`, `swiftwatch`

### 5. 功能性后缀（可选）
- 处理类：`-processor`, `-parser`, `-refiner`
- 管理类：`-orchestrator`, `-pilot`, `-governor`
- 验证类：`-vetter`, `-scout`, `-discerner`
- 服务类：`-service`, `-daemon`, `-agent`

## 好的命名示例
- `finsight` - 金融数据洞察工具
- `sentineld` - 守护进程监控
- `logvent` - 日志事件收集
- `errata` - 错误追踪系统
- `chronolog` - 时间序列日志
- `omniparse` - 通用解析器
- `vaultward` - 保险库管理
- `pathscout` - 路径发现工具

## 避免的命名
- 过于直白普遍：`log-monitor`, `error-notifier`, `file-manager`
- 神话生物：`Pegasus`, `Phoenix`, `Dragon`
- 动漫角色：`Naruto`, `Goku`, `Pikachu`
- 完全随机：`Zephyr`, `Melody`, `Whisper`

---

## 用户项目描述

{{ description }}

请输出10个命名建议，格式为：
1. **名称** - 解释
