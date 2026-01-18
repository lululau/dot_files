---
name: vision_mcp
description: 当模型不支持图像输入时，或当从剪切板读取数据时，使用MCP工具处理图像内容并将其转换为结构化文本。
license: MIT
compatibility: opencode
metadata:
  mcp_server: zai
  category: vision
---

## What I do

当模型不支持图像输入时，应该使用这些 MCP 工具中的一个来处理图像内容并将其转换为结构化文本，然后将结果发送回模型以进行进一步处理。

这些 MCP 工具都要求图像文件路径作为输入。你可以使用下面介绍的 `clipboard-image.sh` 脚本从剪切板获取图像并将其保存为文件。

使用完临时图像文件后，建议删除它以节省空间。

| Tool                           | Use Case                                                       |
|--------------------------------|----------------------------------------------------------------|
| `ui_to_artifact`               | UI screenshots → code, prompts, design specs, or descriptions |
| `extract_text_from_screenshot` | OCR for code, terminal output, docs, general text              |
| `diagnose_error_screenshot`    | Error dialogs, stack traces, logs → diagnosis + fix           |
| `understand_technical_diagram` | Architecture diagrams, flowcharts, UML, ER diagrams            |
| `analyze_data_visualization`   | Dashboards, charts → trends, anomalies, insights              |
| `ui_diff_check`                | Compare two UI screenshots for visual differences              |
| `analyze_image`                | General-purpose image analysis (fallback)                      |
| `analyze_video`                | Video content analysis (MP4/MOV/M4V, max 8MB)                  |

## Getting Image from Clipboard

使用 `clipboard-image.sh` 脚本将剪切板图像保存为文件：

```bash
# 获取图像文件路径
IMAGE_PATH="$(~/.config/opencode/skill/vision_mcp/clipboard-image.sh)"

# 将路径传递给 MCP 工具
mcp__zai__extract_text_from_screenshot({
  image_source: "$IMAGE_PATH",
  prompt: "Extract all text"
})
```

**脚本说明：**
- 图像保存路径：`/tmp/opencode-clipboard-images/clipboard-{时间戳}.png`
- 如果剪切板无图像，脚本返回错误
- 脚本位置：`~/.config/opencode/skill/vision_mcp/clipboard-image.sh`
