#!/bin/bash
# 将剪切板图像保存到文件并返回文件路径

set -e

# 创建临时目录
TEMP_DIR="${TMPDIR:-/tmp}/opencode-clipboard-images"
mkdir -p "$TEMP_DIR"

# 生成唯一文件名（时间戳 + 随机数）
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
RANDOM_SUFFIX=$(head -c 4 /dev/urandom | od -A n -t x1 | tr -d ' \n')
FILEPATH="$TEMP_DIR/clipboard-${TIMESTAMP}-${RANDOM_SUFFIX}.png"

# 使用 pngpaste 保存剪切板图像
if pngpaste "$FILEPATH" 2>/dev/null; then
    # 成功：输出文件路径
    echo "$FILEPATH"
else
    # 失败：检查剪切板是否包含图像
    echo "Error: No image in clipboard or pngpaste failed" >&2
    exit 1
fi
