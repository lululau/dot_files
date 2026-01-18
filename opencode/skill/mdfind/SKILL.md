---
name: mdfind
description: Efficient macOS system-wide file search using Spotlight index
license: MIT
compatibility: opencode
metadata:
  audience: agents
  workflow: file search
---

## What I do

- Perform instant system-wide file searches on macOS
- Search files by metadata (type, date, size, author, etc.)
- Filter searches by file attributes and content
- Integrate with macOS tools for batch operations

## When to use me

Use this when you need to:
- Search across the entire macOS filesystem or large directories
- Find files by metadata attributes (creation date, file type, size, etc.)
- Perform performance-critical searches where speed is essential

**CRITICAL**: Never use me for hidden directories (files/directories starting with `.`) or git repositories.

---

# mdfind - macOS Spotlight Search

## Description

`mdfind` is the command-line interface to macOS Spotlight's metadata store. It provides extremely fast file searching across the entire filesystem by leveraging indexed metadata. It's significantly faster than traditional tools like `find`, `ripgrep`, or `grep` for system-wide and large directory searches.

**Key Advantage**: mdfind searches are instant because they query a pre-built index, not the filesystem directly.

## When to Use

Use mdfind when:

- **System-wide searches**: Need to find files anywhere on macOS
- **Large directory trees**: Searching non-git repositories with thousands of files
- **Metadata-based searches**: Need to filter by file type, creation date, modification date, author, etc.
- **Multi-location searches**: Searching across multiple directories or the entire filesystem
- **Performance-critical searches**: When speed is essential (e.g., searching ~/Downloads, ~/Documents)
- **Fuzzy content searches**: Searching file contents when exact text matching isn't required
- **Finding files by attributes**: When you know file attributes (kind, date, size) but not exact name

## When NOT to Use

### HARD BLOCKS (Never Use mdfind for These)

- **Hidden directories**: mdfind CANNOT search in hidden directories (files/directories starting with `.`)
  - This includes `.git`, `.config`, `.env`, `.npm`, etc.
  - This is a fundamental limitation of Spotlight indexing
  - **For hidden directory searches**: Use `find` or `ripgrep`

- **Git repositories**: Git repos often contain hidden files and version-specific content
  - Use `git` tools, `ripgrep`, or `find` instead
  - mdfind may miss untracked or recently added files

### Soft Blocks (Use Other Tools When)

- **Unindexed files**: Files recently created or in temporary directories
  - mdfind only searches Spotlight's index; unindexed files won't appear

- **Build artifacts and temp directories**: Spotlight excludes many build/cache directories
  - Use `find` or `ripgrep` for these locations

- **Version control operations**: When searching git history or untracked files
  - Use `git grep`, `git ls-files`, or git-specific tools

## Core Concepts

### How mdfind Works

1. **Index-based search**: Queries Spotlight's metadata store (instant results)
2. **Metadata-rich**: Searches file attributes, not just names
3. **Natural language**: Supports natural language queries (similar to Spotlight UI)
4. **Spotlight integration**: Uses the same index as macOS Spotlight

### Query Language

mdfind uses Spotlight's natural language query format:

- Simple terms: `mdfind "report"`
- Multiple terms: `mdfind "project report"`
- Specific attributes: `mdfind "kMDItemKind == '文件夹'"`
- Boolean operators: `mdfind "(kMDItemFSName == '*test*') && (kMDItemKind == '文件夹')"`

### Common Metadata Attributes

| Attribute | Description | Example |
|-----------|-------------|---------|
| `kMDItemKind` | File type/folder | `"PNG图像"`, `"文件夹"`, `"PDF文稿"` |
| `kMDItemFSName` | Filename | `"filename.txt"` |
| `kMDItemContentCreationDate` | Creation date | `$time.today(-7)` |
| `kMDItemContentModificationDate` | Modification date | `$time.today(-1)` |
| `kMDItemFSSize` | File size | `$file.size > 1024` |
| `kMDItemAuthorEmailAddresses` | Author email | `"user@example.com"` |
| `kMDItemPixelWidth` | Image width | `1920` |
| `kMDItemDurationSeconds` | Audio/video duration | `300` |

## Common Patterns

### Basic File Name Search

```bash
# Find files by name
mdfind "filename"
mdfind "myproject"

# Case-insensitive (default behavior)
mdfind "MyProject"  # finds myproject, MyProject, MYPROJECT

# Search in specific directory (limit scope)
mdfind -onlyin ~/Documents "report"
mdfind -onlyin ~/Downloads "installer"
```

### Search by File Type

```bash
# Find all PDFs
mdfind "kMDItemKind == 'PDF文稿'"

# Find all images
mdfind "kMDItemKind == '*图*'"

# Find all folders
mdfind "kMDItemKind == '文件夹'"
```

### Search by Date

```bash
# Files modified in the last 24 hours
mdfind "kMDItemContentModificationDate > \$time.today(-1)"

# Files created in the last 7 days
mdfind "kMDItemContentCreationDate > \$time.today(-7)"

# Files modified between two dates
mdfind "kMDItemContentModificationDate > \$time.today(-30) && kMDItemContentModificationDate < \$time.today(-7)"

# Files from specific date
mdfind "kMDItemContentModificationDate == \$time.today(0)"
```

kMDItemContentModificationDate, kMDItemContentCreationDate 等字段接收的是距离 2001-01-01 00:00:00 GMT 的秒数，
如果需要的计算的时间精度比天小，则可以通过 date 命令计算：

```bash
# Calculate time difference in seconds
BASE_TIME=$(date -j -f "%Y-%m-%d %H:%M:%S %z" "2001-01-01 00:00:00 +0000" +%s)
CURRENT_TIME=$(date +%s)
VALUE_FOR_CURRENT_TIME=$(expr $CURRENT_TIME - $BASE_TIME)
VALUE_FOR_ONE_HOUR_AGO=$(expr $VALUE_FOR_CURRENT_TIME - 3600)
```


### Search by File Size

```bash
# Files larger than 1MB
mdfind "kMDItemFSSize > 1048576"

# Files smaller than 10KB
mdfind "kMDItemFSSize < 10240"

# Files between 1MB and 100MB
mdfind "kMDItemFSSize > 1048576 && kMDItemFSSize < 104857600"

# Find large files (useful for cleanup)
mdfind "kMDItemFSSize > 104857600"  # Files > 100MB
```

### Search by Content

```bash
# Search file contents (supports natural language)
mdfind "kMDItemTextContent == 'important meeting notes'cdw"
mdfind "kMDItemTextContent == 'configuration file database'cdw"

# Note: Content search is not as precise as ripgrep/grep
```

### Boolean Logic

```bash
# AND operation
mdfind "(kMDItemKind == 'PDF文稿') && (kMDItemFSName == '*report*')"

# OR operation
mdfind "(kMDItemKind == 'PDF文稿') || (kMDItemKind == '*Document*')"

# NOT operation
mdfind "kMDItemFSName == '*project*' && !(kMDItemFSName == '*test*')"

# Complex queries
mdfind "((kMDItemKind == 'PNG图像' || kMDItemKind == 'JPEG图像') && (kMDItemPixelWidth > 1920)) || (kMDItemFSName == '*screenshot*')"
```

### Real-World Examples

```bash
# Find all Python scripts in Documents
mdfind -onlyin ~/Documents "kMDItemKind == '*Python Source*'cdw"

# Find recent screenshots (modified in last hour)
BASE_TIME=$(date -j -f "%Y-%m-%d %H:%M:%S %z" "2001-01-01 00:00:00 +0000" +%s)
CURRENT_TIME=$(date +%s)
VALUE_FOR_CURRENT_TIME=$(expr $CURRENT_TIME - $BASE_TIME)
VALUE_FOR_ONE_HOUR_AGO=$(expr $VALUE_FOR_CURRENT_TIME - 3600)
mdfind "kMDItemFSName == '*.png'cdw && kMDItemContentCreationDate > $VALUE_FOR_ONE_HOUR_AGO"

# Find all large video files in Downloads
mdfind -onlyin ~/Downloads "kMDItemFSSize > 104857600 && kMDItemKind == '*影片*'"

# Find duplicate files by size (search, then manually dedupe)
mdfind "kMDItemFSSize == 1048576"
```

## Performance Notes

### Performance Comparison

| Tool | 10,000 files | 100,000 files | 1M+ files | Search Type |
|------|--------------|----------------|-----------|-------------|
| mdfind | <1s | <1s | <1s | Indexed metadata |
| find | 2-5s | 20-30s | 2-5 min | Filesystem traversal |
| ripgrep | 3-5s | 30-60s | 5-10 min | Content search |
| grep | 10-20s | 2-5 min | 20-30 min | Content search |

**Key Insight**: mdfind's performance is constant regardless of dataset size because it queries a pre-built index.

### Performance Characteristics

- **Speed**: Instant results (sub-second for virtually any query)
- **Scaling**: Constant time complexity (O(1)) for search operations
- **Memory**: Minimal memory usage (queries the Spotlight daemon)
- **Index update**: Files appear in search as Spotlight indexes them (usually seconds to minutes)

### When Performance Matters

Use mdfind when:
- Searching user directories (~/Documents, ~/Downloads, ~/Desktop)
- Quick lookups before running more intensive searches
- Searching across multiple volumes/drives
- Performance is blocking other operations

Avoid mdfind when:
- Searching git repositories (use ripgrep)
- Searching hidden directories (use find)
- Working with recently created/unindexed files

## Best Practices

### DO

1. **Use mdfind for system-wide searches** - It's incredibly fast
2. **Limit scope with `-onlyin`** - Reduces noise for targeted searches
3. **Combine with metadata filters** - More precise than simple text searches
4. **Use natural language queries** - Often easier to write and understand
5. **Inspect metadata with mdls** - Verify what attributes are available
6. **Fall back to find/ripgrep when needed** - Especially for hidden directories

### DON'T

1. **Never use mdfind for hidden directories** - It won't work, period
2. **Don't rely on mdfind for git repos** - Use git tools instead
3. **Don't assume immediate indexing** - New files may not appear immediately
4. **Don't use for exact content matching** - ripgrep is better for that
5. **Don't use in cross-platform scripts** - mdfind is macOS-only
6. **Don't ignore privacy settings** - Spotlight may exclude certain directories

## Troubleshooting

### mdfind Returns No Results

**Cause**: Files not indexed by Spotlight
**Solution**:
```bash
# Force Spotlight to reindex a directory
sudo mdutil -E /path/to/directory

# Check indexing status
mdutil -s /
```

### Files Don't Appear in Search

**Cause**: File in excluded location or temporary directory

### Query Returns Too Many Results

**Solution**: Add more filters or use `-onlyin` to scope
```bash
# Too broad
mdfind "report"

# More specific
mdfind "kMDItemKind == 'PDF文稿' && kMDItemFSName == '*report*'"
mdfind -onlyin ~/Documents "report"
```

## Common File Types (From mfd's KIND_MAP)

**Extensions to Kinds Mapping (from mfd's KIND_MAP):**

| Extension | kMDItemKind Value                         |
|-----------|-------------------------------------------|
| .pdf      | "PDF文稿"                                 |
| .png      | "PNG图像"                                 |
| .jpeg     | "JPEG图像"                                |
| .gif      | "GIF图像"                                 |
| .rb       | "Ruby Source"                             |
| .js       | "JavaScript脚本"                          |
| .css      | "CSS样式表"                               |
| .bash     | "Bourne-Again Shell Script"               |
| .zsh      | "Zsh Script"                              |
| .dmg      | "磁盘映像"                                |
| .zip      | "ZIP归档"                                 |
| .app      | "应用程序"                                |
| .php      | "PHP Script"                              |
| .lua      | "Lua script"                              |
| .swift    | "Swift Source"                            |
| .cpp      | "C++ Source"                              |
| .cc       | "C++ Source"                              |
| .yaml     | "YAML Document"                           |
| .yml      | "YAML Document"                           |
| .markdown | "Markdown Document"                       |
| .md       | "Markdown Document"                       |
| .org      | "Org document"                            |
| .docx     | "Microsoft Word document (.docx)"         |
| .xls      | "Microsoft Excel 97-2004 Workbook (.xls)" |
| .xltx     | "Microsoft Excel Template (.xltx)"        |
| .mp4      | "MPEG-4影片"                              |
| .m4a      | "Apple MPEG-4音频"                        |
| .ogg      | "HTML5 Audio (Ogg)"                       |
| .gz       | "gzip压缩归档"                            |
| .bz2      | "bzip2压缩归档"                           |
| .tgz      | "gzip压缩tar归档"                         |
| .xz       | "xz compressed archive"                   |
| .sqlite   | "SQLite"                                  |
| .db       | "SQLite"                                  |
| .ics      | "ICS文件"                                 |
| .epub     | "电子出版物(EPUB)"                        |
| .xml      | "XML Document"                            |
| .hs       | "Haskell Source File"                     |
| .s        | "Assembly Source"                         |
| .el       | "Emacs Lisp Source File"                  |
| .java     | "Java Source File"                        |
| .class    | "Java类文件"                              |
| .py       | "Python Source"                           |
| .go       | "Go Source File"                          |
| .rs       | "Rust Source File"                        |

## Summary

- **Use mdfind for**: Fast system-wide searches, metadata-based searches, large directories (non-git)
- **Never use mdfind for**: Hidden directories, git repositories, exact content searches
- **Speed**: Instant results due to indexed search
- **Limitations**: Cannot search hidden files, requires Spotlight indexing
- **Best practice**: Start with mdfind for quick lookups, fall back to find/ripgrep when needed
- **Production patterns**: Use `cdw` modifiers for fuzzy matching, combine criteria with `&&`, use `-0` for safe piping

Remember: **mdfind is a powerful tool for macOS-specific workflows, but always be aware of its limitations, especially regarding hidden directories. The mfd gem demonstrates production-grade usage patterns that scale to complex queries.**
