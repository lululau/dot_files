---
name: google-search-image
description: Search Google Images and extract image URLs using browser automation
license: MIT
compatibility: opencode
metadata:
  audience: agents
  workflow: web scraping
---

## What I do

- Automate Google Images searches via browser automation
- Extract image URLs (thumbnail and full-size) from search results
- Support multiple search queries and result filtering
- Return structured lists of image URLs for further processing

## When to use me

Use this when you need to:
- Find images from Google Images search results
- Extract image URLs for documentation, research, or processing
- Get multiple image URLs for a given search query
- Access Google Images search results programmatically

**CRITICAL**: Always close the browser after extracting URLs to free resources.

---

# Google Image Search with agent-browser

## Description

This skill uses `agent-browser` to automate Google Images searches and extract image URLs from the search results. It provides a programmatic way to access Google's image search and retrieve URLs for further processing.

**Key Features**:
- Automated search queries on Google Images
- Extract thumbnail URLs (encrypted-tbn0.gstatic.com)
- Returns first N image URLs (configurable count)
- Clean resource management (closes browser automatically)

## When to Use

Use this skill when:
- **Need image URLs**: You need URLs of images from Google Images
- **Batch image collection**: Collecting multiple images for research, documentation, or datasets
- **Programmatic access**: Automating image search without manual browsing
- **URL extraction**: Getting URLs for image processing, downloading, or analysis

## When NOT to Use

- **High-resolution images**: This returns thumbnail URLs; full-size requires different approach
- **Alternative services**: Use dedicated image APIs (Unsplash, Pexels, etc.) for production
- **Rate-limited scenarios**: Google may block excessive automated requests
- **Copyright concerns**: Always check image licenses before use

## Core Workflow

### Basic Search Flow

```bash
# 1. Open Google Images
agent-browser open https://www.google.com/imghp

# 2. Fill search query
agent-browser fill @e3 "your_search_term"

# 3. Submit search
agent-browser press Enter

# 4. Wait for results to load
agent-browser wait --load networkidle

# 5. Extract image URLs
agent-browser eval "const images = document.querySelectorAll('img'); const urls = []; images.forEach(img => { if (img.src && img.src.includes('encrypted-tbn0')) { urls.push(img.src); } }); urls.slice(0, 1).join('\n')"

# 6. Close browser (CRITICAL)
agent-browser close
```

### Step-by-Step Explanation

**Step 1**: Open Google Images homepage
- Navigates to `https://www.google.com/imghp`
- Loads the search interface

**Step 2**: Fill search box
- Uses `@e3` reference (search input field)
- Replace `"your_search_term"` with your search query

**Step 3**: Submit search
- Presses Enter to submit the search
- Triggers Google Images search

**Step 4**: Wait for results
- Waits until network is idle
- Ensures all thumbnails are loaded

**Step 5**: Extract URLs
- Finds all `<img>` elements on the page
- Filters for `encrypted-tbn0.gstatic.com` URLs (thumbnails)
- Returns first 1 URLs (adjust `slice(0, 1)` as needed)
- URLs are newline-separated for easy parsing

**Step 6**: Clean up
- Closes the browser session
- Frees system resources

## Common Patterns

### Basic Image Search

```bash
# Search for "obsidian" and get 1 image URLs
agent-browser open https://www.google.com/imghp
agent-browser fill @e3 "obsidian"
agent-browser press Enter
agent-browser wait --load networkidle
agent-browser eval "const images = document.querySelectorAll('img'); const urls = []; images.forEach(img => { if (img.src && img.src.includes('encrypted-tbn0')) { urls.push(img.src); } }); urls.slice(0, 1).join('\n')"
agent-browser close
```

### Get More Images

```bash
# Search for "python logo" and get 20 image URLs
agent-browser open https://www.google.com/imghp
agent-browser fill @e3 "python logo"
agent-browser press Enter
agent-browser wait --load networkidle
agent-browser eval "const images = document.querySelectorAll('img'); const urls = []; images.forEach(img => { if (img.src && img.src.includes('encrypted-tbn0')) { urls.push(img.src); } }); urls.slice(0, 20).join('\n')"
agent-browser close
```

### Search with Specific Keywords

```bash
# Search for "nature landscape" images
agent-browser open https://www.google.com/imghp
agent-browser fill @e3 "nature landscape"
agent-browser press Enter
agent-browser wait --load networkidle
agent-browser eval "const images = document.querySelectorAll('img'); const urls = []; images.forEach(img => { if (img.src && img.src.includes('encrypted-tbn0')) { urls.push(img.src); } }); urls.slice(0, 15).join('\n')"
agent-browser close
```

### Extract URLs as JSON Array

```bash
# Get URLs in JSON format for programmatic processing
agent-browser open https://www.google.com/imghp
agent-browser fill @e3 "cats"
agent-browser press Enter
agent-browser wait --load networkidle
agent-browser eval "const images = document.querySelectorAll('img'); const urls = []; images.forEach(img => { if (img.src && img.src.includes('encrypted-tbn0')) { urls.push(img.src); } }); JSON.stringify(urls.slice(0, 10))"
agent-browser close
```

### Search for High-Quality Images

```bash
# Search for "4K wallpaper" to get high-quality image thumbnails
agent-browser open https://www.google.com/imghp
agent-browser fill @e3 "4K wallpaper"
agent-browser press Enter
agent-browser wait --load networkidle
agent-browser eval "const images = document.querySelectorAll('img'); const urls = []; images.forEach(img => { if (img.src && img.src.includes('encrypted-tbn0')) { urls.push(img.src); } }); urls.slice(0, 10).join('\n')"
agent-browser close
```

## URL Format

### Thumbnail URLs

Returned URLs follow this pattern:
```
https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9Gc[HASH]&s
```

**Characteristics**:
- Domain: `encrypted-tbn0.gstatic.com`
- Google's thumbnail service
- Lower resolution than source images
- Faster to load than full-size images
- Good for previews and catalogs

### Limitations

- **Not full-size URLs**: These are thumbnails, not original images
- **No direct download**: Cannot directly download full-resolution images
- **Access restrictions**: Some thumbnails may require cookies/session
- **Rate limiting**: Excessive requests may be blocked by Google

## Best Practices

### DO

1. **Always close browser after extraction** - Prevents resource leaks
2. **Wait for network idle** - Ensures all images are loaded before extraction
3. **Filter for specific domain** - Use `encrypted-tbn0` filter for consistent results
4. **Limit result count** - Adjust `slice()` to avoid overwhelming responses
5. **Handle empty results** - Check if URLs were found before processing
6. **Rate limit requests** - Don't run rapid successive searches to avoid blocking

### DON'T

1. **Don't forget to close browser** - Leads to memory leaks and zombie processes
2. **Don't expect full-resolution images** - Returns thumbnails only
3. **Don't use for commercial projects** - Google's ToS may restrict automated access
4. **Don't ignore rate limits** - Excessive searches may result in CAPTCHAs or blocking
5. **Don't assume all URLs are valid** - Some may expire or require authentication
6. **Don't skip network idle wait** - Results may be incomplete or missing

## Troubleshooting

### No Images Returned

**Cause**: Search didn't return results or filter didn't match

**Solution**:
```bash
# Check if images exist on page (before filter)
agent-browser eval "document.querySelectorAll('img').length"

# Check total image count with filter
agent-browser eval "const images = document.querySelectorAll('img'); let count = 0; images.forEach(img => { if (img.src && img.src.includes('encrypted-tbn0')) { count++; } }); count"
```

### Browser Not Closing

**Cause**: Background process or error in automation

**Solution**:
```bash
# Force close browser
agent-browser close

# If that fails, kill Playwright processes manually
pkill -f playwright
```

### Search Returns Different Results

**Cause**: Google personalizes results based on location/language

**Solution**:
- Search results may vary by session
- Use consistent browser settings if reproducibility is needed
- Consider using Google Custom Search API for consistent results

### CAPTCHA or Blocking

**Cause**: Too many rapid requests

**Solution**:
- Add delays between searches
- Reduce frequency of searches
- Use different search terms or alternative services

## Example: Complete Script

Here's a complete example showing how to search and process results:

```bash
#!/bin/bash

SEARCH_TERM="obsidian"
COUNT=1

echo "Searching Google Images for: $SEARCH_TERM"

# Open Google Images
agent-browser open https://www.google.com/imghp

# Fill search
agent-browser fill @e3 "$SEARCH_TERM"

# Submit
agent-browser press Enter

# Wait for results
agent-browser wait --load networkidle

# Extract URLs
URLS=$(agent-browser eval "const images = document.querySelectorAll('img'); const urls = []; images.forEach(img => { if (img.src && img.src.includes('encrypted-tbn0')) { urls.push(img.src); } }); urls.slice(0, $COUNT).join('\n')")

# Close browser
agent-browser close

# Process results
echo ""
echo "Found image URLs:"
echo "$URLS" | nl

# Optional: Download images
# echo "$URLS" | while read -r url; do
#   wget -O image_$(date +%s).jpg "$url"
# done
```

## Alternative Solutions

### For Production Use

Consider these alternatives instead:

1. **Google Custom Search API**
   - Official, documented API
   - Requires API key and quota management
   - More reliable and rate-limited
   - Full image URLs available

2. **Unsplash API**
   - Free, high-quality images
   - Well-documented API
   - Commercial-friendly licenses
   - Multiple sizes available

3. **Pexels API**
   - Free stock photos and videos
   - Rate-limited but generous
   - Easy to use
   - Attribution-friendly

4. **Pixabay API**
   - Large collection of free images
   - Multiple license types
   - API with search capabilities
   - Good for documentation

### When to Switch

Switch from this skill to alternatives when:
- Need full-resolution images
- Commercial production use
- Consistent results across sessions
- Higher rate limits needed
- Legal/licensing clarity required

## Summary

- **Use this skill for**: Quick image URL extraction from Google Images, prototyping, research
- **Returns**: Thumbnail URLs (encrypted-tbn0.gstatic.com)
- **Limitations**: Not full-size, may be rate-limited, browser-dependent
- **Best practice**: Always close browser, wait for network idle, filter results appropriately
- **Production use**: Consider official APIs (Google Custom Search, Unsplash, etc.)
- **Remember**: Google's ToS may restrict automated access; use responsibly

This skill provides a convenient way to programmatically access Google Images search results, but for production applications or large-scale usage, consider using official image search APIs that provide better reliability, documentation, and terms of service.
