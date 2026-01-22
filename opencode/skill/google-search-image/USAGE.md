# Google Image Search Skill

## Quick Start

This skill provides automated Google Images search with URL extraction using `agent-browser`.

### Basic Usage

```bash
./google-search-image.sh "search-term" [count]
```

### Examples

```bash
# Search for "obsidian" images (default 10 results)
./google-search-image.sh "obsidian"

# Search for 20 "cats" images
./google-search-image.sh "cats" 20

# Search for 15 "nature landscape" images
./google-search-image.sh "nature landscape" 15
```

### Save URLs to File

```bash
./google-search-image.sh "search-term" > image_urls.txt
```

## Manual Usage (Step-by-Step)

If you prefer to use `agent-browser` directly:

```bash
# 1. Open Google Images
agent-browser open https://www.google.com/imghp

# 2. Get interactive elements
agent-browser snapshot -i

# 3. Fill search box (usually @e3)
agent-browser fill @e3 "your-search-term"

# 4. Submit search
agent-browser press Enter

# 5. Wait for results
agent-browser wait --load networkidle

# 6. Extract image URLs
agent-browser eval "const images = document.querySelectorAll('img'); const urls = []; images.forEach(img => { if (img.src && img.src.includes('encrypted-tbn0')) { urls.push(img.src); } }); urls.slice(0, 10).join('\n')"

# 7. Close browser (IMPORTANT!)
agent-browser close
```

## Output Format

The script outputs:
1. Progress messages (with emojis)
2. Numbered list of image URLs
3. Tips for saving/downloading

### Example Output

```
🔍 Searching Google Images for: obsidian
📊 Extracting first 10 image URLs...

Opening Google Images...
Filling search query...
Submitting search...
Waiting for results to load...
Extracting image URLs...
Closing browser...

✅ Found 10 image URLs:

     1	https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQj1GCTSY1tzs7H-RVagORwnUGE0WFq4HZgEslmzFJ4YVn6X4lhO8t1pI9_&s
     2	https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQeBAlpsPyzYWNEKPL-QwHpmJ4jjjQMCfjcpD91WJ44d3rtwYEhJQfrAHLR&s
     ...

💡 Tip: You can save these URLs to a file:
   ./google-search-image.sh "obsidian" 10 > image_urls.txt

💡 Tip: Download images with:
   cat image_urls.txt | xargs -I {} wget -O image_{#}.jpg {}
```

## URL Format

Returned URLs are Google's thumbnail URLs:
```
https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9Gc[HASH]&s
```

**Note**: These are thumbnail URLs, not full-resolution images.

## Downloading Images

After saving URLs to a file, you can download them:

```bash
# Save URLs first
./google-search-image.sh "cats" 10 > cat_urls.txt

# Download all images
i=1
while read -r url; do
    wget -O cat_$i.jpg "$url"
    ((i++))
done < cat_urls.txt
```

Or use a one-liner:
```bash
./google-search-image.sh "cats" 10 | grep "https://" | xargs -I {} wget -O image_$(date +%s%N).jpg {}
```

## Troubleshooting

### Script Hangs

If the script hangs during execution:
1. Check if browser is stuck on CAPTCHA
2. Try a different search term
3. Kill existing processes: `pkill -f playwright`

### No Results Returned

If the script returns no URLs:
1. Check if Google Images loaded the page
2. Try a different search term
3. Increase wait time (edit script: `agent-browser wait --load networkidle`)

### Browser Not Closing

If the browser doesn't close:
```bash
# Force close
agent-browser close

# Kill all playwright processes
pkill -f playwright
```

## Limitations

1. **Thumbnails only**: Returns thumbnail URLs, not full-resolution images
2. **Rate limiting**: Google may block rapid successive searches
3. **Personalized results**: Results may vary by session/location
4. **CAPTCHA**: Excessive use may trigger CAPTCHA

## Alternatives

For production use, consider:
- **Google Custom Search API**: Official API with documented limits
- **Unsplash API**: Free high-quality images
- **Pexels API**: Free stock photos and videos
- **Pixabay API**: Large collection of free images

## See Also

- `agent-browser` skill: Browser automation
- Full documentation: See `README.md` in this directory
