#!/bin/bash

# Google Image Search Helper Script
# Usage: ./google-search-image.sh "search term" [count]

set -e

DEFAULT_COUNT=10

if [ $# -lt 1 ]; then
    echo "Usage: $0 <search-term> [count]"
    echo ""
    echo "Examples:"
    echo "  $0 \"obsidian\""
    echo "  $0 \"cats\" 15"
    echo "  $0 \"nature landscape\" 20"
    exit 1
fi

SEARCH_TERM="$1"
COUNT="${2:-$DEFAULT_COUNT}"

echo "🔍 Searching Google Images for: $SEARCH_TERM"
echo "📊 Extracting first $COUNT image URLs..."
echo ""

echo "Opening Google Images..."
agent-browser open https://www.google.com/imghp > /dev/null 2>&1

echo "Filling search query..."
agent-browser fill @e3 "$SEARCH_TERM" > /dev/null 2>&1

echo "Submitting search..."
agent-browser press Enter > /dev/null 2>&1

echo "Waiting for results to load..."
agent-browser wait --load networkidle > /dev/null 2>&1

echo "Extracting image URLs..."
URLS=$(agent-browser eval "const images = document.querySelectorAll('img'); const urls = []; images.forEach(img => { if (img.src && img.src.includes('encrypted-tbn0')) { urls.push(img.src); } }); urls.slice(0, $COUNT).join('\n')")

echo "Closing browser..."
agent-browser close > /dev/null 2>&1

echo ""
echo "✅ Found $(echo "$URLS" | grep -c 'http') image URLs:"
echo ""
echo "$URLS" | nl
echo ""

echo "💡 Tip: You can save these URLs to a file:"
echo "   $0 \"$SEARCH_TERM\" $COUNT > image_urls.txt"
echo ""
echo "💡 Tip: Download images with:"
echo "   cat image_urls.txt | xargs -I {} wget -O image_{#}.jpg {}"
