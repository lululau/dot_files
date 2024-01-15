#!/usr/bin/env python3

# Usage: pandoc --filter=pandoc-org-filter.py --columns=$LINE_WIDTH -t markdown -f input.md -t org

from pandocfilters import toJSONFilter, Para
import json
import re

# 1. insert a white space before and after the `Code` element in a paragraph
# 2. insert zero-width space before and after quotes characters if they are at boundaries of a Code element
def code(key, value, format, meta):
    if key == 'Para':
        new_elemets = []
        elments = value
        for element in elments:
            if element['t'] in ['Code', 'Strong', 'Emph']:
                new_elemets.append({'t': 'Space'})
                if len(element['c']) > 1:
                    element['c'][1] = re.sub(r'^"', r'​"', element['c'][1])
                    element['c'][1] = re.sub(r'"$', r'"​', element['c'][1])
                    element['c'][1] = re.sub(r"^'", r"​'", element['c'][1])
                    element['c'][1] = re.sub(r"'$", r"'​", element['c'][1])
                new_elemets.append(element)
                new_elemets.append({'t': 'Space'})
            else:
                new_elemets.append(element)
        return Para(new_elemets)
    else:
        return None

if __name__ == "__main__":
    toJSONFilter(code)
