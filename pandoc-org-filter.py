#!/usr/bin/env python3

# Usage: pandoc --filter=pandoc-org-filter.py --columns=$LINE_WIDTH -t markdown -f input.md -t org

from pandocfilters import toJSONFilter, Plain, Para, BlockQuote, DefinitionList, CodeBlock, Header
import re

constructors_map = {
    'Plain': Plain,
    'Para': Para,
    'BlockQuote': BlockQuote,
    'DefinitionList': DefinitionList,
}

def find_code_block(value):
    if isinstance(value, list):
        for e in reversed(value):
            result = find_code_block(e)
            if result:
                return result
        return None
    elif isinstance(value, dict) and value['t'] == 'CodeBlock':
        return CodeBlock(*value['c'])
    elif isinstance(value, dict) and 'c' in value and isinstance(value['c'], list):
        return find_code_block(value['c'])
    else:
        return None


# 1. insert a white space before and after the `Code` element in a paragraph
# 2. insert zero-width space before and after quotes characters if they are at boundaries of a Code element
def code(key, value, format, meta):
    if key in constructors_map and isinstance(value, list):
        new_elemets = []
        elments = value
        for element in elments:
            if isinstance(element, dict) and element['t'] in ['Code', 'Strong', 'Emph']:
                new_elemets.append({'t': 'Space'})
                if len(element['c']) > 1 and isinstance(element['c'][1], str):
                    element['c'][1] = re.sub(r'^"', r'​"', element['c'][1])
                    element['c'][1] = re.sub(r'"$', r'"​', element['c'][1])
                    element['c'][1] = re.sub(r"^'", r"​'", element['c'][1])
                    element['c'][1] = re.sub(r"'$", r"'​", element['c'][1])
                new_elemets.append(element)
                new_elemets.append({'t': 'Space'})
            else:
                new_elemets.append(element)
        return constructors_map[key](new_elemets)
    elif key == 'Table':
        return find_code_block(value)
    elif key == 'Header':
        header_children = [e for e in value[2] if e['t'] != 'Link']
        return Header(value[0], ["", [], []], header_children)
    elif key == 'Div' and isinstance(value, list) and isinstance(value[0], list) and value[0][1] == ["zeroclipboard-container"]:
        return Para([{'t': 'Space'}])
    elif key == 'Link' and isinstance(value, list) and isinstance(value[-1], list) and value[-1][0] == "https://github.com/tefra/null":
        return {'t': 'Space'}
    else:
        return None

if __name__ == "__main__":
    toJSONFilter(code)
