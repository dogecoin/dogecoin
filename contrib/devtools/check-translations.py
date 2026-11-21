#!/usr/bin/env python3
"""
Enhanced Translation Integrity Checker
Optimized for CI/CD, performance, and deeper XML validation.
"""

import re
import sys
import os
import io
import xml.etree.ElementTree as ET
from typing import Set, List, Tuple

# Configuration
SOURCE_LANG = 'bitcoin_en.ts'
LOCALE_DIR = 'src/qt/locale'
# Control characters that break XML parsers (excluding valid whitespace)
XML_INVALID_RE = re.compile(b'[\x00-\x08\x0b\x0c\x0e-\x1f]')

def find_format_specifiers(s: str) -> List[str]:
    """Finds all format specifiers following the '%' character."""
    return re.findall(r'%(.)', s)

def split_format_specifiers(specifiers: List[str]) -> Tuple[Set[str], List[str]]:
    """Separates Qt numeric specifiers from standard printf specifiers."""
    numeric = {s for s in specifiers if s.isdigit() and s != '0'}
    # If numeric (Qt) is found, standard printf specifiers in the same string are ignored
    other = [] if numeric else [s for s in specifiers]
    return numeric, other

def check_format_specifiers(source: str, translation: str, numerus: bool) -> List[str]:
    """Validates if translation specifiers match source specifiers."""
    errors = []
    try:
        source_numeric, source_other = split_format_specifiers(find_format_specifiers(source))
        trans_numeric, trans_other = split_format_specifiers(find_format_specifiers(translation))
    except Exception as e:
        return [f"Regex error: {str(e)}"]

    # Source should not mix Qt and printf styles
    if source_numeric and source_other:
        errors.append(f"CRITICAL: Mixed format styles in source: {source}")

    if source_numeric != trans_numeric or source_other != trans_other:
        # Special case: allow omitting %n in numerus forms
        if numerus and source_other == ['n'] and not trans_other and '%' not in translation:
            return []
        errors.append(f"Mismatch: Source {source_numeric or source_other} vs Translation {trans_numeric or trans_other}")
    
    return errors

def check_all_translations():
    print(f'Checking translations in {LOCALE_DIR}...')
    have_errors = False

    if not os.path.exists(LOCALE_DIR):
        print(f"Directory {LOCALE_DIR} not found!")
        return True

    for filename in os.listdir(LOCALE_DIR):
        if not filename.endswith('.ts') or filename == SOURCE_LANG:
            continue

        filepath = os.path.join(LOCALE_DIR, filename)
        with open(filepath, 'rb') as f:
            data = f.read()

        # Security: Search entire file for invalid XML control characters
        if XML_INVALID_RE.search(data):
            print(f'Error: {filename} contains invalid XML control characters.')
            have_errors = True
            continue

        try:
            tree = ET.parse(io.BytesIO(data))
            root = tree.getroot()
        except ET.ParseError as e:
            print(f"Error: {filename} is not valid XML: {e}")
            have_errors = True
            continue

        for message in root.findall('.//message'):
            source_node = message.find('source')
            trans_node = message.find('translation')
            if source_node is None or trans_node is None: continue

            source_text = source_node.text or ""
            numerus = message.get('numerus') == 'yes'
            
            # Extract forms to check
            forms = [n.text for n in trans_node.findall('numerusform')] if numerus else [trans_node.text]
            
            for translation in forms:
                if translation is None: continue
                specifier_errors = check_format_specifiers(source_text, translation, numerus)
                for err in specifier_errors:
                    print(f"[{filename}] {err} | Context: {source_text[:30]}...")
                    have_errors = True

    return have_errors

if __name__ == '__main__':
    # Ensure execution from root
    if not os.path.exists('.git'):
        print('Error: Run this script from the repository root.', file=sys.stderr)
        sys.exit(1)
        
    if check_all_translations():
        print("Build failed: Translation errors detected.")
        sys.exit(1)
    else:
        print("Success: All translations are valid.")
