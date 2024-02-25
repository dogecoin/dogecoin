#!/usr/bin/env python3
# Copyright (c) 2014 Wladimir J. van der Laan
# Copyright (c) 2022 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
'''
Run this script from the root of the repository to check all translations
'''

import re
import sys
import os
import io
import xml.etree.ElementTree as ET

# Name of source language file
SOURCE_LANG = 'bitcoin_en.ts'
# Directory with locale files
LOCALE_DIR = 'src/qt/locale'

def check_at_repository_root():
    if not os.path.exists('.git'):
        print('No .git directory found')
        print('Execute this script at the root of the repository', file=sys.stderr)
        exit(1)

def find_format_specifiers(s):
    '''Find all format specifiers in a string.'''
    pos = 0
    specifiers = []
    while True:
        percent = s.find('%', pos)
        if percent < 0:
            break
        specifiers.append(s[percent+1])
        pos = percent+2
    return specifiers

def split_format_specifiers(specifiers):
    '''Split format specifiers between numeric (Qt) and others (strprintf)'''
    numeric = []
    other = []
    for s in specifiers:
        if s in {'1','2','3','4','5','6','7','8','9'}:
            numeric.append(s)
        else:
            other.append(s)

    # If both numeric format specifiers and "others" are used, assume we're dealing
    # with a Qt-formatted message. In the case of Qt formatting (see https://doc.qt.io/qt-5/qstring.html#arg)
    # only numeric formats are replaced at all. This means "(percentage: %1%)" is valid, without needing
    # any kind of escaping that would be necessary for strprintf. Without this, this function
    # would wrongly detect '%)' as a printf format specifier.
    if numeric:
        other = []

    # numeric (Qt) can be present in any order, others (strprintf) must be in specified order
    return set(numeric),other

def sanitize_string(s):
    '''Sanitize string for printing'''
    return s.replace('\n',' ')

def check_format_specifiers(source, translation, errors, numerus):
    source_f = split_format_specifiers(find_format_specifiers(source))
    # assert that no source messages contain both Qt and strprintf format specifiers
    # if this fails, go change the source as this is hacky and confusing!
    assert(not(source_f[0] and source_f[1]))
    try:
        translation_f = split_format_specifiers(find_format_specifiers(translation))
    except IndexError:
        errors.append("Parse error in translation for '%s': '%s'" % (sanitize_string(source), sanitize_string(translation)))
        return False
    else:
        if source_f != translation_f:
            if numerus and source_f == (set(), ['n']) and translation_f == (set(), []) and translation.find('%') == -1:
                # Allow numerus translations to omit %n specifier (usually when it only has one possible value)
                return True
            errors.append("Mismatch between '%s' and '%s'" % (sanitize_string(source), sanitize_string(translation)))
            return False
    return True

def all_ts_files(suffix=''):
    for filename in os.listdir(LOCALE_DIR):
        # process only language files, and do not process source language
        if not filename.endswith('.ts'+suffix) or filename == SOURCE_LANG+suffix:
            continue
        if suffix: # remove provided suffix
            filename = filename[0:-len(suffix)]
        filepath = os.path.join(LOCALE_DIR, filename)
        yield(filename, filepath)

FIX_RE = re.compile(b'[\x00-\x09\x0b\x0c\x0e-\x1f]')

# Override cdata escape function to make our output match Qt's (optional, just for cleaner diffs for
# comparison, disable by default)
_orig_escape_cdata = None
def escape_cdata(text):
    text = _orig_escape_cdata(text)
    text = text.replace("'", '&apos;')
    text = text.replace('"', '&quot;')
    return text

def check_all_translations():
    print('Checking all translations...')

    have_errors = False
    for (filename,filepath) in all_ts_files():
        global FIX_RE

        parser = ET.XMLParser(encoding='utf-8')
        with open(filepath, 'rb') as f:
            data = f.read()

        # find control characters; otherwise the XML parser will fail
        if FIX_RE.match(data):
            print(f'{{ filename }}: Found control characters, this needs to be fixed.')
            have_errors = True
            continue

        tree = ET.parse(io.BytesIO(data), parser=parser)

        # iterate over all messages in file
        root = tree.getroot()
        for context in root.findall('context'):
            for message in context.findall('message'):
                numerus = message.get('numerus') == 'yes'
                source = message.find('source').text
                translation_node = message.find('translation')
                # pick all numerusforms
                if numerus:
                    translations = [i.text for i in translation_node.findall('numerusform')]
                    if len(translations) == 0:
                        print(f'Numerus message without numerusform translations, needs fix.', sanitize_string(source))
                        have_errors = True
                    if translation_node.text is not None and not translation_node.text.isspace():
                        print(f'Numerus message contains extra text {translation_node.text}, needs fix.')
                        have_errors = True
                    for child in list(translation_node.iter())[1:]: # exclude root
                        if child.tag != 'numerusform':
                            print('Numerus message contains extra child node, needs fix.', sanitize_string(source))
                            have_errors = True
                            break
                else:
                    translations = [translation_node.text]
                    if len(list(translation_node.iter())) != 1:
                        print('Non-numerus message contains child node in translation, needs fix.', sanitize_string(source))
                        have_errors = True
                    
                for translation in translations:
                    if translation is None:
                        continue
                    errors = []
                    valid = check_format_specifiers(source, translation, errors, numerus)

                    for error in errors:
                        print('%s: %s' % (filename, error))

                    if not valid:
                        have_errors = True

        # Check for location tags
        location_count = len(message.findall('location'))
        if location_count != 0:
            have_errors = True
            print(f'{filename}: {location_count} location message(s) found!')

    return have_errors

if __name__ == '__main__':
    check_at_repository_root()
    errors_detected = check_all_translations()
    if errors_detected:
        exit(1)
    else:
        print('No errors detected.')
