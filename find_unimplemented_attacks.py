#!/usr/bin/env python3

"""
This script is meant to help discover attack types/damages that have not been
added to Pinobot unixmain.patch.

Example:
./find_unimplemented_attacks.py nethack/include/monattk.h pinobot/patch/unixmain.patch
"""

import re
import sys

AT_RE = re.compile('.*(AT|AD)_([0-9A-Za-z]+).*')

if __name__ == '__main__':
    seen_ats1 = set()
    seen_ats2 = set()

    with open(sys.argv[1], 'rt') as f:
        for line in f:
            m = AT_RE.match(line)
            if not m:
                continue
            seen_ats1.add(f'{m.group(1)}_{m.group(2)}')

    with open(sys.argv[2], 'rt') as f:
        for line in f:
            m = AT_RE.match(line)
            if not m:
                continue
            seen_ats2.add(f'{m.group(1)}_{m.group(2)}')

    # What attacks/damages are missing?
    missing = seen_ats1 - seen_ats2

    # Go through the first file again, but if it contains a missing entry then
    # print the entire line.
    with open(sys.argv[1], 'rt') as f:
        for line in f:
            for m in missing:
                if m in line:
                    print(line.rstrip('\n'))
                    break
