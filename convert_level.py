#!/usr/bin/env python3

import argparse
import re
from itertools import chain

line_with_color_re = re.compile(r'([0123456789abcdef])\s$')

parser = argparse.ArgumentParser(
    description='Convert level text to assembly source'
)
parser.add_argument(
    '-o', type=argparse.FileType('w'),
    help='File to write'
)
parser.add_argument(
    'input', type=argparse.FileType('r'), nargs='+',
    help='Text files to read'
)

args = parser.parse_args()

bitmaps = []
level = []
patterns = {}   # Map bits pattern to index in bitmaps
used_colors = { c: 0 for c in range(16) }

current_color = 0

for line in chain.from_iterable(args.input):
    m = line_with_color_re.search(line)
    if m:
        current_color = int(m.group(1), 16)

    bits = tuple(
        1 if char in ('X', 'x') else 0
        for char in line.ljust(40, '.')[:40]
    )
    key = (bits, current_color)
    if key not in patterns:
        pf1l = sum(value << (7 - i) for i, value in enumerate(bits[ 0: 8]))
        pf2l = sum(value << (0 + i) for i, value in enumerate(bits[ 8:16]))
        pf0r = sum(
            value << (4 + i) for i, value in enumerate(bits[16:20])
        ) | current_color
        pf1r = sum(value << (7 - i) for i, value in enumerate(bits[20:28]))
        pf2r = sum(value << (0 + i) for i, value in enumerate(bits[28:36]))

        patterns[key] = len(bitmaps)
        bitmaps.append((None, pf1l, pf2l, pf0r, pf1r, pf2r))
        used_colors[current_color] += 1

    level.append(patterns[key])

print('Color usage:\n' + ', '.join(
    '$%02x: %d' % (color, count)
    for color, count in used_colors.items()
))

out = args.o

address = 0


def write_source(label, values):
    global address
    bytecount = len(values)
    align = (address & 0xff) + bytecount > 0x100
    if align:
        address = (address + 0xff) & 0xff00
        out.write('\tALIGN $100\n')
    print('{0}: {1} bytes at offset ${2:04x} to ${3:04x}{4}'.format(
        label, bytecount, address, address + bytecount,
        ' needs align' if align else '')
    )
    out.write(label + ':\n')
    for index, value in enumerate(values):
        out.write("\t.byte ${0:02x}  ; %{0:08b} {1}+${2:02x}\n".format(value, label, index))
        address += 1


write_source('level', level)

for pf in range(1, 6):
    write_source(
        'bitmap_pf{0}{1}'.format(pf % 3, 'l' if pf < 3 else 'r'),
        [value[pf] for value in bitmaps]
    )
