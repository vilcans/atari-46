#!/usr/bin/env python3

import argparse
import re

line_with_color_re = re.compile(r'\s*([0123456789abcdef])')

parser = argparse.ArgumentParser(
    description='Convert level text to assembly source'
)
parser.add_argument(
    '-o', type=argparse.FileType('w'),
    help='File to write'
)
parser.add_argument(
    'input', type=argparse.FileType('r'),
    help='Text file to read'
)

args = parser.parse_args()

bitmaps = []
level = []
patterns = {}   # Map bits pattern to index in bitmaps

current_color = 0

for line in args.input:
    m = line_with_color_re.match(line, pos=40)
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

    level.append(patterns[key])

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


for pf in range(1, 6):
    write_source(
        'bitmap_pf{0}{1}'.format(pf % 3, 'l' if pf < 3 else 'r'),
        [value[pf] for value in bitmaps]
    )

write_source('level', level)
