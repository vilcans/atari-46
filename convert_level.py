#!/usr/bin/env python3

import argparse

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

for line in args.input:
    line = line.rstrip().ljust(40, '.')[:40]
    bits = tuple(
        1 if char in ('X', 'x') else 0
        for char in line
    )
    if bits not in patterns:
        pf1l = sum(value << (7 - i) for i, value in enumerate(bits[ 0: 8]))
        pf2l = sum(value << (0 + i) for i, value in enumerate(bits[ 8:16]))
        pf0r = sum(value << (4 + i) for i, value in enumerate(bits[16:20]))
        pf1r = sum(value << (7 - i) for i, value in enumerate(bits[20:28]))
        pf2r = sum(value << (0 + i) for i, value in enumerate(bits[28:36]))

        patterns[bits] = len(bitmaps)
        bitmaps.append((None, pf1l, pf2l, pf0r, pf1r, pf2r))

    level.append(patterns[bits])

out = args.o

for pf in range(1, 6):
    out.write('\tALIGN $100\n')
    out.write('bitmap_pf{0}{1}:\n'.format(pf % 3, 'l' if pf < 3 else 'r'))
    for index, value in enumerate(bitmaps):
        out.write("\t.byte %{0:08b}  ; {1}\n".format(value[pf], index))

out.write('\tALIGN $100\n')
out.write('level:\n')
for bitmap_ref in level:
    out.write("\t.byte {0}\n".format(bitmap_ref))
