#!/usr/bin/env python3

import argparse

parser = argparse.ArgumentParser(
    description='Convert level text to assembly source'
)
parser.add_argument(
    'input', type=argparse.FileType('r'),
    help='Text file to read'
)
parser.add_argument(
    'output', type=argparse.FileType('w'),
    help='File to write'
)

args = parser.parse_args()

values = []
for line in args.input:
    line = line.rstrip().ljust(40, '.')[:40]
    bits = [
        1 if char in ('X', 'x') else 0
        for char in line
    ]
    pf0l = sum(value << (4 + i) for i, value in enumerate(bits[ 0: 4]))
    pf1l = sum(value << (7 - i) for i, value in enumerate(bits[ 4:12]))
    pf2l = sum(value << (0 + i) for i, value in enumerate(bits[12:20]))
    pf0r = sum(value << (4 + i) for i, value in enumerate(bits[20:24]))
    pf1r = sum(value << (7 - i) for i, value in enumerate(bits[24:32]))
    pf2r = sum(value << (0 + i) for i, value in enumerate(bits[32:40]))
    for _ in range(4):
        values.append((pf0l, pf1l, pf2l, pf0r, pf1r, pf2r))

out = args.output
for pf in range(6):
    out.write('level_pf{0}{1}:\n'.format(pf % 3, 'l' if pf < 3 else 'r'))
    for value in values:
        out.write("\t.byte %{0:08b}\n".format(value[pf],))
