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
    line = line.rstrip().ljust(20, '.')[:20]
    bits = [
        1 if char in ('X', 'x') else 0
        for char in line
    ]
    pf0 = sum(
        value << (4 + i) for i, value in enumerate(bits[0:4])
    )
    pf1 = sum(
        value << (7 - i) for i, value in enumerate(bits[4:12])
    )
    pf2 = sum(
        value << (0 + i) for i, value in enumerate(bits[12:20])
    )
    for _ in range(8):
        values.append((pf0, pf1, pf2))

out = args.output
for pf in (0, 1, 2):
    out.write('level_pf{0}:\n'.format(pf,))
    for value in values:
        out.write("\t.byte %{0:08b}\n".format(value[pf],))
