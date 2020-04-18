#!/usr/bin/env python3

import argparse
import json
import os.path

parser = argparse.ArgumentParser(
    description='Convert level text to assembly source'
)
parser.add_argument(
    'input', nargs='+', type=argparse.FileType('r'),
    help='JSON level file to read'
)

args = parser.parse_args()

for in_file in args.input:
    doc = json.load(in_file)
    out_filename = os.path.splitext(in_file.name)[0] + '.txt'
    out = open(out_filename, 'w')

    layer = doc['layers'][0]
    width, height = layer['width'], layer['height']
    data = layer['data']
    for row in range(height):
        bits = []
        row_color = None
        warn = False
        for col in range(width):
            tile = data[col + row * width]
            if 1 <= tile < 16:
                bits.append(1)
                tile_color = tile - 1
                if row_color is None:
                    row_color = tile_color
                elif row_color != tile_color:
                    warn = True
            else:
                bits.append(0)

        if warn:
            print('Warning: More than one color on row {0}'.format(row))

        out.write(''.join('x' if bit else '.' for bit in bits) + ' ' + '{0:1x}'.format(row_color or 0) + '\n')
    out.close()
