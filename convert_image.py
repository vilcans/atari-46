#!/usr/bin/env python3

import sys
from array import array
from PIL import Image, ImageOps

import argparse


def get_pixel(image, x, y):
    r, g, b = image.getpixel((x, y))
    return r >= 128 and g >= 128 and b >= 128


def convert(image):
    """Returns the image as a byte array"""

    image = image.convert('RGB')
    data = array('B')
    for c in range(image.size[0] // 8):
        for y in range(image.size[1]):
            value = 0
            for xoffs in range(8):
                pixel = get_pixel(image, c * 8 + xoffs, y)
                value = (value << 1) | pixel
            data.append(value)

    return data


def main():
    parser = argparse.ArgumentParser(
        description='Convert an image to raw bytes'
    )
    parser.add_argument(
        'source', metavar='IMAGE_FILE',
        help='Image file to convert'
    )
    parser.add_argument(
        'out', metavar='OUTPUT_FILE',
        type=argparse.FileType('wb'),
        help='File to write graphics data to'
    )
    parser.add_argument(
        '--flip', action='store_true', default=False,
    )

    args = parser.parse_args()

    image = Image.open(args.source)
    if args.flip:
        image = ImageOps.flip(image)

    if (image.size[0] % 8) != 0:
        print('Image must have a width that is a multiple of 8', file=sys.stderr)
        sys.exit(1)

    raw_data = convert(image)

    args.out.write(raw_data)

if __name__ == '__main__':
    main()
