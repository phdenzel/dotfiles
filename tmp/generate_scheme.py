#!/usr/bin/env python
"""
Generate themes from templates
"""
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from pathlib import Path
from phd_ark import palettes, all_palettes

flavors = list(palettes.keys())


def parse_args():
    parser = ArgumentParser(formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-f', '--flavor', '--flavour',
                        type=str, choices=['all']+flavors,
                        help="Select the phd-ark color scheme flavor.")
    parser.add_argument('-i', '--template', '--input', '--file',
                        type=Path, required=True,
                        help="Input template file to generate scheme.")
    parser.add_argument('-o', '--output', '--scheme',
                        type=Path,
                        help="Output file for the generated scheme.")
    parser.add_argument('--save', action='store_true',
                        help="Output file for the generated scheme.")
    args = parser.parse_args()
    return args


def generate(args, search_prefix='#phd-ark-'):
    """
    Generate a theme from a template
    """
    # get template file content
    tmpl = args.template.resolve()
    if args.output is None:
        filename = Path(
            f"{str(tmpl.stem).replace('-template', '')}"
            f"-{args.flavor}"
            f"{tmpl.suffix}")
    else:
        filename = args.output.resolve()
    content = tmpl.read_text()
    # deduce flavor if necessary
    flavor = args.flavor
    if args.flavor is None and args.output is not None:
        for f in flavors:
            if f in str(args.output):
                flavor = f
                break
    if flavor is None:
        flavor = 'default'
    # get color palette from flavor
    if flavor != 'all':
        colors = palettes[flavor]
    else:
        colors = all_palettes(palettes)
    # sort keys to ensure /b256 colors are used first
    color_keys = sorted(colors.keys(),
                        key=lambda x: x.endswith('/b256'),
                        reverse=True)
    # insert corresponding colors to template
    for cname in color_keys:
        find = f"{search_prefix}{cname}"
        replace = colors[cname]
        if isinstance(replace, int):
            replace = f"{replace:03d}"
        content = content.replace(find, replace)
    if args.save or args.output is not None:
        filename.write_text(content)
    else:
        print(content)

if __name__ == "__main__":
    args = parse_args()
    generate(args)
