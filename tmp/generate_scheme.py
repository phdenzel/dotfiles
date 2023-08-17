#!/usr/bin/env python
"""
Generate themes from templates
"""
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from pathlib import Path
from phd_ark import palettes, all_palettes

flavors = list(palettes.keys())

SVG_CIRCLE="""
<svg height="50" width="50">
  <circle cx="25" cy="25" r="20" stroke="{color}" stroke-width="3" fill="{color}" />
</svg>
"""


def parse_args():
    parser = ArgumentParser(formatter_class=ArgumentDefaultsHelpFormatter)
    reqarg = parser.add_mutually_exclusive_group(required=True)
    reqarg.add_argument('-i', '--template', '--input', '--file', type=Path,
                        help="Input template file to generate scheme.")
    reqarg.add_argument('--palette', action='store_true',
                        help="Generate palette as svg circles.")
    parser.add_argument('-f', '--flavor', '--flavour',
                        type=str, choices=['all']+flavors,
                        help="Select the phd-ark color scheme flavor.")
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
    # deduce flavor if necessary
    flavor = args.flavor
    if args.flavor is None and args.output is not None:
        for f in flavors + ['all']:
            if f.lower() in str(args.output).lower():
                flavor = f
                break
    if flavor is None:
        flavor = 'iridis'
    # get color palette from flavor
    if flavor != 'all':
        colors = palettes[flavor]
    else:
        colors = all_palettes(palettes)
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


def svg_palette(args,
                svg_template=SVG_CIRCLE.strip(),
                default_outdir='phd-ark'):
    """
    Generate SVG palettes of a theme
    """
    # deduce flavor if necessary
    flavor = args.flavor
    if args.flavor is None and args.output is not None:
        for f in flavors + ['all']:
            if f in str(args.output):
                flavor = f
                break
    if flavor is None:
        flavor = 'all'
    flavor_keys = flavors if flavor == 'all' else [flavor]
    for f in flavor_keys:
        colors = palettes[f]
        # sort keys to ensure /b256 colors are used first
        color_keys = sorted(colors.keys(),
                            key=lambda x: x.endswith('/b256'),
                            reverse=True)
        # determine output directory
        if args.output is None:
            outdir = Path(f"{default_outdir}-{f}")
        else:
            outdir = args.output
        if f not in str(outdir):
            outdir = Path(str(outdir) + f"-{f}")
        print(outdir)
        # generate a file for each color
        for cname in color_keys:
            color = colors[cname]
            if not isinstance(color, str) \
               or not color.startswith('#') or cname.endswith('/b256'):
                continue
            content = svg_template.format(color=color)
            filename = outdir / Path(f"{cname}.svg")
            if args.save or args.output is not None:
                filename.write_text(content)
            else:
                print(f"{filename}:")
                print(content, end="\n\n")


if __name__ == "__main__":
    args = parse_args()
    if args.palette:
        svg_palette(args)
    else:
        generate(args)
