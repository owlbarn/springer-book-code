#!/usr/bin/env python3
"""Draw the chapter's diagrams and retained-frame predictions as PDF figures."""
import argparse
import json
from pathlib import Path
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
import numpy as np
from PIL import Image

ROOT = Path(__file__).resolve().parents[2]
MODEL = ROOT/'simlingo-owl'
plt.rcParams.update({'font.family': 'DejaVu Sans', 'font.size': 9,
                     'pdf.fonttype': 42, 'axes.spines.top': False,
                     'axes.spines.right': False})
BLUE, ORANGE, GREEN = '#24577c', '#a74c20', '#426b42'


def camera(name):
    data = json.loads((MODEL/'demo'/name/'input.json').read_text())
    im = Image.open(MODEL/'demo'/name/'frame.png').convert('RGB')
    return im.crop((0, 0, im.width, int(im.height*(1-data['crop_bottom']))))


def save(fig, out, name):
    fig.savefig(out/(name+'.pdf'), bbox_inches='tight', metadata={'CreationDate': None})
    plt.close(fig)


def box(ax, x, y, width, height, label, color='0.95'):
    ax.add_patch(FancyBboxPatch((x, y), width, height,
                 boxstyle='round,pad=0.008', facecolor=color, edgecolor='0.35', lw=0.8))
    ax.text(x+width/2, y+height/2, label, ha='center', va='center', fontsize=9)


def arrow(ax, start, end):
    ax.annotate('', xy=end, xytext=start,
                arrowprops={'arrowstyle': '->', 'color': '0.3', 'lw': 1.1})


def draw(out, run):
    out.mkdir(parents=True, exist_ok=True)
    fig, ax = plt.subplots(figsize=(6.2, 2.3))
    ax.imshow(camera('straight')); ax.axis('off')
    save(fig, out, 'scene')

    fig, ax = plt.subplots(figsize=(6.2, 4.5))
    ax.set(xlim=(0, 1), ylim=(0, 1)); ax.axis('off')
    box(ax, .01, .87, .29, .1, 'Camera frame')
    box(ax, .355, .87, .29, .1, 'Speed + question')
    box(ax, .70, .87, .29, .1, 'Two target points')
    for center in [.155, .50, .845]: arrow(ax, (center, .87), (center, .79))
    box(ax, .01, .66, .29, .13, 'ViT + projector\n512 × 896')
    box(ax, .355, .66, .29, .13, 'Tokenizer + lookup\ntext rows × 896')
    box(ax, .70, .66, .29, .13, 'Waypoint encoder\n2 × 896')
    for center in [.155, .50, .845]: arrow(ax, (center, .66), (center, .59))
    box(ax, .01, .49, .98, .10, 'Replace placeholders: one sequence of 896-dimensional rows')
    arrow(ax, (.50, .49), (.50, .43))
    box(ax, .01, .30, .61, .13, 'Qwen2 → next token → append its embedding\nRepeat until end of answer')
    arrow(ax, (.62, .365), (.70, .365))
    box(ax, .70, .30, .29, .13, 'Decoded answer')
    arrow(ax, (.315, .30), (.315, .24))
    box(ax, .01, .11, .61, .13, 'Append 20 route + 10 speed queries\nQwen2 with the same weights')
    arrow(ax, (.62, .175), (.70, .175))
    box(ax, .70, .11, .29, .13, 'Heads +\ncumulative sum\n20 × 2 and 10 × 2')
    save(fig, out, 'pipeline')

    fig, axes = plt.subplots(1, 2, figsize=(6.2, 3.0))
    resized = camera('straight').resize((896, 448), Image.Resampling.BILINEAR)
    for i, ax in enumerate(axes):
        ax.imshow(resized.crop((i*448, 0, (i+1)*448, 448)))
        for p in range(0, 449, 14):
            ax.axhline(p-.5, color='white', alpha=.35, lw=.3)
            ax.axvline(p-.5, color='white', alpha=.35, lw=.3)
        ax.set_title(('Left tile', 'Right tile')[i]+' — 32 × 32 patches')
        ax.axis('off')
    fig.tight_layout()
    save(fig, out, 'tiles')

    fig, ax = plt.subplots(figsize=(6.2, 2.6))
    ax.set(xlim=(0, 1), ylim=(0, 1)); ax.axis('off')
    for y, title, answer, queries in [(.74, 'Input: 546 rows', False, False),
                                      (.42, 'After answer: 577 rows', True, False),
                                      (.10, 'Driving pass: 607 rows', True, True)]:
        ax.text(0, y+.19, title, fontsize=9)
        box(ax, .01, y, .45, .14, 'Text + 512 image + 2 target rows', '#e8eef3')
        if answer: box(ax, .48, y, .22, .14, '31 answer tokens', '#f5e8de')
        if queries: box(ax, .72, y, .27, .14, '20 + 10 queries', '#e8efe6')
    save(fig, out, 'sequence')

    fig, axes = plt.subplots(3, 2, figsize=(6.2, 6.6),
                             gridspec_kw={'width_ratios': [1.45, 1]})
    for row, name in enumerate(['straight', 'curve', 'close_follow']):
        inp = json.loads((MODEL/'demo'/name/'input.json').read_text())
        result = json.loads((run/name/'output.json').read_text())
        axes[row, 0].imshow(camera(name)); axes[row, 0].axis('off')
        axes[row, 0].set_title(name.replace('_', ' ').capitalize()+f" — {inp['speed']:.1f} m/s", loc='left')
        ax = axes[row, 1]
        for key, color, marker, label in [('route', BLUE, 'o', 'Route'),
                                         ('speed_waypoints', ORANGE, 'x', 'Speed waypoints')]:
            points = np.asarray(result[key])
            ax.plot(points[:, 1], points[:, 0], color=color, marker=marker,
                    ms=3, lw=.9, label=label)
        ax.plot(0, 0, '^', color='black', ms=5)
        ax.set(xlim=(-4, 4), ylim=(-1, 40), xlabel='Right (m)', ylabel='Forward (m)')
        ax.grid(alpha=.2)
        if row == 0: ax.legend(fontsize=7, loc='upper left')
    fig.tight_layout(h_pad=1.2)
    save(fig, out, 'cases')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--out', required=True, type=Path)
    parser.add_argument('--run', required=True, type=Path)
    args = parser.parse_args()
    draw(args.out, args.run)
