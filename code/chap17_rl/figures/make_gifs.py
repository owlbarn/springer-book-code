"""Web animations of the two case-study evaluation episodes.

Usage: python3 make_gifs.py CASE FRAMES OUTPUT_DIRECTORY
  CASE is lunar or doom; FRAMES holds before-NNNN.ppm and after-NNNN.ppm for
  every step (LunarLander) or tic (VizDoom), as written by the recording runs.

The layout follows the static figure, so its caption still applies: before
training on top, after training below; the left column is the static frame of
the PDF figure and the right column plays the episode, stopping on its final
frame. The website shows NAME.gif in place of NAME.png.
"""
import sys
from pathlib import Path
from PIL import Image, ImageDraw

case, frames, target = sys.argv[1], Path(sys.argv[2]), Path(sys.argv[3])
if case == 'lunar':
    # The PDF figure's crop; every second 50 Hz step plays in real time at 40 ms.
    name, still, crop, scale, stride, delay = 'lunar-frames', 50, (60, 0, 420, 330), 1, 2, 40
    titles = ['Before: step 50', 'Before: episode', 'After: step 50', 'After: episode']
else:
    # 35 tics per second; every second tic at 57 ms is real time.
    name, still, crop, scale, stride, delay = 'doom-frames', 0, (0, 0, 160, 120), 2, 2, 57
    titles = ['Before: initial frame', 'Before: episode', 'After: initial frame', 'After: episode']


def load(path):
    image = Image.open(path).convert('RGB').crop(crop)
    return image.resize((image.width * scale, image.height * scale), Image.NEAREST)


rows = []
for label in ('before', 'after'):
    paths = sorted(frames.glob(label + '-[0-9][0-9][0-9][0-9].ppm'))
    episode = [load(p) for p in paths[::stride]]
    if (len(paths) - 1) % stride:
        episode.append(load(paths[-1]))  # always end on the final frame
    rows.append((load(paths[still]), episode))

width, height = rows[0][0].size
gap, title = 12, 22
canvas_size = (2 * width + 3 * gap, 2 * (height + title) + 3 * gap)
count = max(len(episode) for _, episode in rows)
pictures = []
for i in range(count):
    canvas = Image.new('RGB', canvas_size, 'white')
    draw = ImageDraw.Draw(canvas)
    for r, (fixed, episode) in enumerate(rows):
        top = gap + r * (height + title + gap)
        for c, panel in enumerate((fixed, episode[min(i, len(episode) - 1)])):
            left = gap + c * (width + gap)
            draw.text((left, top + 4), titles[2 * r + c], fill='black')
            canvas.paste(panel, (left, top + title))
    pictures.append(canvas.quantize(colors=128, method=Image.Quantize.MEDIANCUT))
durations = [delay] * (count - 1) + [2000]  # pause on the final frames before looping
pictures[0].save(target / f'{name}.gif', save_all=True, append_images=pictures[1:],
                 duration=durations, loop=0, optimize=True)
print(f'{name}.gif: {count} frames, {(target / (name + ".gif")).stat().st_size / 1e6:.1f} MB')
