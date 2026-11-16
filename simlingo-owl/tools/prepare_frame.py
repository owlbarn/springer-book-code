"""Select a recorded frame and prepare explicit, replayable CLI inputs."""
import argparse
import json
from pathlib import Path
from PIL import Image
from assets import digest

p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--frames',type=Path,required=True,help='OverDrive frames.json')
p.add_argument('--index',type=int,default=-1,help='index among rows with images; -1 selects middle')
p.add_argument('--crop-bottom',type=float,default=0.3)
p.add_argument('--question',default='What should the ego do next?')
p.add_argument('--out',type=Path,required=True)
a=p.parse_args()
rows=[r for r in json.loads(a.frames.read_text()) if 'image' in r]
row=rows[len(rows)//2 if a.index == -1 else a.index]
source=a.frames.parent/row['image']
a.out.mkdir(parents=True,exist_ok=False)
Image.open(source).convert('RGB').save(a.out/'frame.ppm')
(a.out/'targets.json').write_text(json.dumps(row['target_points'])+'\n')
metadata=dict(source=str(source.resolve()),source_sha256=digest(source),frame_id=row['frame_id'],
              speed=round(row['speed_mps'],1),targets=row['target_points'],
              crop_bottom=a.crop_bottom,question=a.question)
(a.out/'input.json').write_text(json.dumps(metadata,indent=2)+'\n')
print(json.dumps(metadata,indent=2))
