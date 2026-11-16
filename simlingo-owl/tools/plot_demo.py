"""Create a standalone input/trajectory figure and a compact demonstration record."""
import argparse
import json
from pathlib import Path
import textwrap
import numpy as np
from PIL import Image
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

p=argparse.ArgumentParser(description=__doc__)
p.add_argument('frames',nargs='+',type=Path)
p.add_argument('--out',type=Path,required=True)
a=p.parse_args()
a.out.mkdir(parents=True,exist_ok=True)
fig,axes=plt.subplots(len(a.frames),2,figsize=(12,4.6*len(a.frames)),squeeze=False)
records=[]
for index,directory in enumerate(a.frames):
    metadata=json.loads((directory/'input.json').read_text())
    output=directory/'owl/output.json'
    if not output.exists(): output=directory/'output.json'
    result=json.loads(output.read_text())
    image_path=directory/'frame.ppm'
    if not image_path.exists(): image_path=directory/'frame.png'
    image=Image.open(image_path)
    image=image.crop((0,0,image.width,int(image.height*(1-metadata['crop_bottom']))))
    left,right=axes[index]
    left.imshow(image);left.axis('off');left.set_title(directory.name+' — model input (before resize)')
    left.text(0,-0.08,textwrap.fill(result['answer'],75),transform=left.transAxes,va='top',fontsize=9)
    route=np.asarray(result['route']);speed=np.asarray(result['speed_waypoints'])
    assert route.shape==(20,2) and speed.shape==(10,2)
    assert np.isfinite(route).all() and np.isfinite(speed).all()
    right.plot(route[:,0],route[:,1],'-o',ms=3,label='Route (20 points)')
    right.plot(speed[:,0],speed[:,1],'.-',label='Speed waypoints (10 points)')
    right.scatter([0],[0],marker='^',c='black',label='Ego')
    right.set_xlabel('Forward x (m)');right.set_ylabel('Lateral y (m)')
    right.axis('equal');right.grid(alpha=.3);right.legend(fontsize=8)
    right.set_title('Input speed %.1f m/s; %.1f s CPU' % (metadata['speed'],result['elapsed_seconds']))
    records.append(dict(name=directory.name,input=metadata,output=result,
                        speed_segment_lengths=np.linalg.norm(np.diff(speed,axis=0),axis=1).tolist()))
fig.tight_layout(h_pad=5)
fig.savefig(a.out/'trajectories.png',dpi=150,bbox_inches='tight')
fig.savefig(a.out/'trajectories.pdf',bbox_inches='tight')
plt.close(fig)
(a.out/'results.json').write_text(json.dumps(records,indent=2)+'\n')
