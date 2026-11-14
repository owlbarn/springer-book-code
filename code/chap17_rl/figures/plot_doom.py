"""Regenerate the editorial VizDoom figures from recorded CSV and PPM frames.
Usage: python3 plot_doom.py INPUT_DIRECTORY OUTPUT_DIRECTORY [--frames-only]
"""
import csv,sys
from pathlib import Path
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image
source,target=map(Path,sys.argv[1:3]);target.mkdir(parents=True,exist_ok=True)
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':10})
if '--frames-only' not in sys.argv[3:]:
 with (source/'vizdoom-dqn.csv').open() as f:
  rows=list(csv.DictReader(f))
 x=np.array([int(r['episode']) for r in rows]);y=np.array([float(r['return']) for r in rows])
 smooth=np.array([y[max(0,i-49):i+1].mean() for i in range(len(y))])
 fig,ax=plt.subplots(figsize=(6.2,2.8),layout='constrained')
 ax.plot(x,y,color='#a8bed4',linewidth=.4,alpha=.6,label='Episode return')
 ax.plot(x,smooth,color='#205b91',linewidth=1.3,label='Trailing 50 episodes')
 ax.set(xlabel='Completed training episode',ylabel='Undiscounted return');ax.legend(frameon=False,loc='lower right');ax.spines[['top','right']].set_visible(False)
 fig.savefig(target/'doom-learning.pdf');plt.close(fig)
fig,axes=plt.subplots(2,2,figsize=(6.2,4.5),layout='constrained')
for ax,name,title in zip(axes.flat,['before-0000','before-final','after-0000','after-final'],['Before: initial frame','Before: last available frame','After: initial frame','After: last available frame']):
 ax.imshow(Image.open(source/(name+'.ppm')));ax.set_title(title,fontsize=10);ax.axis('off')
fig.savefig(target/'doom-frames.png',dpi=180);plt.close(fig)
