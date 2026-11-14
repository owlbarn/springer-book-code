"""Regenerate the editorial LunarLander figures from recorded CSV and PPM frames.
Usage: python3 plot_lunar.py INPUT_DIRECTORY OUTPUT_DIRECTORY [--frames-only]
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
 with (source/'lunar-lander.csv').open() as f:
  rows=list(csv.DictReader(f))
 x=np.array([int(r['episode']) for r in rows]);y=np.array([float(r['return']) for r in rows])
 smooth=np.array([y[max(0,i-49):i+1].mean() for i in range(len(y))])
 fig,ax=plt.subplots(figsize=(6.2,2.8),layout='constrained')
 ax.plot(x,y,color='#a8bed4',linewidth=.4,alpha=.6,label='Episode return')
 ax.plot(x,smooth,color='#205b91',linewidth=1.3,label='Trailing 50 episodes')
 ax.set(xlabel='Completed training episode',ylabel='Undiscounted return');ax.legend(frameon=False,loc='lower right');ax.spines[['top','right']].set_visible(False)
 fig.savefig(target/'lunar-learning.pdf');plt.close(fig)
fig,axes=plt.subplots(2,2,figsize=(6.2,5.4),layout='constrained')
for ax,name,title in zip(axes.flat,['before-0050','before-final','after-0050','after-final'],['Before: step 50','Before: episode end','After: step 50','After: episode end']):
 pixels=Image.open(source/(name+'.ppm'))
 ax.imshow(pixels);ax.set_xlim(60,420);ax.set_ylim(330,0)
 ax.set_title(title,fontsize=10);ax.axis('off')
 if name.endswith('final'):
  # Equal 100x100-pixel detail windows, centred on each final lander position.
  left=70 if name.startswith('before') else 250
  detail=ax.inset_axes([0.55,0.44,0.42,0.42])
  detail.imshow(pixels);detail.set_xlim(left,left+100);detail.set_ylim(330,230)
  detail.set_xticks([]);detail.set_yticks([])
  detail.set_title('Detail',fontsize=8,color='white',pad=2)
  for spine in detail.spines.values(): spine.set_edgecolor('0.6')
fig.savefig(target/'lunar-frames.png',dpi=180);plt.close(fig)
