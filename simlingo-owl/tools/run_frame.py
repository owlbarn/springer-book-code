"""Run the OCaml executable on a prepared frame. Python does not run the model."""
import argparse
import json
import os
from pathlib import Path
import subprocess

p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--frame',type=Path,required=True)
p.add_argument('--weights',type=Path,required=True)
p.add_argument('--exe',type=Path,default=Path('_build/default/bin/main.exe'))
p.add_argument('--max-new-tokens',type=int,default=100)
p.add_argument('--threads',type=int,default=4,help='BLAS/OpenMP threads (default 4)')
p.add_argument('--out',type=Path,help='new output directory (default: FRAME/owl)')
a=p.parse_args()
if a.threads < 1: p.error('threads must be positive')
metadata=json.loads((a.frame/'input.json').read_text())
out=a.out or a.frame/'owl'
out.mkdir(parents=True,exist_ok=False)
image=a.frame/'frame.ppm'
if not image.exists():
    from PIL import Image
    image=out/'frame.ppm'
    Image.open(a.frame/'frame.png').convert('RGB').save(image)
env=os.environ.copy()
env['OPENBLAS_NUM_THREADS']=str(a.threads)
env['OMP_NUM_THREADS']=str(a.threads)
cmd=[str(a.exe.resolve()),'--weights',str(a.weights),'--image',str(image),
     '--targets',str(a.frame/'targets.json'),'--speed',str(metadata['speed']),
     '--crop-bottom',str(metadata['crop_bottom']),'--question',metadata['question'],
     '--max-new-tokens',str(a.max_new_tokens),'--dump',str(out),'--output',str(out/'output.json')]
with (out/'run.log').open('w') as log:
    with subprocess.Popen(cmd,env=env,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,
                          text=True,bufsize=1) as process:
        for line in process.stdout:
            print(line,end='',flush=True)
            log.write(line)
            log.flush()
        status=process.wait()
        if status:
            raise subprocess.CalledProcessError(status,cmd)
print((out/'output.json').read_text())
