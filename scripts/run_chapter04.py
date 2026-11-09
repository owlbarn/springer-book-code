#!/usr/bin/env python3
"""Execute Chapter 4 examples, decomposition properties and SVD image outputs."""
import argparse
import binascii
import hashlib
import json
import shutil
import struct
import subprocess
import zlib
from pathlib import Path
from run_sessions import execute


def png_from_ppm(source, target):
    magic, dimensions, maximum, pixels = source.read_bytes().split(b'\n',3)
    width,height = map(int,dimensions.split())
    if magic!=b'P6' or maximum!=b'255' or len(pixels)!=width*height*3:
        raise ValueError(f'Invalid PPM: {source}')
    def chunk(kind,data):
        return struct.pack('!I',len(data))+kind+data+struct.pack('!I',binascii.crc32(kind+data)&0xffffffff)
    raw=b''.join(b'\0'+pixels[i:i+width*3] for i in range(0,len(pixels),width*3))
    target.write_bytes(b'\x89PNG\r\n\x1a\n'+chunk(b'IHDR',struct.pack('!2I5B',width,height,8,2,0,0,0))+chunk(b'IDAT',zlib.compress(raw))+chunk(b'IEND',b''))


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output',type=Path,default=Path('/tmp/chapter04-results'))
    args=parser.parse_args()
    root=Path(__file__).resolve().parents[1]
    chapter=root/'code/chap04_linear-algebra'
    output=args.output.resolve();output.mkdir(parents=True,exist_ok=True)
    (output/'results.json').unlink(missing_ok=True)
    names=['cat_bw','cat_10','cat_20','cat_50']
    for name in names:
        (output/(name+'.png')).unlink(missing_ok=True)
        (output/(name+'.ppm')).unlink(missing_ok=True)
    shutil.copyfile(chapter/'cat_bw.ppm',output/'cat_bw.ppm')
    commands=[]
    for command,cwd in [(['dune','build','--profile','release','@chap04_linear-algebra/all'],root/'code'),
                        ([str(root/'code/_build/default/chap04_linear-algebra/test_linear.exe')],output),
                        ([str(root/'code/_build/default/chap04_linear-algebra/svd.exe')],output)]:
        r=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=180)
        commands.append({'command':command,'cwd':str(cwd),'returncode':r.returncode,'stdout':r.stdout,'stderr':r.stderr})
        (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
        print(r.stdout,end='')
        if r.returncode:raise ValueError(r.stdout+r.stderr)
    execute(chapter/'sessions/chapter04.ml',output)
    files=[]
    for name in names:
        path=output/(name+'.png');png_from_ppm(output/(name+'.ppm'),path)
        data=path.read_bytes()
        files.append({'name':path.name,'bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()})
    record={'scope':'Chapter 4 only; numerical properties, all canonical phrases and four SVD image outputs.',
            'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
            'sources':{str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(chapter.rglob('*')) if p.is_file()},
            'files':files}
    (output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
    print('PASS: Chapter 4 canonical examples, decompositions and four image files')


if __name__=='__main__':main()
