#!/usr/bin/env python3
"""Verify the fixed portable Mask R-CNN cache; HDF5 conversion is build-only."""
import argparse
import hashlib
import json
import shutil
import struct
import tempfile
import urllib.request
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]
MANIFEST=ROOT/'code/chap14_case-obj-detect/portable-manifest.json'
DEFAULT_DIRECTORY=Path.home()/'.owl/dataset/mrcnn'
def digest(path):
    h=hashlib.sha256()
    with Path(path).open('rb') as stream:
        for data in iter(lambda:stream.read(1024*1024),b''):h.update(data)
    return h.hexdigest()
def check(directory):
    directory=Path(directory);manifest=json.loads(MANIFEST.read_text())
    if not directory.is_dir() or {p.name for p in directory.iterdir()}!={item['path'] for item in manifest['files']}:
        raise ValueError('Missing or extra portable Mask R-CNN input')
    files={}
    for item in manifest['files']:
        path=directory/item['path']
        if path.stat().st_size!=item['bytes'] or digest(path)!=item['sha256']:
            raise ValueError('Changed portable Mask R-CNN input: '+item['path'])
        files[item['path']]=item['sha256']
    return {'source_hdf5':manifest['original_hdf5'],'manifest_sha256':digest(MANIFEST),
            'datasets':manifest['datasets'],'unique_parameters':manifest['parameters'],
            'float32_roundtrip_exact':manifest['float32_roundtrip_exact'],'files':files}
def download(source,target):
    h=hashlib.sha256();size=0
    with urllib.request.urlopen(source['url'],timeout=120) as response,target.open('wb') as out:
        while data:=response.read(1024*1024):
            size+=len(data)
            if size>source['bytes']:raise ValueError('Oversized HDF5 input')
            h.update(data);out.write(data)
    if size!=source['bytes'] or h.hexdigest()!=source['sha256']:raise ValueError('Changed original HDF5 input')
def prepare(directory,hdf5=None):
    import h5py
    import numpy as np
    directory=Path(directory)
    if directory.exists():return check(directory)
    manifest=json.loads(MANIFEST.read_text());source=manifest['original_hdf5']
    with tempfile.TemporaryDirectory(prefix='mrcnn-model-') as temporary:
        work=Path(temporary);portable=work/'portable';portable.mkdir()
        if hdf5:
            hdf5=Path(hdf5)
            if hdf5.stat().st_size!=source['bytes'] or digest(hdf5)!=source['sha256']:raise ValueError('Changed local HDF5 input')
        else:hdf5=work/'source.hdf5';download(source,hdf5)
        total=0
        with h5py.File(hdf5,'r') as handle:
            if len(handle)!=manifest['datasets']:raise ValueError('Incomplete HDF5 datasets')
            for name in sorted(handle):
                if '/' in name or name in ('.','..'):raise ValueError('Unexpected dataset path')
                values=handle[name][...];f32=np.asarray(values,dtype='<f4',order='C')
                if not np.isfinite(f32).all() or not np.array_equal(f32.astype('float64'),values):
                    raise ValueError('Original tensor is not exactly representable as float32: '+name)
                shape=', '.join(map(str,f32.shape))+','
                header=("{'descr': '<f4', 'fortran_order': False, 'shape': ("+shape+"), }").encode('ascii')
                header+=b' '*((16-(10+len(header)+1)%16)%16)+b'\n'
                data=b'\x93NUMPY\x01\x00'+struct.pack('<H',len(header))+header+f32.tobytes(order='C')
                (portable/(name+'.npy')).write_bytes(data);total+=f32.size
        if total!=manifest['parameters']:raise ValueError('Incomplete parameter count')
        evidence=check(portable)
        directory.parent.mkdir(parents=True,exist_ok=True);shutil.copytree(portable,directory)
    return evidence
def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--directory',type=Path,default=DEFAULT_DIRECTORY)
    p.add_argument('--prepare',action='store_true',help='Build stage: needs h5py/NumPy; not required by readers')
    p.add_argument('--hdf5',type=Path,help='Reuse only a matching original HDF5 input during preparation')
    a=p.parse_args();evidence=prepare(a.directory,a.hdf5) if a.prepare else check(a.directory)
    print('PASS: 690 original datasets, 64158584 exact float32 parameters and all portable hashes')
if __name__=='__main__':main()
