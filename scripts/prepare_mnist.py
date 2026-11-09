#!/usr/bin/env python3
"""Fetch or verify only the six pinned MNIST files used by the existing book."""
import argparse
import gzip
import hashlib
import json
import urllib.request
from pathlib import Path

ROOT=Path(__file__).resolve().parents[1]
DEFAULT_MANIFEST=ROOT/'code/chap11_neural_network/mnist.json'
DEFAULT_DIRECTORY=Path.home()/'.owl/dataset'
def digest(path):return hashlib.sha256(path.read_bytes()).hexdigest()
def check(directory, manifest_path=DEFAULT_MANIFEST):
    manifest=json.loads(Path(manifest_path).read_text())
    files={}
    for item in manifest['files']:
        name=item['path'].removesuffix('.gz')
        path=Path(directory)/name
        if not path.is_file() or path.stat().st_size!=item['decoded_size'] or digest(path)!=item['decoded_sha256']:
            raise ValueError(f'Missing or changed MNIST input: {path}; run scripts/prepare_mnist.py')
        files[name]=item['decoded_sha256']
    return {'repository':manifest['repository'],'revision':manifest['revision'],
            'manifest_sha256':digest(Path(manifest_path)),'files':files,
            'license_declaration':manifest['license_declaration']}
def prepare(directory,manifest_path=DEFAULT_MANIFEST):
    directory=Path(directory);directory.mkdir(parents=True,exist_ok=True)
    manifest=json.loads(Path(manifest_path).read_text())
    for item in manifest['files']:
        target=directory/item['path'].removesuffix('.gz')
        if target.exists():
            if digest(target)==item['decoded_sha256']:continue
            raise ValueError(f'Refusing to replace a different existing file: {target}')
        data=urllib.request.urlopen(item['url'],timeout=60).read(item['size']+1)
        if len(data)!=item['size'] or hashlib.sha256(data).hexdigest()!=item['sha256']:
            raise ValueError('Unexpected compressed MNIST input: '+item['path'])
        if hashlib.sha1(b'blob '+str(len(data)).encode()+b'\0'+data).hexdigest()!=item['sha']:
            raise ValueError('Unexpected Git object: '+item['path'])
        raw=gzip.decompress(data)
        if len(raw)!=item['decoded_size'] or hashlib.sha256(raw).hexdigest()!=item['decoded_sha256']:
            raise ValueError('Unexpected decoded MNIST input: '+item['path'])
        temporary=target.with_suffix('.book-download')
        temporary.write_bytes(raw);temporary.replace(target)
    return check(directory,manifest_path)
def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--directory',type=Path,default=DEFAULT_DIRECTORY)
    p.add_argument('--manifest',type=Path,default=DEFAULT_MANIFEST)
    p.add_argument('--check',action='store_true')
    a=p.parse_args();record=check(a.directory,a.manifest) if a.check else prepare(a.directory,a.manifest)
    print('PASS: six pinned MNIST inputs; revision '+record['revision'])
if __name__=='__main__':main()
