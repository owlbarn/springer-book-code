#!/usr/bin/env python3
"""Verify/download the pinned original news input into a caller-owned work directory."""
import argparse
import gzip
import hashlib
import json
import tempfile
import urllib.request
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]
DEFAULT_MANIFEST=ROOT/'code/chap12_nlp/news.json'
def digest(path):
    h=hashlib.sha256()
    with Path(path).open('rb') as stream:
        for block in iter(lambda:stream.read(1024*1024),b''):h.update(block)
    return h.hexdigest()
def receipt(manifest_path=DEFAULT_MANIFEST):
    manifest=json.loads(Path(manifest_path).read_text())
    return {key:manifest[key] for key in ('repository','revision','sha256','decoded_sha256','decoded_bytes','documents')} | {'manifest_sha256':digest(manifest_path)}
def check(path,manifest_path=DEFAULT_MANIFEST):
    m=json.loads(Path(manifest_path).read_text());path=Path(path)
    if not path.is_file() or path.stat().st_size!=m['decoded_bytes'] or digest(path)!=m['decoded_sha256']:
        raise ValueError('Missing or changed news input: '+str(path))
    return receipt(manifest_path)
def prepare(directory,manifest_path=DEFAULT_MANIFEST):
    directory=Path(directory);directory.mkdir(parents=True,exist_ok=True)
    target=directory/'news.txt'
    if target.exists():return target,check(target,manifest_path)
    m=json.loads(Path(manifest_path).read_text())
    with tempfile.TemporaryDirectory(prefix='news-download-',dir=directory) as temporary:
        compressed=Path(temporary)/'news.gz';decoded=Path(temporary)/'news.txt'
        h=hashlib.sha256();git=hashlib.sha1(b'blob '+str(m['size']).encode()+b'\0');size=0
        with urllib.request.urlopen(m['download_url'],timeout=120) as response,compressed.open('wb') as stream:
            while block:=response.read(1024*1024):
                size+=len(block)
                if size>m['size']:raise ValueError('Oversized compressed news input')
                h.update(block);git.update(block);stream.write(block)
        if size!=m['size'] or h.hexdigest()!=m['sha256'] or git.hexdigest()!=m['sha']:
            raise ValueError('Changed compressed news input or Git object')
        size=0
        with gzip.open(compressed,'rb') as source,decoded.open('wb') as destination:
            while block:=source.read(1024*1024):
                size+=len(block)
                if size>m['decoded_bytes']:raise ValueError('Oversized decoded news input')
                destination.write(block)
        evidence=check(decoded,manifest_path)
        decoded.replace(target)
    return target,evidence
def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--directory',type=Path,required=True)
    a=p.parse_args();path,evidence=prepare(a.directory);print('PASS: pinned 130000-document news input: '+str(path))
if __name__=='__main__':main()
