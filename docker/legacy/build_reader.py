#!/usr/bin/env python3
"""Build the isolated historical data reader; never install it into the book switch."""
import hashlib
import json
import subprocess
import tarfile
import urllib.request
from pathlib import Path
root=Path(__file__).resolve().parent
spec=json.loads((root/'ocaml.json').read_text())
archive=root/'ocaml-source.tar.gz'
data=urllib.request.urlopen(spec['url'],timeout=120).read()
if hashlib.sha256(data).hexdigest()!=spec['sha256']:raise ValueError('Changed legacy reader source archive')
archive.write_bytes(data)
with tarfile.open(archive) as stream:stream.extractall(root,filter='data')
source=root/('ocaml-'+spec['version'])
with (root/'build.log').open('w') as log:
    for command in [['./configure','--prefix='+str(root/'unused-install'),'CC=gcc','AR=ar','RANLIB=ranlib'],
                    ['make','-j2','world','OC_CFLAGS=-O2 -fcommon -fPIC -fno-strict-aliasing -fwrapv']]:
        result=subprocess.run(command,cwd=source,stdout=log,stderr=subprocess.STDOUT)
        if result.returncode:
            log.flush();print((root/'build.log').read_text()[-12000:]);raise ValueError('Legacy reader build failed')
version=subprocess.check_output([str(source/'runtime/ocamlrun'),str(source/'ocamlc'),'-version'],text=True).strip()
if version!=spec['version']:raise ValueError('Wrong migration reader version')
print('PASS: isolated '+version+' historical data reader; final book compiler is resolved independently')
