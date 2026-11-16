"""Create deterministic independent test inputs/outputs; needs local tokenizer snapshot."""
import argparse
import json
import random
import string
from pathlib import Path
import numpy as np
import torch
from torch.nn import functional as F
from reference import tokenizer, prompt, local_module
from assets import digest

p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--base',type=Path,required=True)
p.add_argument('--out',type=Path,required=True)
a=p.parse_args()
a.out.mkdir(parents=True,exist_ok=True)
tok=tokenizer(a.base)
texts=['',"I'm following the car. Isn't it slowing down?",'  12.30\r\n\nstop!  ',
       '<TARGET_POINT><TARGET_POINT>', '<img><IMG_CONTEXT></img>',
       prompt(a.base,9.5,'What should the ego do next?'),
       "a\t b\n\r  \t", "We'RE ready; I'LL turn."]
rng=random.Random(42)
texts += [''.join(rng.choices(string.ascii_letters+string.digits+string.punctuation+' \t\n\r\v\f',k=rng.randrange(1,200))) for _ in range(250)]
cases=[]
for text in texts:
    ids=tok.encode(text,add_special_tokens=False)
    cases.append(dict(text=text,ids=ids,decoded=tok.decode(ids,skip_special_tokens=True,clean_up_tokenization_spaces=False)))
(a.out/'tokenizer.json').write_text(json.dumps(cases))
decode_cases=[]
for text in ['你好，前方有车。','Straße — café','🚗🛑','日本語','中文 mixed English']:
    ids=tok.encode(text,add_special_tokens=False)
    for n in range(1,len(ids)+1):
        decode_cases.append(dict(ids=ids[:n],decoded=tok.decode(ids[:n],skip_special_tokens=True,clean_up_tokenization_spaces=False)))
(a.out/'decode.json').write_text(json.dumps(decode_cases))
torch.manual_seed(42)
def save(name,x): np.save(a.out/(name+'.npy'),x.float().numpy())
x=torch.linspace(-8,8,101).reshape(1,-1)
save('activation_input',x); save('gelu',F.gelu(x));save('silu',F.silu(x))
x=torch.randn(7,896); save('rope_input',x)
t=torch.outer(torch.arange(7).float(),1/(1e6**(torch.arange(0,64,2).float()/64)))
emb=torch.cat([t,t],-1)
y=x.reshape(7,14,64)
y=y*emb.cos()[:,None]+torch.cat([-y[:,:,32:],y[:,:,:32]],-1)*emb.sin()[:,None]
save('rope',y.reshape(7,896))
q,k,v=torch.randn(7,896),torch.randn(7,128),torch.randn(7,128)
for name,value in [('q',q),('k',k),('v',v)]: save(name,value)
qt=q.reshape(7,14,64).permute(1,0,2)
kt=k.reshape(7,2,64).permute(1,0,2).repeat_interleave(7,dim=0)
vt=v.reshape(7,2,64).permute(1,0,2).repeat_interleave(7,dim=0)
scores=qt@kt.transpose(-2,-1)/8
scores=scores.masked_fill(torch.ones(7,7,dtype=torch.bool).triu(1),float('-inf'))
save('attention',(scores.softmax(-1)@vt).permute(1,0,2).reshape(7,896))
x=torch.arange(1024*1024).float().reshape(1024,1024)/1000000
save('shuffle_input',x)
import types
chat=local_module(a.base,'modeling_internvl_chat').InternVLChatModel
save('shuffle',chat.pixel_shuffle(types.SimpleNamespace(ps_version='v2'),x.reshape(1,32,32,1024)).reshape(256,4096))
(a.out/'whitespace.ppm').write_bytes(b'P6\n# comment\n1 1\n255\n\n\r ')
for name,shape,checksum in [('valid',[2,2],None),('bad-shape',[4],None),('bad-hash',[2,2],'0'*64)]:
    directory=a.out/name
    directory.mkdir(exist_ok=True)
    np.save(directory/'weight.npy',np.eye(2,dtype=np.float32))
    entry=dict(file='weight.npy',shape=shape,dtype='float32',sha256=checksum or digest(directory/'weight.npy'))
    (directory/'manifest.json').write_text(json.dumps(dict(format_version=1,tensors={'weight':entry})))
print('fixtures created')
