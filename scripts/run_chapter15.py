#!/usr/bin/env python3
"""Run the book's original NST/FST models, real optimisation and regenerated figures."""
import argparse
import hashlib
import json
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from prepare_style import check,DEFAULT_DIRECTORY
from run_sessions import execute
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--output',type=Path,default=Path('/tmp/chapter15-results'))
p.add_argument('--quick',action='store_true',help='Real small-image gradients/updates/inference; not publication figures')
a=p.parse_args();root=Path(__file__).resolve().parents[1];chapter=root/'code/chap15_case-nst'
output=a.output.resolve();output.mkdir(parents=True,exist_ok=True);(output/'results.json').unlink(missing_ok=True)
evidence=check(DEFAULT_DIRECTORY)
for item in json.loads((chapter/'data/inputs.json').read_text()):
    path=chapter/'data'/item['path']
    if path.stat().st_size!=item['bytes'] or hashlib.sha256(path.read_bytes()).hexdigest()!=item['sha256']:
        raise ValueError('Changed original book photo: '+str(path))
commands=[]
def run(command,cwd,timeout=180):
    result=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=timeout)
    commands.append({'command':command,'cwd':str(cwd),'returncode':result.returncode,'stdout':result.stdout,'stderr':result.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    if result.returncode:raise ValueError(result.stdout+result.stderr)
    if result.stdout:print(result.stdout.strip(),flush=True)
run(['dune','build','--profile','release','@chap15_case-nst/all','chap15_case-nst/book_style.cma'],root/'code')
figures=['contents-reconstruction.png','style-reconstruction.png','hojo-wave.png','6754_0.png','6754_1.png']
with tempfile.TemporaryDirectory(prefix='chapter15-') as temporary:
    work=Path(temporary);build=root/'code/_build/default/chap15_case-nst'
    mapping=json.loads((chapter/'listings.json').read_text())
    prefix=''.join('open '+module+'\n' for module in mapping['interactive_opens'])
    prefix+=''.join(line.removesuffix(';;')+'\n' for line in mapping['interactive_bindings'] if line.startswith('open '))
    excerpts={};bodies=[]
    for entry in mapping['listings']:
        display=entry['display_source'];text=(root/display['path']).read_text()
        pattern=re.escape('(* book: '+display['region']+' begin *)')+r'\n(.*?)\n'+re.escape('(* book: '+display['region']+' end *)')
        body=re.search(pattern,text,re.S).group(1).strip()+'\n'
        excerpts[entry['region']]=hashlib.sha256(body.encode()).hexdigest();bodies.append(body)
    printed=prefix+'\n'.join(bodies)
    (work/'printed_excerpts.ml').write_text(printed)
    run(['ocamlfind','ocamlc','-package','owl','-I',str(build/'.book_style.objs/byte'),'-c','printed_excerpts.ml'],work)
    typecheck={'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
               'excerpts':excerpts,'combined_sha256':hashlib.sha256(printed.encode()).hexdigest(),
               'prefix_sha256':hashlib.sha256(prefix.encode()).hexdigest(),'returncode':0}
    (work/'excerpt-typecheck.json').write_text(json.dumps(typecheck,indent=2)+'\n')
    if a.quick:
        for executable in ['conv_regression','chapter15_native','fst_native']:
            run([str(build/(executable+'.exe')),str(DEFAULT_DIRECTORY),str(chapter/'data'),'--quick'],work,300)
        (output/'results.json').write_text(json.dumps({'validation_level':'quick','scope':'analytic gradients, Adam resume, real 32px VGG updates and six FST models',
            'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),'excerpt_typecheck':typecheck,'files':[]})+'\n')
        print('PASS: QUICK Chapter 15; full training and publication figures not run',flush=True)
        raise SystemExit(0)
    for executable in ['conv_regression','chapter15_native','reconstruct_native','fst_native']:
        run([str(build/(executable+'.exe')),str(DEFAULT_DIRECTORY),str(chapter/'data')],work,3600)
    metrics=json.loads((work/'native-metrics.json').read_text())
    reconstructions=json.loads((work/'reconstruction-metrics.json').read_text())
    fst=json.loads((work/'fst-metrics.json').read_text())
    if metrics['steps']!=250 or metrics['final_loss']>=metrics['initial_loss'] or len(reconstructions)!=10 or any(r['steps']!=100 or r['final_loss']>=r['initial_loss'] for r in reconstructions) or fst['models']!=6:
        raise ValueError('Incomplete real optimisation/model coverage')
    for kind,filename in [('content','contents-reconstruction.png'),('style','style-reconstruction.png')]:
        command=['montage','-font','DejaVu-Sans','-pointsize','14']
        for index in [2,7,12,21,30]:command+=['-label',('Layer ' if kind=='content' else 'Through layer ')+str(index),f'{kind}-{index}.png']
        run(command+['-tile','5x1','-geometry','224x224+4+4',filename],work)
    dependencies='#require "owl";;\n'+''.join('#directory '+json.dumps(str(path))+';;\n' for path in [build/'.book_style.objs/byte',build])+'#load "book_style.cma";;\n'
    (work/'book-dependencies.ml').write_text(dependencies)
    execute(chapter/'sessions/chapter15.ml',work)
    for name in figures+['excerpt-typecheck.json','native-metrics.json','reconstruction-metrics.json','fst-metrics.json','repl-execution.json','session-results.json']:
        shutil.copy2(work/name,output/name)
files=[]
for name in figures:
    data=(output/name).read_bytes()
    if not data.startswith(b'\x89PNG\r\n\x1a\n'):raise ValueError('Invalid regenerated style figure')
    files.append({'name':name,'bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()})
paths=[path for path in chapter.rglob('*') if path.is_file()]
paths += [Path(__file__).resolve(),root/'scripts/run_sessions.py',root/'scripts/prepare_style.py']
paths += [root/'docker/legacy'/name for name in ['export_weights.ml','build_reader.py','ocaml.json']]
paths += [root/'docker/patches'/name for name in ['owl-conv-block-offset.patch','owl-conv-block-offset.json']]
record={'scope':json.loads((chapter/'listings.json').read_text())['scope'],
        'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(path.relative_to(root)):hashlib.sha256(path.read_bytes()).hexdigest() for path in paths},
        'datasets':{'style':evidence},'excerpt_typecheck':typecheck,'metrics':metrics,'reconstructions':reconstructions,'fst':fst,'files':files}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 15 analytic gradients, exact Adam resume, seven original models, 1250 real updates and all five book figures')
