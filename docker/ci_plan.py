#!/usr/bin/env python3
"""Plan an image build against the last published latest image, never a PR cache."""
import argparse
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'scripts'))
from verification import pipeline_inputs

ROOT=Path(__file__).resolve().parents[1]
REPOSITORY='matrixanger/springer-book-code'


def same_environment(resolved, env):
    if any(env.get(k)!=resolved[k] for k in ('ocaml','base_image')):
        return False
    if any(env.get('sources',{}).get(k)!=resolved[k] for k in ('owl','owl-plplot','owl-ode')):
        return False
    for key, patch in [('owl-ode','owl-ode-patch'),('owl-conv','owl-conv-patch'),('camlimages','camlimages-patch')]:
        if env.get('dependency_patches',{}).get(key,{}).get('sha256')!=resolved[patch]:
            return False
    return True


def needs_build(resolved, inputs, baseline, mode):
    if mode=='full':return True,'Manual full verification requested'
    if not baseline:return True,'No published incremental baseline available'
    if not same_environment(resolved,baseline):return True,'Compiler, upstream or base/dependency inputs changed'
    if baseline.get('pipeline_inputs')!=inputs:return True,'Runtime source or build/verification inputs changed'
    return False,'Published environment and runtime inputs are unchanged'


def pull_baseline():
    # Existing main is a migration baseline only; publishing never updates it.
    for tag in ('latest','main'):
        image=REPOSITORY+':'+tag
        result=subprocess.run(['docker','pull',image],capture_output=True,text=True)
        if result.returncode==0:return image
    return None


def migration_plan(resolved, inputs, baseline, mode, baseline_tag):
    needed,reason=needs_build(resolved,inputs,baseline,mode)
    if baseline_tag==REPOSITORY+':main':
        return True,'Migrate legacy main baseline to latest; '+reason
    return needed,reason


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--resolved',type=Path,required=True)
    p.add_argument('--mode',choices=['auto','full','quick'],default='auto')
    p.add_argument('--output',type=Path,required=True)
    a=p.parse_args()
    cache=ROOT/'.book-cache'
    if cache.exists():shutil.rmtree(cache)
    cache.mkdir()
    baseline=None;origin=None
    # A pull failure is never treated as evidence that inputs are unchanged.
    baseline_tag=pull_baseline()
    if baseline_tag:
        inspect=json.loads(subprocess.check_output(['docker','image','inspect',baseline_tag],text=True))[0]
        origin={'tag':baseline_tag,'image_id':inspect['Id'],'registry_digest':next((d for d in inspect['RepoDigests'] if d.startswith(REPOSITORY+'@')),None)}
        if origin['registry_digest'] is None:raise ValueError('Published baseline has no immutable registry digest')
        container=subprocess.check_output(['docker','create',origin['registry_digest'],'true'],text=True).strip()
        try:
            subprocess.run(['docker','cp',container+':/opt/book/environment.json',str(cache/'environment.json')],check=True)
            baseline=json.loads((cache/'environment.json').read_text())
            subprocess.run(['docker','cp',container+':/opt/book/verification',str(cache/'verification')],check=True)
            (cache/'origin.json').write_text(json.dumps(origin)+'\n')
        finally:
            subprocess.run(['docker','rm',container],check=True,stdout=subprocess.DEVNULL)
    else:
        print('Published baseline unavailable; a fresh build/verification is required.',flush=True)
    resolved=json.loads(a.resolved.read_text())
    needed,reason=migration_plan(resolved,pipeline_inputs(ROOT),baseline,a.mode,baseline_tag)
    plan={'build':needed,'mode':a.mode,'reason':reason,'baseline':origin}
    a.output.write_text(json.dumps(plan,indent=2)+'\n')
    print(json.dumps(plan,indent=2),flush=True)
    if os.environ.get('GITHUB_OUTPUT'):
        with open(os.environ['GITHUB_OUTPUT'],'a') as stream:
            stream.write('build='+str(needed).lower()+'\n')
    if os.environ.get('GITHUB_STEP_SUMMARY'):
        with open(os.environ['GITHUB_STEP_SUMMARY'],'a') as stream:
            stream.write('Image plan: '+reason+'\n')


if __name__=='__main__':main()
