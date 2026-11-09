#!/usr/bin/env python3
"""Publish a unique tested image, verify a registry pull, then promote pilot and main."""
import argparse
import datetime
import json
import re
import subprocess
from pathlib import Path
from validate import validate

REPOSITORY='matrixanger/springer-book-code'


def pushed_digest(text):
    matches=re.findall(r'digest: (sha256:[0-9a-f]{64})',text)
    if not matches:raise ValueError('Docker push did not report a registry digest')
    return matches[-1]


def validate_before_publish(image_id,resolved,previous=None):
    if previous is None:
        return validate(image_id,resolved)
    # A completed reader run for this exact immutable image can be reused.
    # Re-read and compare the entire baked evidence against resolved inputs;
    # mutable tags, partial runs, stale sources and changed environments fail.
    prior=json.loads(Path(previous).read_text())
    current=validate(image_id,resolved,execute=False)
    if (prior.get('runtime_check')!='passed' or
        prior.get('interactive_check',{}).get('status')!='passed' or
        prior.get('interactive_check',{}).get('terminal')!='pty' or
        any(prior.get(key)!=current[key] for key in ('image','environment','chapters','verification'))):
        raise ValueError('Previous validation is incomplete or belongs to a different image/evidence')
    return prior


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--image',required=True)
    p.add_argument('--resolved',type=Path,required=True)
    p.add_argument('--output',type=Path,required=True)
    p.add_argument('--validation',type=Path,help='Reuse a complete reader/PTY record only for this immutable image and exact baked evidence')
    p.add_argument('--run-id',default=datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%d%H%M%S%f'))
    a=p.parse_args()
    if not re.fullmatch(r'[A-Za-z0-9_.-]+',a.run_id):raise ValueError('Invalid build run ID')
    resolved=json.loads(a.resolved.read_text())
    # Lock onto the image configuration that passed local checks, not a movable tag.
    image_id=subprocess.check_output(['docker','image','inspect','--format','{{.Id}}',a.image],text=True).strip()
    validate_before_publish(image_id,resolved,a.validation)
    unique=f'{REPOSITORY}:pilot-{resolved["code_revision"][:12]}-{a.run_id}'
    subprocess.run(['docker','tag',image_id,unique],check=True)
    push=subprocess.run(['docker','push',unique],text=True,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,check=True)
    print(push.stdout,end='')
    digest=pushed_digest(push.stdout)
    pulled=REPOSITORY+'@'+digest
    subprocess.run(['docker','pull',pulled],check=True)
    remote_id=subprocess.check_output(['docker','image','inspect','--format','{{.Id}}',pulled],text=True).strip()
    if remote_id != image_id:raise ValueError('Registry returned a different image')
    validate(pulled,resolved)
    # Promote main last, after registry validation and the compatibility pilot tag.
    # Both aliases use the exact tested image; no rebuild or upstream resolution.
    for tag in ('pilot','main'):
        alias=REPOSITORY+':'+tag
        subprocess.run(['docker','tag',image_id,alias],check=True)
        push=subprocess.run(['docker','push',alias],text=True,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,check=True)
        print(push.stdout,end='')
        if pushed_digest(push.stdout) != digest:raise ValueError(f'Promoted {tag} digest differs')
    report={'image_id':image_id,'unique_tag':unique,'registry_digest':digest,
            'pilot_tag':REPOSITORY+':pilot','main_tag':REPOSITORY+':main','resolved':resolved,'registry_pull_validation':'passed'}
    a.output.parent.mkdir(parents=True,exist_ok=True)
    a.output.write_text(json.dumps(report,indent=2)+'\n')
    print('Published verified main and pilot:',pulled)


if __name__=='__main__':main()
