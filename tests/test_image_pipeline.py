import contextlib
import io
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]/'docker'))
import build
import publish
from validate import validate


class PipelineTests(unittest.TestCase):
    def setUp(self):
        self.resolved={'ocaml':'5.5.2','owl':'a'*40,'owl-plplot':'b'*40,
                       'base_image':'ubuntu@sha256:'+'c'*64,
                       'code_revision':'d'*40,'code_dirty':False}
        self.record={'environment':{'ocaml':'5.5.2','sources':{'owl':'a'*40,'owl-plplot':'b'*40},
                       'owl_cflags':'-O3 -march=x86-64','base_image':self.resolved['base_image'],'code_revision':'d'*40,'code_dirty':'false'},
                     'verification':{'ocaml':'5.5.2','files':[{}]*8}}

    def test_release_candidates_and_drafts_cannot_replace_stable(self):
        for tag,pre,draft in [('5.6.0~rc1',True,False),('5.6.0',False,True),('trunk',False,False)]:
            with self.subTest(tag=tag),self.assertRaises(ValueError):
                build.stable_version({'tag_name':tag,'prerelease':pre,'draft':draft})
        self.assertEqual(build.stable_version({'tag_name':'5.5.2','prerelease':False,'draft':False}),'5.5.2')

    def test_wrong_compiler_or_source_is_rejected_before_reader_execution(self):
        for key in ('ocaml','code_revision','base_image','code_dirty'):
            with self.subTest(key=key):
                record=json.loads(json.dumps(self.record));record['environment'][key]='wrong'
                with patch('validate.subprocess.check_output',return_value=json.dumps(record)),patch('validate.subprocess.run') as run:
                    with self.assertRaises(ValueError):validate('image',self.resolved)
                    run.assert_not_called()
        record=json.loads(json.dumps(self.record));record['environment']['sources']['owl']='wrong'
        with patch('validate.subprocess.check_output',return_value=json.dumps(record)),self.assertRaises(ValueError):
            validate('image',self.resolved)

    def test_host_specific_cpu_build_is_rejected(self):
        self.record['environment']['owl_cflags']='-O3 -march=native'
        with patch('validate.subprocess.check_output',return_value=json.dumps(self.record)),patch('validate.subprocess.run') as run:
            with self.assertRaises(ValueError):validate('image',self.resolved)
            run.assert_not_called()

    def test_failed_interactive_startup_cannot_pass_validation(self):
        with patch('validate.subprocess.check_output',return_value=json.dumps(self.record)),patch('validate.check_repl',side_effect=ValueError('startup failed')),patch('validate.subprocess.run') as run:
            with self.assertRaisesRegex(ValueError,'startup failed'):
                validate('image',self.resolved)
            run.assert_not_called()

    def test_push_without_registry_digest_is_not_success(self):
        with self.assertRaises(ValueError):publish.pushed_digest('upload started')
        self.assertEqual(publish.pushed_digest('digest: sha256:'+'e'*64),'sha256:'+'e'*64)

    def exercise_publication(self,remote_error=False):
        image='sha256:'+'f'*64
        with tempfile.TemporaryDirectory() as tmp:
            spec=Path(tmp)/'resolved.json';spec.write_text(json.dumps(self.resolved))
            report=Path(tmp)/'published.json'
            def command(cmd,**kwargs):
                return subprocess.CompletedProcess(cmd,0,stdout='digest: sha256:'+'e'*64+'\n')
            with patch.object(sys,'argv',['publish.py','--image',image,'--resolved',str(spec),'--output',str(report),'--run-id','42-1']),patch('publish.validate',side_effect=[{},ValueError('registry check failed')] if remote_error else [{},{}]),patch('publish.subprocess.check_output',return_value=image+'\n'),patch('publish.subprocess.run',side_effect=command) as run,contextlib.redirect_stdout(io.StringIO()):
                if remote_error:
                    with self.assertRaises(ValueError):publish.main()
                else:
                    publish.main();self.assertEqual(json.loads(report.read_text())['registry_pull_validation'],'passed')
                return [call.args[0] for call in run.call_args_list]

    def test_failed_pulled_image_cannot_replace_pilot(self):
        commands=self.exercise_publication(remote_error=True)
        self.assertFalse(any(publish.REPOSITORY+':pilot' in cmd for cmd in commands))

    def test_promotion_reuses_the_tested_image_and_never_updates_latest(self):
        commands=self.exercise_publication()
        tags=[cmd for cmd in commands if cmd[:2]==['docker','tag']]
        self.assertEqual(len(tags),2)
        self.assertEqual(tags[0][2],tags[1][2])
        self.assertEqual(tags[1][-1],publish.REPOSITORY+':pilot')
        self.assertFalse(any(':latest' in arg for cmd in commands for arg in cmd))


if __name__=='__main__':unittest.main()
