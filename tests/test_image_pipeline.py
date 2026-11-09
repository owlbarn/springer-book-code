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
        self.resolved={'ocaml':'5.5.2','owl':'a'*40,'owl-plplot':'b'*40,'owl-ode':'1'*40,'owl-ode-patch':'patch-sha','owl-conv-patch':'conv-sha',
                       'base_image':'ubuntu@sha256:'+'c'*64,
                       'code_revision':'d'*40,'code_dirty':False}
        self.record={'environment':{'ocaml':'5.5.2','sources':{'owl':'a'*40,'owl-plplot':'b'*40,'owl-ode':'1'*40},
                       'dependency_patches':{'owl-ode':{'sha256':'patch-sha'},'owl-conv':{'sha256':'conv-sha','patched_file_sha256':'patched'}},
                       'owl_cflags':'-O3 -march=x86-64','base_image':self.resolved['base_image'],'code_revision':'d'*40,'code_dirty':'false','source_hashes':{'example.ml':'source-hash'}},
                     'chapters':{'02':{'plots':['plot.png']}},
                     'verification':{'02':{'ocaml':'5.5.2','files':[{'name':'plot.png'}],'sources':{'example.ml':'source-hash'}}}}

        self.record['environment']['build_inputs']={name:'build-hash' for name in
            ['code/dune-project','code/dune','code/springer-book-code.opam','code/chap04_linear-algebra/imageUtils.ml']}
        self.record['environment']['source_hashes'].update(self.record['environment']['build_inputs'])

    def test_missing_or_changed_build_inputs_rejected_before_reader_run(self):
        for name in ('missing','code/dune-project','code/chap04_linear-algebra/imageUtils.ml'):
            with self.subTest(name=name):
                record=json.loads(json.dumps(self.record))
                if name=='missing':record['environment'].pop('build_inputs')
                else:record['environment']['build_inputs'][name]='stale'
                with patch('validate.subprocess.check_output',return_value=json.dumps(record)),patch('validate.check_repl') as repl:
                    with self.assertRaisesRegex(ValueError,'shared/build input evidence'):
                        validate('image',self.resolved)
                    repl.assert_not_called()

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

    def test_stale_baked_results_are_rejected(self):
        self.record['verification']['02']['sources']['example.ml']='old-hash'
        with patch('validate.subprocess.check_output',return_value=json.dumps(self.record)),patch('validate.check_repl') as repl:
            with self.assertRaisesRegex(ValueError,'do not match image sources'):
                validate('image',self.resolved)
            repl.assert_not_called()

    def test_missing_convolution_patch_cannot_reach_reader_checks(self):
        self.record['environment']['dependency_patches'].pop('owl-conv')
        with patch('validate.subprocess.check_output',return_value=json.dumps(self.record)),patch('validate.check_repl') as repl:
            with self.assertRaisesRegex(ValueError,'blocked-convolution accuracy patch'):
                validate('image',self.resolved)
            repl.assert_not_called()

    def test_complete_exact_reader_validation_reused_without_training_again(self):
        current=dict(self.record,image='sha256:exact',runtime_check='not-run')
        prior=dict(current,runtime_check='passed',interactive_check={'status':'passed','terminal':'pty'})
        with tempfile.TemporaryDirectory() as temporary:
            path=Path(temporary)/'validation.json';path.write_text(json.dumps(prior))
            with patch('publish.validate',return_value=current) as check:
                self.assertEqual(publish.validate_before_publish('sha256:exact',self.resolved,path),prior)
                check.assert_called_once_with('sha256:exact',self.resolved,execute=False)

    def test_reused_validation_rejects_stale_image_evidence_and_incomplete_runs(self):
        current=dict(self.record,image='sha256:exact',runtime_check='not-run')
        for key in ('image','environment','chapters','verification','runtime_check','interactive_check'):
            with self.subTest(key=key),tempfile.TemporaryDirectory() as temporary:
                prior=dict(current,runtime_check='passed',interactive_check={'status':'passed','terminal':'pty'})
                prior[key]='different' if key!='interactive_check' else {'status':'failed','terminal':'pty'}
                path=Path(temporary)/'validation.json';path.write_text(json.dumps(prior))
                with patch('publish.validate',return_value=current),self.assertRaisesRegex(ValueError,'Previous validation'):
                    publish.validate_before_publish('sha256:exact',self.resolved,path)

    def test_missing_ode_accuracy_patch_is_rejected(self):
        self.record['environment']['dependency_patches']={}
        with patch('validate.subprocess.check_output',return_value=json.dumps(self.record)),patch('validate.check_repl') as repl:
            with self.assertRaisesRegex(ValueError,'ODE accuracy patch'):
                validate('image',self.resolved)
            repl.assert_not_called()

    def test_silent_extra_skip_is_rejected(self):
        self.record['verification']['02']['blocked_regions']={'example':'unexpected skip'}
        with patch('validate.subprocess.check_output',return_value=json.dumps(self.record)),patch('validate.check_repl') as repl:
            with self.assertRaisesRegex(ValueError,'skipped coverage differs'):
                validate('image',self.resolved)
            repl.assert_not_called()

    def test_missing_camlimages_patch_cannot_pass_chapter14(self):
        self.record['chapters']['14']=dict(self.record['chapters']['02'])
        self.record['verification']['14']=dict(self.record['verification']['02'])
        self.resolved['camlimages-patch']='expected'
        for patch_record in ({},{'sha256':'other','patched_files':{'dune':'sha'}}):
            with self.subTest(patch=patch_record):
                self.record['environment']['dependency_patches']['camlimages']=patch_record
                with patch('validate.subprocess.check_output',return_value=json.dumps(self.record)),patch('validate.check_repl') as repl:
                    with self.assertRaisesRegex(ValueError,'CamlImages build compatibility patch'):
                        validate('image',self.resolved)
                    repl.assert_not_called()

    def test_unreported_illustrative_scope_is_rejected(self):
        self.record['verification']['02']['illustrations']=['not-executed']
        with patch('validate.subprocess.check_output',return_value=json.dumps(self.record)),patch('validate.check_repl') as repl:
            with self.assertRaisesRegex(ValueError,'illustrative scope differs'):
                validate('image',self.resolved)
            repl.assert_not_called()

    def test_missing_or_stale_dataset_evidence_is_rejected(self):
        self.record['chapters']['02']['datasets']=['mnist']
        for evidence in (None, {'files':{'data':'stale-hash'}}):
            with self.subTest(evidence=evidence):
                self.record['environment']['datasets']={'mnist':{'files':{'data':'current-hash'}}}
                self.record['verification']['02']['datasets']={} if evidence is None else {'mnist':evidence}
                with patch('validate.subprocess.check_output',return_value=json.dumps(self.record)),patch('validate.check_repl') as repl:
                    with self.assertRaisesRegex(ValueError,'dataset evidence differs'):
                        validate('image',self.resolved)
                    repl.assert_not_called()

    def test_runtime_network_is_only_enabled_for_explicit_external_inputs(self):
        for external in (False,True):
            with self.subTest(external=external):
                self.record['chapters']['02']['external_network']=external
                with patch('validate.subprocess.check_output',return_value=json.dumps(self.record)),patch('validate.check_repl'),patch('validate.subprocess.run') as run:
                    validate('image',self.resolved)
                command=run.call_args.args[0]
                self.assertEqual('--network' not in command,external)

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
