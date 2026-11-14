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
import prune_tags
import publish
from validate import validate


class PipelineTests(unittest.TestCase):
    def setUp(self):
        self.resolved={'ocaml':'5.5.2','owl':'a'*40,'owl-plplot':'b'*40,'owl-ode':'1'*40,'owl-rllib':'2'*40,'owl-ode-patch':'patch-sha','owl-conv-patch':'conv-sha',
                       'base_image':'ubuntu@sha256:'+'c'*64,
                       'code_revision':'d'*40,'code_dirty':False}
        self.record={'environment':{'ocaml':'5.5.2','sources':{'owl':'a'*40,'owl-plplot':'b'*40,'owl-ode':'1'*40},
                       'rllib':{'commit':'2'*40,'python_packages':'gymnasium==0.29.1'},
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
        for rllib in ({'commit':'wrong','python_packages':'x'},{'commit':'2'*40}):
            record=json.loads(json.dumps(self.record));record['environment']['rllib']=rllib
            with patch('validate.subprocess.check_output',return_value=json.dumps(record)),self.assertRaisesRegex(ValueError,'owl-rllib'):
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
        for key in ('image','environment','chapters','verification','examples','policy','runtime_check','interactive_check'):
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

    def test_new_image_reader_and_registry_checks_use_short_profiles(self):
        self.record['environment']['verification_schema']=1
        for profile in ['smoke','registry']:
            with patch('validate.subprocess.check_output',return_value=json.dumps(self.record)),patch('validate.check_repl'),patch('validate.subprocess.run') as run:
                record=validate('image',self.resolved,profile=profile)
            self.assertEqual(run.call_args.args[0][-2:],['--mode',profile])
            self.assertEqual(record['runtime_profile'],profile)

    def test_manual_full_build_bypasses_only_final_verification_cache(self):
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary);spec=root/'resolved.json';spec.write_text(json.dumps(self.resolved))
            commands=[]
            for stamp in [1,2]:
                with patch.object(build,'ROOT',root),patch.object(sys,'argv',['build.py','--resolved',str(spec),'--mode','full']),patch('build.time.time_ns',return_value=stamp),patch('build.subprocess.run') as run,contextlib.redirect_stdout(io.StringIO()):
                    build.main()
                commands.append(run.call_args.args[0])
            self.assertIn('BOOK_VERIFY_NONCE=1',commands[0])
            self.assertIn('BOOK_VERIFY_NONCE=2',commands[1])
            self.assertNotIn('--no-cache',commands[0])

    def test_push_without_registry_digest_is_not_success(self):
        with self.assertRaises(ValueError):publish.pushed_digest('upload started')
        self.assertEqual(publish.pushed_digest('digest: sha256:'+'e'*64),'sha256:'+'e'*64)

    def exercise_publication(self,remote_error=False,remote_image=None,wrong_digest_tag=None,initial_error=False):
        image='sha256:'+'f'*64
        with tempfile.TemporaryDirectory() as tmp:
            spec=Path(tmp)/'resolved.json';spec.write_text(json.dumps(self.resolved))
            report=Path(tmp)/'published.json'
            def command(cmd,**kwargs):
                digest=('0' if cmd==['docker','push',wrong_digest_tag] else 'e')*64
                return subprocess.CompletedProcess(cmd,0,stdout='digest: sha256:'+digest+'\n')
            with patch.object(sys,'argv',['publish.py','--image',image,'--resolved',str(spec),'--output',str(report),'--run-id','42-1']),patch('publish.validate',side_effect=[ValueError('initial validation failed')] if initial_error else ([{},ValueError('registry check failed')] if remote_error else [{},{}])),patch('publish.subprocess.check_output',side_effect=[image+'\n',(remote_image or image)+'\n']),patch('publish.subprocess.run',side_effect=command) as run,contextlib.redirect_stdout(io.StringIO()):
                if remote_error or remote_image or wrong_digest_tag or initial_error:
                    with self.assertRaises(ValueError):publish.main()
                else:
                    publish.main()
                    result=json.loads(report.read_text())
                    self.assertEqual(result['registry_pull_validation'],'passed')
                    self.assertEqual(result['latest_tag'],publish.REPOSITORY+':latest')
                    self.assertNotIn('pilot_tag',result)
                    self.assertNotIn('main_tag',result)
                    self.assertEqual(result['unique_tag'],publish.REPOSITORY+':build-'+self.resolved['code_revision'][:12]+'-42-1')
                    self.assertEqual(result['registry_digest'],'sha256:'+'e'*64)
                return [call.args[0] for call in run.call_args_list]

    def test_failed_pulled_image_cannot_replace_reader_tags(self):
        commands=self.exercise_publication(remote_error=True)
        for tag in ('pilot','main','latest'):
            self.assertFalse(any(publish.REPOSITORY+':'+tag in cmd for cmd in commands))

    def test_promotion_reuses_the_tested_image_and_never_updates_pilot_or_main(self):
        commands=self.exercise_publication()
        tags=[cmd for cmd in commands if cmd[:2]==['docker','tag']]
        self.assertEqual(len(tags),2)
        self.assertTrue(all(cmd[2]==tags[0][2] for cmd in tags))
        self.assertEqual([cmd[-1] for cmd in tags[1:]],
                         [publish.REPOSITORY+':latest'])
        self.assertEqual(commands[-1],['docker','push',publish.REPOSITORY+':latest'])
        self.assertFalse(any(':main' in arg or ':pilot' in arg for cmd in commands for arg in cmd))

    def test_registry_identity_mismatch_cannot_replace_reader_tags(self):
        commands=self.exercise_publication(remote_image='sha256:'+'0'*64)
        self.assertEqual(len([cmd for cmd in commands if cmd[:2]==['docker','tag']]),1)

    def test_failed_initial_validation_never_pushes_any_tag(self):
        self.assertEqual(self.exercise_publication(initial_error=True),[])

    def test_latest_digest_mismatch_cannot_report_success(self):
        self.exercise_publication(wrong_digest_tag=publish.REPOSITORY+':latest')



class PruneTagTests(unittest.TestCase):
    def test_only_superseded_build_tags_are_deleted(self):
        found={'latest':'sha256:new','build-new':'sha256:new','build-old':'sha256:old',
               'build-older':'sha256:older','edition-2':'sha256:old'}
        self.assertEqual(prune_tags.superseded(found),['build-old','build-older'])

    def test_missing_latest_prunes_nothing(self):
        with self.assertRaises(SystemExit):prune_tags.superseded({'build-old':'sha256:old'})


if __name__=='__main__':unittest.main()
