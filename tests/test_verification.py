import copy
import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
ROOT=Path(__file__).resolve().parents[1]
sys.path[:0]=[str(ROOT/'scripts'),str(ROOT/'docker')]
import verification as v
import ci_plan


class VerificationTests(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory();self.addCleanup(self.temp.cleanup)
        self.root=Path(self.temp.name)/'repo';self.root.mkdir()
        self.env={'ocaml':'5.5.2','sources':{'owl':'a','owl-plplot':'b','owl-ode':'c'},
                  'base_image':'ubuntu@digest','packages':'owl 1','system_packages':'libblas=1',
                  'dependency_patches':{n:{'sha256':n} for n in ['owl-ode','owl-conv','camlimages']},
                  'build_inputs':{'code/dune':'original'},'datasets':{'news':{'sha256':'news'}}}
        self.chapters={'03':{'directory':'code/chap03_stats','output':'chapter03','plots':[]},
                       '12':{'directory':'code/chap12_nlp','output':'chapter12','plots':[],'datasets':['news']}}
        for name,text in [('Dockerfile','FROM base'),('scripts/verification.py','policy'),
                          ('scripts/run_sessions.py','sessions'),('scripts/book_examples.py','examples'),('scripts/run_chapter03.py','run03'),
                          ('scripts/run_chapter12.py','run12'),('docker/validate.py','validator'),
                          ('code/chap03_stats/native.ml','stats'),('code/chap12_nlp/native.ml','lda'),
                          ('code/chap12_nlp/README.md','docs')]:self.write(name,text)
        self.write('docker/chapters.json',json.dumps(self.chapters))
        self.write('environment.json',json.dumps(self.env))
        self.cache=self.root.parent/'cache';self.output=self.root.parent/'output'
        self.policy={'schema':1,'complete':True,'chapters':{},'examples':{}}
        for number,spec in self.chapters.items():
            target=self.cache/'verification'/spec['output'];target.mkdir(parents=True)
            (target/'results.json').write_text('{}')
            (target/'execution.json').write_text('original full run')
            self.policy['chapters'][number]=v.receipt(target,v.chapter_key(self.root,number,spec,self.env),'full')
        (self.cache/'verification/policy.json').write_text(json.dumps(self.policy))
        (self.cache/'origin.json').write_text(json.dumps({'registry_digest':'repo@sha256:abc'}))

    def write(self,name,text):
        p=self.root/name;p.parent.mkdir(parents=True,exist_ok=True);p.write_text(text)

    def key(self,n='12',env=None):return v.chapter_key(self.root,n,self.chapters[n],env or self.env)

    def test_ordinary_edit_does_not_invalidate_training(self):
        k12=self.key();k03=self.key('03')
        self.write('code/chap03_stats/native.ml','new stats')
        self.assertEqual(k12,self.key());self.assertNotEqual(k03,self.key('03'))

    def test_docs_and_new_examples_do_not_invalidate_historical_chapter(self):
        key=self.key()
        self.write('code/chap12_nlp/README.md','new docs')
        self.write('code/chap12_nlp/examples/new.ml','new example')
        self.assertEqual(key,self.key())

    def test_environment_and_dataset_changes_invalidate_training(self):
        for field,value in [('ocaml','next'),('sources',{'owl':'next'}),('base_image','next'),
                            ('packages','new'),('system_packages','new'),('dependency_patches',{}),
                            ('build_inputs',{}),('datasets',{'news':{'sha256':'new'}}),('owl_cflags','new')]:
            with self.subTest(field=field):
                env=copy.deepcopy(self.env);env[field]=value
                self.assertNotEqual(self.key(),self.key(env=env))

    def test_source_runner_and_validation_changes_invalidate_training(self):
        for name in ['code/chap12_nlp/native.ml','scripts/run_chapter12.py','scripts/run_sessions.py',
                     'scripts/verification.py','docker/validate.py','Dockerfile','scripts/prepare_news.py']:
            key=self.key();self.write(name,'changed '+name)
            self.assertNotEqual(key,self.key())

    def test_declared_support_checker_invalidates_its_chapter_only(self):
        self.write('code/chap03_stats/listings.json',json.dumps({'support_scripts':['scripts/check_stats.py']}))
        self.write('scripts/check_stats.py','old checker')
        k03=self.key('03');k12=self.key()
        self.write('scripts/check_stats.py','new checker')
        self.assertNotEqual(k03,self.key('03'));self.assertEqual(k12,self.key())
        mapping=json.loads((ROOT/'code/chap02_maths/listings.json').read_text())
        self.assertIn('scripts/check_chapter02_sessions.py',mapping['support_scripts'])

    def test_corrupt_missing_and_quick_proofs_cannot_be_reused(self):
        target=self.cache/'verification/chapter12';entry=self.policy['chapters']['12']
        self.assertTrue(v.reusable(target,entry,self.key()))
        for action in ['quick','unknown']:
            modified={**entry,'action':action};self.assertFalse(v.reusable(target,modified,self.key()))
        (target/'execution.json').write_text('corrupted')
        self.assertFalse(v.reusable(target,entry,self.key()))
        (target/'results.json').unlink();self.assertFalse(v.reusable(target,entry,self.key()))

    def test_receipt_path_traversal_rejected(self):
        entry={**self.policy['chapters']['12'],'files':{'../results.json':'x'}}
        self.assertFalse(v.reusable(self.cache/'verification/chapter12',entry,self.key()))

    def fake_run(self,cmd,check):
        target=Path(cmd[cmd.index('--output')+1]);target.mkdir(parents=True,exist_ok=True)
        (target/'results.json').write_text(json.dumps({'validation_level':'quick' if '--quick' in cmd else 'full'}))

    def test_unchanged_auto_reuses_full_results_without_execution(self):
        with patch.object(v.subprocess,'run') as execute:
            policy=v.run(self.root,self.output,'auto',self.cache)
        execute.assert_not_called()
        self.assertEqual({e['action'] for e in policy['chapters'].values()},{'reused-full'})
        v.validate_policy(self.root,self.output,self.env,self.chapters,{})
        self.assertEqual(policy['chapters']['12']['origin']['registry_digest'],'repo@sha256:abc')

    def test_changed_chapter_runs_only_that_chapter(self):
        self.write('code/chap03_stats/native.ml','changed')
        with patch.object(v.subprocess,'run',side_effect=self.fake_run) as execute:
            policy=v.run(self.root,self.output,'auto',self.cache)
        self.assertEqual(execute.call_count,1)
        self.assertIn('run_chapter03.py',execute.call_args.args[0][1])
        self.assertEqual(policy['chapters']['12']['action'],'reused-full')

    def test_full_forces_execution_and_missing_cache_falls_back_to_full(self):
        for mode,cache in [('full',self.cache),('auto',None)]:
            with patch.object(v.subprocess,'run',side_effect=self.fake_run) as execute:
                policy=v.run(self.root,self.output,mode,cache)
            self.assertEqual(execute.call_count,2)
            self.assertEqual({e['action'] for e in policy['chapters'].values()},{'full'})

    def test_quick_runs_real_runner_option_but_cannot_publish_or_reuse(self):
        with patch.object(v.subprocess,'run',side_effect=self.fake_run) as execute:
            policy=v.run(self.root,self.output,'quick')
        self.assertIn('--quick',execute.call_args.args[0])
        self.assertFalse(policy['complete'])
        with self.assertRaises(ValueError):v.validate_policy(self.root,self.output,self.env,self.chapters,{})
        v.validate_policy(self.root,self.output,self.env,self.chapters,{},require_complete=False)
        (self.output/'chapter12/results.json').write_text('tampered')
        with self.assertRaises(ValueError):v.validate_policy(self.root,self.output,self.env,self.chapters,{},require_complete=False)

    def test_pipeline_ignores_docs_build_products_but_tracks_example_data(self):
        initial=v.pipeline_inputs(self.root)
        self.write('code/chap12_nlp/README.md','updated')
        self.write('code/_build/foo','compiled')
        self.assertEqual(initial,v.pipeline_inputs(self.root))
        self.write('code/chap12_nlp/examples/demo.ml','demo')
        self.write('code/chap12_nlp/examples/demo.data/text.md','input')
        changed=v.pipeline_inputs(self.root)
        self.write('code/chap12_nlp/examples/demo.data/text.md','different input')
        self.assertNotEqual(changed,v.pipeline_inputs(self.root))

    def test_new_example_runs_while_all_historical_results_are_reused(self):
        self.write('code/chap12_nlp/examples/demo.ml','let () = print_endline "demo"')
        def example_run(root, identity, target):
            target.mkdir(parents=True)
            report={'id':identity,'status':'passed'}
            (target/'result.json').write_text(json.dumps(report))
            return report
        with patch.object(v.subprocess,'run') as chapters,patch('book_examples.run_example',side_effect=example_run) as example:
            policy=v.run(self.root,self.output,'auto',self.cache)
        chapters.assert_not_called();self.assertEqual(example.call_count,1)
        self.assertEqual(policy['examples']['12/demo']['action'],'full')
        self.assertEqual(policy['chapters']['12']['action'],'reused-full')

    def test_quick_cache_does_not_satisfy_auto(self):
        self.policy['complete']=False
        self.policy['chapters']['12']['action']='quick'
        (self.cache/'verification/policy.json').write_text(json.dumps(self.policy))
        with patch.object(v.subprocess,'run',side_effect=self.fake_run) as execute:
            policy=v.run(self.root,self.output,'auto',self.cache)
        self.assertEqual(execute.call_count,2)
        self.assertTrue(policy['complete'])
        self.assertEqual({e['action'] for e in policy['chapters'].values()},{'full'})

    def test_one_corrupted_receipt_reruns_only_that_chapter(self):
        (self.cache/'verification/chapter12/execution.json').write_text('corrupted')
        with patch.object(v.subprocess,'run',side_effect=self.fake_run) as execute:
            policy=v.run(self.root,self.output,'auto',self.cache)
        self.assertEqual(execute.call_count,1)
        self.assertIn('run_chapter12.py',execute.call_args.args[0][1])
        self.assertEqual(policy['chapters']['03']['action'],'reused-full')
        self.assertEqual(policy['chapters']['12']['action'],'full')

    def test_daily_no_change_skips_but_environment_source_and_full_rebuild(self):
        resolved={'ocaml':self.env['ocaml'],'base_image':self.env['base_image'],**self.env['sources'],
                  'owl-ode-patch':'owl-ode','owl-conv-patch':'owl-conv','camlimages-patch':'camlimages'}
        inputs=v.pipeline_inputs(self.root);baseline={**self.env,'pipeline_inputs':inputs}
        self.assertFalse(ci_plan.needs_build(resolved,inputs,baseline,'auto')[0])
        self.assertTrue(ci_plan.needs_build(resolved,inputs,baseline,'full')[0])
        self.assertTrue(ci_plan.needs_build(resolved,inputs,None,'auto')[0])
        self.assertTrue(ci_plan.needs_build({**resolved,'ocaml':'next'},inputs,baseline,'auto')[0])
        self.assertTrue(ci_plan.needs_build(resolved,{},baseline,'auto')[0])


if __name__=='__main__':unittest.main()
