import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'scripts'))
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'docker'))
from book_examples import discover, source_inputs, regions, configuration, run_all
from validate import verify_examples


class ExampleTests(unittest.TestCase):
    def setUp(self):
        temp=tempfile.TemporaryDirectory();self.addCleanup(temp.cleanup)
        self.root=Path(temp.name)
        (self.root/'scripts').mkdir()
        (self.root/'scripts/book_examples.py').write_text('runner fixture')

    def source(self, chapter='03', name='demo'):
        source=self.root/f'code/chap{chapter}_fixture/examples/{name}.ml'
        source.parent.mkdir(parents=True,exist_ok=True)
        source.write_text('let () = print_endline "test"\n')
        return source

    def test_new_chapter_and_existing_chapter_discovered_without_registration(self):
        a=self.source();b=self.source('17','new-plot')
        self.assertEqual(discover(self.root),{'03/demo':a,'17/new-plot':b})
        calls=[]
        with patch('book_examples.run_example',side_effect=lambda root,identity,output: calls.append(identity) or {'status':'passed'}):
            run_all(self.root,self.root/'out')
        self.assertEqual(calls,['03/demo','17/new-plot'])
        self.assertEqual(set(json.loads((self.root/'out/results.json').read_text())),set(calls))

    def test_failed_discovery_run_removes_old_success_summary(self):
        self.source()
        output=self.root/'out';output.mkdir()
        (output/'results.json').write_text('{"old":"passed"}')
        with patch('book_examples.run_example',side_effect=ValueError('failed')):
            with self.assertRaises(ValueError):run_all(self.root,output)
        self.assertFalse((output/'results.json').exists())

    def test_input_record_includes_data_and_options_but_not_other_examples(self):
        source=self.source();self.source('03','other')
        source.with_suffix('.json').write_text('{"timeout":300}')
        source.with_suffix('.data').mkdir()
        (source.with_suffix('.data')/'data.csv').write_text('1,2')
        inputs=source_inputs(self.root,source)
        self.assertEqual(len(inputs),4)
        self.assertFalse(any('other' in name for name in inputs))
        (source.with_suffix('.data')/'data.csv').write_text('3,4')
        self.assertNotEqual(inputs,source_inputs(self.root,source))

    def test_incomplete_or_duplicate_regions_rejected(self):
        valid='(* book: plot begin *)\nlet x = 1\n(* book: plot end *)\n'
        self.assertEqual(regions(valid),{'plot':'let x = 1\n'})
        for text in [valid+valid,'(* book: plot begin *)\nlet x = 1', '(* book: plot end *)']:
            with self.assertRaises(ValueError):regions(text)

    def test_invalid_options_are_not_silently_ignored(self):
        source=self.source()
        for config in [None, {'timeout':0},{'timeout':True},{'packages':'owl'},{'packges':['owl']}]:
            source.with_suffix('.json').write_text(json.dumps(config))
            with self.assertRaises(ValueError):configuration(source)

    def test_new_example_missing_failed_or_stale_cannot_pass_publication(self):
        inputs={'code/chap03_fixture/examples/demo.ml':'hash'}
        env={'ocaml':'5.5.2','example_inputs':{'03/demo':inputs}}
        result={'id':'03/demo','status':'passed','sources':inputs,'ocaml':'5.5.2',
                'execution_sha256':'execution','artifacts':{'source.ml':'hash'}}
        verify_examples({'environment':env,'examples':{'03/demo':result}})
        for actual in [None,{}, {'03/demo':dict(result,status='failed')},
                       {'03/demo':dict(result,sources={})}, {'03/demo':dict(result,ocaml='wrong')}]:
            with self.assertRaises(ValueError):verify_examples({'environment':env,'examples':actual})

    def test_empty_new_example_scope_still_requires_completed_discovery(self):
        with self.assertRaises(ValueError):
            verify_examples({'environment':{'example_inputs':{}},'examples':None})
        verify_examples({'environment':{'example_inputs':{}},'examples':{}})


if __name__=='__main__':unittest.main()
