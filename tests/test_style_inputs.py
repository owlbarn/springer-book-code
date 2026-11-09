import hashlib
import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'scripts'))
import prepare_style
class StyleCacheTests(unittest.TestCase):
    def fixture(self,root):
        cache=root/'cache';folder=cache/'vgg19';folder.mkdir(parents=True)
        data=b'integrity fixture';(folder/'tensor.npy').write_bytes(data)
        manifest=root/'manifest.json';manifest.write_text(json.dumps({'models':[{'name':'vgg19','nodes':1,'tensors':1,'parameters':1,'source':{'sha256':'original'},'files':[{'path':'tensor.npy','bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()}]}]}))
        return cache,manifest
    def test_receipt_and_tampered_tensor(self):
        with tempfile.TemporaryDirectory() as temporary:
            cache,manifest=self.fixture(Path(temporary))
            with patch.object(prepare_style,'MANIFEST',manifest):
                self.assertEqual(prepare_style.check(cache)['models']['vgg19']['parameters'],1)
                (cache/'vgg19/tensor.npy').write_bytes(b'changed')
                with self.assertRaisesRegex(ValueError,'Changed style tensor'):prepare_style.check(cache)
    def test_original_marshal_and_untracked_models_rejected(self):
        for relative in ('vgg19/original.weight','unexpected-model'):
            with self.subTest(relative=relative),tempfile.TemporaryDirectory() as temporary:
                cache,manifest=self.fixture(Path(temporary));(cache/relative).write_bytes(b'not in reader cache')
                with patch.object(prepare_style,'MANIFEST',manifest),self.assertRaisesRegex(ValueError,'extra portable'):
                    prepare_style.check(cache)
    def test_migration_never_overwrites_cache(self):
        with tempfile.TemporaryDirectory() as temporary:
            cache,manifest=self.fixture(Path(temporary))
            with self.assertRaisesRegex(ValueError,'overwrite'):
                prepare_style.migrate(cache,Path(temporary)/'no-reader-needed')
if __name__=='__main__':unittest.main()
