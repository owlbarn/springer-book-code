import gzip
import hashlib
import io
import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'scripts'))
import prepare_mnist

class MnistInputsTests(unittest.TestCase):
    def fixture(self,root):
        raw=b'known test-only bytes'
        data=gzip.compress(raw,mtime=0)
        item={'path':'fixture.gz','size':len(data),'url':'https://example.invalid/pinned.gz',
              'sha':hashlib.sha1(b'blob '+str(len(data)).encode()+b'\0'+data).hexdigest(),
              'sha256':hashlib.sha256(data).hexdigest(),'decoded_size':len(raw),
              'decoded_sha256':hashlib.sha256(raw).hexdigest()}
        manifest=root/'manifest.json'
        manifest.write_text(json.dumps({'repository':'test fixture','revision':'fixed','files':[item],'license_declaration':{'declared_license':'test fixture'}}))
        return manifest,raw,data
    def test_fetch_checks_both_representations_and_reuses_verified_cache(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);manifest,raw,data=self.fixture(root);directory=root/'inputs'
            with patch('prepare_mnist.urllib.request.urlopen',return_value=io.BytesIO(data)):
                result=prepare_mnist.prepare(directory,manifest)
            self.assertEqual((directory/'fixture').read_bytes(),raw)
            self.assertEqual(result,prepare_mnist.check(directory,manifest))
            with patch('prepare_mnist.urllib.request.urlopen') as download:
                prepare_mnist.prepare(directory,manifest);download.assert_not_called()
    def test_wrong_download_and_changed_cache_are_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);manifest,raw,data=self.fixture(root);directory=root/'inputs'
            with patch('prepare_mnist.urllib.request.urlopen',return_value=io.BytesIO(b'wrong')):
                with self.assertRaisesRegex(ValueError,'Unexpected compressed'):
                    prepare_mnist.prepare(directory,manifest)
            self.assertFalse((directory/'fixture').exists())
            (directory/'fixture').write_bytes(b'changed existing input')
            with self.assertRaisesRegex(ValueError,'Missing or changed'):
                prepare_mnist.check(directory,manifest)
            with patch('prepare_mnist.urllib.request.urlopen') as download:
                with self.assertRaisesRegex(ValueError,'Refusing to replace'):
                    prepare_mnist.prepare(directory,manifest)
                download.assert_not_called()

if __name__=='__main__':unittest.main()
