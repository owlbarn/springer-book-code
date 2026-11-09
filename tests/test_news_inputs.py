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
import prepare_news
class NewsInputsTests(unittest.TestCase):
    def fixture(self,root):
        raw=b'test-only one document\n';compressed=gzip.compress(raw,mtime=0)
        manifest=root/'manifest.json'
        manifest.write_text(json.dumps({'repository':'test fixture','revision':'fixed','documents':1,
          'download_url':'https://example.invalid/news.gz','size':len(compressed),
          'sha':hashlib.sha1(b'blob '+str(len(compressed)).encode()+b'\0'+compressed).hexdigest(),
          'sha256':hashlib.sha256(compressed).hexdigest(),'decoded_bytes':len(raw),
          'decoded_sha256':hashlib.sha256(raw).hexdigest()}))
        return manifest,raw,compressed
    def test_fetch_verifies_both_representations_and_reuses_matching_input(self):
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary);manifest,raw,data=self.fixture(root)
            with patch('prepare_news.urllib.request.urlopen',return_value=io.BytesIO(data)):
                path,evidence=prepare_news.prepare(root/'inputs',manifest)
            self.assertEqual(path.read_bytes(),raw)
            self.assertEqual(evidence,prepare_news.check(path,manifest))
            with patch('prepare_news.urllib.request.urlopen') as fetch:
                self.assertEqual(prepare_news.prepare(root/'inputs',manifest),(path,evidence));fetch.assert_not_called()
            path.write_bytes(b'different author-owned input')
            with self.assertRaises(ValueError):prepare_news.prepare(root/'inputs',manifest)
            self.assertEqual(path.read_bytes(),b'different author-owned input')
    def test_corrupt_git_compressed_and_decoded_content_never_becomes_input(self):
        for field in ('sha','sha256','decoded_sha256'):
            with self.subTest(field=field),tempfile.TemporaryDirectory() as temporary:
                root=Path(temporary);manifest,raw,data=self.fixture(root)
                metadata=json.loads(manifest.read_text());metadata[field]='0'*len(metadata[field]);manifest.write_text(json.dumps(metadata))
                with patch('prepare_news.urllib.request.urlopen',return_value=io.BytesIO(data)),self.assertRaises(ValueError):
                    prepare_news.prepare(root/'inputs',manifest)
                self.assertEqual(list((root/'inputs').iterdir()),[])
if __name__=='__main__':unittest.main()
