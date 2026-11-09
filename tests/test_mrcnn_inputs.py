import hashlib
import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'scripts'))
import prepare_mrcnn
class PortableMrcnnCacheTests(unittest.TestCase):
    def fixture(self,root):
        directory=root/'cache';directory.mkdir();raw=b'test-only integrity fixture'
        (directory/'tensor.npy').write_bytes(raw)
        manifest=root/'manifest.json';manifest.write_text(json.dumps({'original_hdf5':{'sha256':'original'},'datasets':1,'parameters':1,'float32_roundtrip_exact':True,'files':[{'path':'tensor.npy','bytes':len(raw),'sha256':hashlib.sha256(raw).hexdigest()}]}))
        return directory,manifest
    def test_complete_cache_receipt_and_changed_tensor_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory,manifest=self.fixture(Path(temporary))
            with patch.object(prepare_mrcnn,'MANIFEST',manifest):
                receipt=prepare_mrcnn.check(directory);self.assertEqual(receipt['datasets'],1)
                (directory/'tensor.npy').write_bytes(b'changed')
                with self.assertRaisesRegex(ValueError,'Changed portable'):prepare_mrcnn.check(directory)
    def test_extra_original_exchange_file_cannot_enter_reader_cache(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory,manifest=self.fixture(Path(temporary));(directory/'original.hdf5').write_bytes(b'not for the reader layer')
            with patch.object(prepare_mrcnn,'MANIFEST',manifest),self.assertRaisesRegex(ValueError,'extra portable'):
                prepare_mrcnn.check(directory)
if __name__=='__main__':unittest.main()
