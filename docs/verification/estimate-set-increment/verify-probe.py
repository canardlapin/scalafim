#!/usr/bin/env python3
"""Independently verify every value from EstimateProcessProbe using only stdlib."""
import argparse
import hashlib
import json
import struct
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument('root', type=Path)
args = parser.parse_args()
root = args.root
manifest = root / 'units/00000000-0000-4000-8000-000000000014/estimates.json'
representation = json.loads(manifest.read_text())['Content']['Representations'][0]
count = 0
checksum = 0
for name in ('values', 'validity'):
    reference = representation[name]
    path = root / reference['Path']
    assert path.stat().st_size == reference['Bytes']
    with path.open('rb') as stream:
        assert hashlib.file_digest(stream, 'sha256').hexdigest() == reference['SHA256']
    with path.open('rb') as stream:
        header = stream.read(352)
        assert struct.unpack_from('<i', header, 0)[0] == 348
        assert struct.unpack_from('<h', header, 254)[0] == 1
        offset = int(struct.unpack_from('<f', header, 108)[0])
        stream.seek(offset)
        if name == 'values':
            while chunk := stream.read(65536):
                for (value,) in struct.iter_unpack('<d', chunk):
                    expected = (count // 65536) * 100000 + count % 65536
                    assert value == expected, (count, value, expected)
                    count += 1
                    checksum += int(value)
        else:
            validity_count = 0
            while chunk := stream.read(65536):
                assert chunk.count(0) == len(chunk)
                validity_count += len(chunk)
            assert validity_count == count
assert count == 16777216
assert checksum == 214459251425280
print(json.dumps({'cells': count, 'checksum': checksum, 'all_values_verified': True}))
