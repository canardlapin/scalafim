#!/usr/bin/env python3
"""Verify this retained diagnostic packet; never declare PHRF qualification."""
import hashlib
import json
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
manifest = json.loads((packet / 'manifest.json').read_text())
for name, expected in manifest['sha256'].items():
    actual = hashlib.sha256((root / name).read_bytes()).hexdigest()
    if actual != expected:
        raise SystemExit(f'hash mismatch: {name}')
for path in packet.glob('b0-*.json'):
    result = json.loads(path.read_text())
    if result['qualification'] != 'not-admitted':
        raise SystemExit(f'unsupported qualification: {path.name}')
    if 'attemptedDelivered' in result:
        attempted, emitted = result['attemptedDelivered'], result['emitted']
        checks = [sum(result['statuses'].values()) == attempted,
                  result['statuses'].get('Accepted', 0) == emitted,
                  0 <= emitted <= attempted <= result['voxels'],
                  result['float32OutputBytes'] == emitted * result['trials'] * 4,
                  result['retainedSinkBytes'] == result['trials'] * 4]
        if not all(checks):
            raise SystemExit(f'inconsistent attempted/output receipt: {path.name}')
print(f"Verified {len(manifest['sha256'])} hashes and diagnostic receipts; PHRF-33 remains not admitted.")
