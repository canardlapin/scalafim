#!/usr/bin/env python3
"""Check the exact combined sources and the completed local integration gates."""
import gzip
import hashlib
import json
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
inputs = json.loads((packet / 'inputs.json').read_text())
manifest = json.loads((packet / 'manifest.json').read_text())
for relative, expected in manifest['sha256'].items():
    assert hashlib.sha256((root / relative).read_bytes()).hexdigest() == expected, relative
for relative, expected in inputs['preservedMainFiles'].items():
    assert hashlib.sha256((root / relative).read_bytes()).hexdigest() == expected, relative
validation = json.loads((packet / 'validation.json').read_text())
assert validation['mainReady'] and not validation['productionQualified']
assert not validation['hostedCiRun'] and not validation['mainMoved']
assert len(validation['results']) == 13
results = {r['command']: r for r in validation['results']}
assert len(results) == 13
expected = {
    'fitJVM/test': (738, 0), 'fitJS/test': (680, 0),
    'designJVM/test': (481, 0), 'designJS/test': (480, 0),
    'datasetJVM/test': (76, 0), 'datasetJS/test': (62, 0),
    'mvpaJVM/test': (468, 3), 'mvpaJS/test': (468, 3),
    'phrfComparisonJVM/test': (466, 10), 'phrfComparisonJS/test': (243, 0),
}
for command, (passed, skipped) in expected.items():
    result = results[command]
    assert result['passed'] == passed and result['skipped'] == skipped
for platform in ('JVM', 'JS'):
    selected = [r for r in results.values() if r['command'].startswith('firstLevelLaws' + platform)]
    assert len(selected) == 1 and selected[0]['passed'] == 19 and selected[0]['skipped'] == 0
combined = '\n'.join(gzip.decompress((packet / f).read_bytes()).decode()
                     for f in ('jvm.log.gz', 'js-core.log.gz', 'consumers.log.gz'))
assert '[error]' not in combined
assert all('multiple main classes detected' in line
           for line in combined.splitlines() if '[warn]' in line)
for result in results.values():
    assert result['exitCode'] == 0
    assert f"[sbt-warm] {result['command']}: exit 0" in combined
    if 'passed' in result:
        assert f"Passed: Total {result['total']}, Failed 0, Errors 0, Passed {result['passed']}" in combined
backup = Path(inputs['backup']['path'])
if backup.exists():
    assert backup.stat().st_size == inputs['backup']['bytes']
    assert hashlib.sha256(backup.read_bytes()).hexdigest() == inputs['backup']['sha256']
print(f"Verified {len(manifest['sha256'])} hashes and all 13 local gates. Engineering main-ready; production unqualified.")
