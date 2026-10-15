#!/usr/bin/env python3
"""Verify moved fixture bytes and unchanged qualification assertions against the landing base."""
import hashlib
import json
from pathlib import Path
import subprocess

root=Path(__file__).resolve().parents[3]
base='eecae59f'
moves=json.loads(Path(__file__).with_name('moves.json').read_text())
checks=[]
for move in moves:
    old=subprocess.check_output(['git','show',base+':'+move['old']],cwd=root)
    assert hashlib.sha256(old).hexdigest()==move['sha256'],move['old']
    new=(root/move['new']).read_bytes()
    if move['new'].endswith('RealAssets.scala'):
        old=old[:old.index(b'/** Real-asset suites skip')]
    if move['new'].endswith('.scala'):
        new=new.replace(b'\nimport scalafim.testkit.RealAssetGate\n',b'')
        new=new.replace(b'surfaceTestFixturesJVM/runMain',b'surfaceJVM/Test/runMain')
    assert new==old,move['new']
    checks.append(dict(path=move['new'],original_sha256=move['sha256'],verified=True))
# The two atlas consumer courts receive only the moved gate import.
for rel in ['modules/atlas/jvm/src/test/scala/scalafim/atlas/io/StandardSurfaceRouteFilesSuite.scala',
            'modules/atlas/jvm/src/test/scala/scalafim/atlas/reference/Fslr32kRouteBudgetSuite.scala',
            'modules/surface/jvm/src/test/scala/scalafim/surface/reference/DeclaredSurfaceReaderSuite.scala']:
    old=subprocess.check_output(['git','show',base+':'+rel],cwd=root)
    new=(root/rel).read_bytes().replace(b'\nimport scalafim.testkit.RealAssetGate\n',b'')
    assert new==old,rel
    checks.append(dict(path=rel,assertions_unchanged=True))
rel='modules/surface/js/src/test/scala/scalafim/surface/reference/RealPointwiseJsSuite.scala'
old=subprocess.check_output(['git','show',base+':'+rel],cwd=root)
new=(root/rel).read_bytes().replace(b'modules/surface-test-fixtures/jvm/src/main/resources/pointmap-real',b'modules/surface/jvm/src/test/resources/pointmap-real')
new=new.replace(b'The oracle fixture lives in the unpublished JVM fixture module',b'The oracle fixture lives with the JVM test resources')
assert new==old,rel
checks.append(dict(path=rel,assertions_unchanged=True))
print(json.dumps(dict(base=base,checks=checks,pass_all=True),indent=2))
