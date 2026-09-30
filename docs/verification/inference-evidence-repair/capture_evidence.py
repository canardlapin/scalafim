"""Capture this bounded repair's actual source, provider and readback receipts."""
import datetime
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys

WORK = Path(__file__).resolve().parents[3]
OUT = Path(__file__).resolve().parent
ROOT = WORK.parent
ORIGINAL = WORK / 'modules/estimates-io/jvm/src/test/resources/estimate-golden/inference-evidence/verification'
REPORT = 'docs/verification/estimate-set-inference-evidence-2026-09-30.md'
BASE = 'edd1feafdde5ad783a4d2771c2d3fb841cdbcc85'
LOGS = ['inference-evidence-repair-prefixed-repro.log',
        'inference-evidence-repair-jvm-r2.log', 'inference-evidence-repair-js-contract.log',
        'inference-evidence-repair-js-consumers.log', 'inference-evidence-repair-compile.log',
        'inference-evidence-repair-fresh-readback.log']


def write(name, value):
    path = OUT / name
    assert not path.exists(), f'append-only receipt already exists: {path}'
    path.write_text(json.dumps(value, sort_keys=True, indent=2) + '\n')


def git(*args, work=WORK):
    return subprocess.check_output(['git', '-C', str(work), *args], text=True).strip()


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def providers():
    result = []
    for previous in json.loads((ORIGINAL / 'provider-source-closure.json').read_text()):
        path = Path(previous['path'])
        names = git('ls-files', work=path).splitlines()
        result.append({'path': str(path), 'head': git('rev-parse', 'HEAD', work=path),
                       'tree': git('rev-parse', 'HEAD^{tree}', work=path),
                       'status': git('status', '--porcelain', work=path),
                       'trackedFiles': {name: digest(path / name) for name in names}})
    assert all(not value['status'] for value in result)
    return result


def freeze():
    assert git('rev-parse', 'HEAD') == BASE
    names = [p for p in git('ls-files').splitlines() if p != REPORT]
    write('source-freeze.json', {'base': BASE, 'createdUtc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
          'files': {name: {'sha256': digest(WORK / name), 'bytes': (WORK / name).stat().st_size} for name in names},
          'documentaryExclusions': [REPORT, 'docs/verification/inference-evidence-repair/']})
    if (OUT / 'provider-source-closure.json').exists():
        assert providers() == json.loads((OUT / 'provider-source-closure.json').read_text())
    else:
        write('provider-source-closure.json', providers())
    print(f'REPAIR_SOURCE_FROZEN files={len(names)} providers=12')


def readback():
    log = (ROOT / 'logs' / LOGS[1]).read_text()
    meta = json.loads((ROOT / 'logs' / (LOGS[1] + '.meta.json')).read_text())
    assert meta['exit_code'] == 0
    entries = re.findall(r'\[info\] \* Attributed\(([^\n]+)\)', log)
    assert entries and not any('/modules/fit/' in p or '/modules/fit-estimates/' in p for p in entries)
    values = []
    for entry in entries:
        path = Path(entry)
        if path.is_file():
            values.append({'path': entry, 'sha256': digest(path), 'bytes': path.stat().st_size})
        else:
            assert path.is_dir(), entry
            values.append({'path': entry, 'files': {str(p.relative_to(path)): digest(p)
                                                   for p in sorted(path.rglob('*')) if p.is_file()}})
    write('runtime-closure.json', {'classpath': values, 'fitterFree': True})
    homes = re.findall(r'\[info\] ans: String = ([^\n]+)', log)
    home = next((Path(p) for p in homes if Path(p, 'bin/java').is_file()), None)
    assert home
    argv = [str(home / 'bin/java'), '-cp', os.pathsep.join(entries),
            'scalafim.estimates.io.InferenceEvidenceReadbackProbe', str(ORIGINAL.parent)]
    write('readback-argv.json', argv)
    raise SystemExit(subprocess.call(['python3', '/Users/bbuchsbaum/.agents/skills/lean-logs/scripts/run_logged.py',
          '--log', str(ROOT / 'logs' / LOGS[-1]), '--cwd', str(WORK), '--timeout', '120', '--', *argv]))


def finish():
    frozen = json.loads((OUT / 'source-freeze.json').read_text())['files']
    changed = [name for name, info in frozen.items() if digest(WORK / name) != info['sha256']]
    assert not changed, changed
    assert providers() == json.loads((OUT / 'provider-source-closure.json').read_text())
    old = json.loads((ORIGINAL / 'source-stability.json').read_text())['oldGoldenHashes']
    assert all(digest(WORK / name) == value for name, value in old.items())
    logdir = OUT / 'logs'
    logdir.mkdir(exist_ok=False)
    summary = {}
    for name in LOGS:
        raw = ROOT / 'logs' / name
        meta_path = Path(str(raw) + '.meta.json')
        meta = json.loads(meta_path.read_text())
        assert meta['exit_code'] == (1 if 'prefixed-repro' in name else 0), (name, meta)
        text = raw.read_text()
        counts = re.findall(r'Passed: Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)', text)
        if name in LOGS[1:4]:
            assert counts and all(int(c[0]) > 0 and c[1:3] == ('0', '0') for c in counts)
        if name in LOGS[1:5]:
            assert not re.search(r'(?im)\[(?:warn|error)\]|\bskipped\b', text), name
        if name == LOGS[0]:
            assert 'Failed: Total 12, Failed 2, Errors 0, Passed 10' in text
            assert '=> Obtained\nVector(\n  2,\n  99\n)' in text
            assert 'Vector[Byte](99, 99)' in text
        if name == LOGS[-1]:
            assert 'lateCancel=true lateCallback=true tailUnchanged=true' in text
        (logdir / name).write_bytes(raw.read_bytes())
        (logdir / (name + '.meta.json')).write_bytes(meta_path.read_bytes())
        summary[name] = {'exitCode': meta['exit_code'], 'testCounts': counts,
                         'sha256': digest(raw), 'metaSha256': digest(meta_path)}
    write('source-stability.json', {'changedSourceFiles': changed, 'sourceFilesCompared': len(frozen),
          'providerBuildsCompared': 12, 'changedProviderBuilds': [], 'oldGoldenHashes': old,
          'originalFixtureAndReceiptsUnchanged': True})
    for name in ['inference-evidence-repair-jvm.log', 'inference-evidence-repair-jvm.log.meta.json']:
        (logdir / name).write_bytes((ROOT / 'logs' / name).read_bytes())
    historical = ROOT / 'logs' / 'inference-evidence-repair-jvm.log'
    historical_meta = Path(str(historical) + '.meta.json')
    assert json.loads(historical_meta.read_text())['exit_code'] == 1
    assert 'Failed: Total 57, Failed 1, Errors 0, Passed 56' in historical.read_text()
    summary[historical.name] = {'exitCode': 1, 'sha256': digest(historical),
          'metaSha256': digest(historical_meta),
          'failure': 'Unchanged shared-covariance global descriptor equality: 452 decreased to 450; repaired status suite passed.'}
    write('test-summary.json', summary)
    print(f'REPAIR_EVIDENCE_PASS sourceFiles={len(frozen)} providers=12 oldGoldens={len(old)}')


{'freeze': freeze, 'readback': readback, 'finish': finish}[sys.argv[1]]()
