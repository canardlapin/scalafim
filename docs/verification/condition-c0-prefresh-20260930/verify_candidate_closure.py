"""Read-only recheck of the Gaussian candidate's executed artifact closure."""
import datetime
import hashlib
import json
import subprocess
from pathlib import Path

ROOT = Path('/private/tmp/scalafim-phrf-push-20260930')
WORK = ROOT / 'work'
OUT = Path('/private/tmp/scalafim-phrf21-next-20260930')
E = ROOT / 'evidence'
ARCHIVE = WORK / 'docs/verification/condition-stationarity-roundoff-20260930'

def sha(p):
    return hashlib.sha256(p.read_bytes()).hexdigest()

def git(*args, cwd=WORK):
    return subprocess.check_output(['git', *args], cwd=cwd, text=True).strip()

failures, differences = [], []
source = json.loads((E / 'final-source.json').read_text())
binding = json.loads((ARCHIVE / 'final-source-binding.json').read_text())
allowed = {x['path']: x for x in binding['changesSinceCompilation']}
for name, expected in source['sources'].items():
    current = sha(WORK / name)
    if current != expected:
        if name in allowed and allowed[name]['executed'] == expected and allowed[name]['current'] == current:
            differences.append(allowed[name])
        else:
            failures.append({'kind': 'source', 'path': name, 'expected': expected, 'current': current})

cp = json.loads((E / 'jvm-artifacts.json').read_text())
for entry in cp['classpath']:
    p = Path(entry['path'])
    if entry.get('absent'):
        if p.exists():
            failures.append({'kind': 'formerly-absent-classpath', 'path': str(p)})
    elif 'sha256' in entry:
        if not p.exists() or sha(p) != entry['sha256']:
            failures.append({'kind': 'classpath-file', 'path': str(p)})
    else:
        current = {str(x.relative_to(p)): sha(x) for x in sorted(p.rglob('*')) if x.is_file()}
        if current != entry['files']:
            failures.append({'kind': 'classpath-tree', 'path': str(p),
                             'changed': sorted(k for k in set(current) | set(entry['files']) if current.get(k) != entry['files'].get(k))})

js = json.loads((E / 'js-artifacts.json').read_text())
for name, expected in js['files'].items():
    if sha(ROOT / 'frozen-js' / name) != expected:
        failures.append({'kind': 'js-artifact', 'path': name})

providers = []
for name, expected in source['providerCheckouts'].items():
    p = ROOT / 'staging' / name
    head = git('rev-parse', 'HEAD', cwd=p)
    status = git('status', '--porcelain=v1', '--untracked-files=all', cwd=p)
    providers.append({'path': str(p), 'head': head, 'status': status})
    if head != expected['head'] or status:
        failures.append({'kind': 'provider', 'path': str(p), 'head': head, 'status': status})

runtime = json.loads((ARCHIVE / 'runtime-identity.json').read_text())
for platform in ('java', 'node'):
    if sha(Path(runtime[platform]['path'])) != runtime[platform]['sha256']:
        failures.append({'kind': 'runtime', 'platform': platform})

key_sources = [
    'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/family/GaussianFamily.scala',
    'modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/ShapeDecoder.scala',
    'modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/CompactCondition.scala',
    'modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/profile/ConditionC0QualificationSuite.scala',
    'modules/first-level-laws/jvm/src/test/scala/scalafim/fmri/laws/profile/ConditionC0QualificationPlatform.scala',
    'modules/first-level-laws/js/src/test/scala/scalafim/fmri/laws/profile/ConditionC0QualificationPlatform.scala',
    'docs/verification/condition-c0-qualification.md', 'build.sbt', 'project/plugins.sbt', 'project/build.properties', '.jvmopts']
result = {'schema': 'c0-candidate-prefresh-readonly-closure-v1',
          'checkedUtc': datetime.datetime.now(datetime.timezone.utc).isoformat(), 'candidateHead': git('rev-parse', 'HEAD'),
          'executionSourceId': source['sourceId'], 'implementationCommit': binding['implementationCommit'],
          'sourceFilesChecked': len(source['sources']), 'classpathEntriesChecked': len(cp['classpath']),
          'jsFilesChecked': len(js['files']), 'keySources': {p: sha(WORK / p) for p in key_sources},
          'providerCheckouts': providers, 'runtime': runtime, 'packagingOnlySourceMapping': differences,
          'inputManifests': {str(p): sha(p) for p in [E / 'final-source.json', E / 'jvm-artifacts.json', E / 'js-artifacts.json', ARCHIVE / 'final-source-binding.json']},
          'failures': failures,
          'scope': 'Candidate-only closure recheck; final integrated source/provider closure and historical stream disposition remain required before fresh entry.'}
(OUT / 'candidate-closure.json').write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps({k: result[k] for k in ['candidateHead', 'executionSourceId', 'sourceFilesChecked', 'classpathEntriesChecked', 'jsFilesChecked', 'failures']}, indent=2))
if failures:
    raise SystemExit(1)
