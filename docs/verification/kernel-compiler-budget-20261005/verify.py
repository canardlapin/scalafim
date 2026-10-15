#!/usr/bin/env python3
"""Run focused compiler gates and bounded mutants under the shared build slot.

python3 docs/verification/kernel-compiler-budget-20261005/verify.py

Each mutant uses an inherited selector that requires the exact test set. Only
metadata admission or tiny synthetic arrays run; oversized compiler requests
never bypass admission under mutation. Sources/selectors restore in finally.
"""
from pathlib import Path
import argparse
import datetime
import hashlib
import json
import re
import subprocess
import tarfile
import time
import sys

ROOT = Path(__file__).resolve().parents[3]
OUT = Path(__file__).resolve().parent
POLICY = ROOT / 'modules/design/shared/src/main/scala/scalafim/fmri/design/hrf/KernelBasisBudget.scala'
COMPILER = ROOT / 'modules/design/shared/src/main/scala/scalafim/fmri/design/hrf/KernelBasis.scala'
TEST = ROOT / 'modules/design/shared/src/test/scala/scalafim/fmri/design/hrf/KernelBasisBudgetSuite.scala'
PARITY = ROOT / 'modules/design/shared/src/test/scala/scalafim/fmri/design/HrfKernelBasisSuite.scala'
SELECTOR = TEST.with_name('KernelBasisMutationSuite.scala')
SUITE = 'scalafim.fmri.design.hrf.KernelBasisBudgetSuite'
parser = argparse.ArgumentParser()
parser.add_argument('--budget-only', action='store_true', help='Use focused budget gates; parent runs final exact-source scientific integration.')
args = parser.parse_args()
prior_file = OUT / 'prior-scientific-gate.json'
prior_receipt = OUT / 'receipt.json'
prior_scientific = json.loads(prior_file.read_text()) if prior_file.exists() else (
    json.loads(prior_receipt.read_text()).get('prior_scientific_gate') if prior_receipt.exists() else None)
records = []

def run(label, commands, expected):
    command = ['python3', 'tools/build/sbt-warm', *commands]
    start = time.monotonic()
    result = subprocess.run(command, cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    (OUT / (label + '.log')).write_text(result.stdout)
    counts = [dict(zip(('total', 'failed', 'errors', 'passed'), map(int, match))) for match in
              re.findall(r'Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)', result.stdout)]
    warnings = [line for line in result.stdout.splitlines() if '[warn]' in line]
    record = dict(label=label, command=command, exit_code=result.returncode, expected=expected,
                  seconds=round(time.monotonic() - start, 3), counts=counts, warnings=warnings)
    records.append(record)
    print(json.dumps(record), flush=True)
    (OUT / 'runs.json').write_text(json.dumps(records, indent=2) + '\n')
    if expected == 'pass' and result.returncode != 0:
        raise RuntimeError('gate failed: ' + label)
    if expected == 'test-failure' and (result.returncode == 0 or not any(c['failed'] > 0 and c['total'] > 0 for c in counts) or any(c['errors'] > 0 for c in counts)):
        raise RuntimeError('mutant not detected by a test failure: ' + label)

gates = SUITE if args.budget_only else SUITE + ' scalafim.fmri.design.HrfKernelBasisSuite scalafim.fmri.design.KernelBasisDesignSuite'
run('gates-before', [f'{p}/testOnly {gates}' for p in ('designJVM', 'designJS')], 'pass')
mutants = [
    ('limit-cap', POLICY, [('count > maximum.toDouble', 'false')], ['portable-budget-exact-boundaries']),
    ('requested-rank-as-SVD-width', POLICY,
     [('val k = math.min(n, columns)', 'val k = math.min(spec.maxRank.toDouble, math.min(n, columns))')],
     ['full-SVD-shapes-are-independent-of-requested-rank']),
    ('value-only-certification-undercount', POLICY,
     [('jets * available * n + jets * available', 'components * available * n + components * available')],
     ['value-only-training-still-charges-full-certificate-jets']),
    ('unchecked-chart-nodes', POLICY, [('_ <- validateChart(spec)', '_ <- if spec.nodesPerAxis.isEmpty then validateChart(spec) else Right(())')],
     ['finite-chart-width-with-overflowing-node-intermediate-is-refused']),
    ('nonfinite-certificate-derived-values', COMPILER,
     [('if !projected.isFinite then', 'if false then'), ('if !residual.isFinite then', 'if false then')],
     ['certificate-refuses-finite-projection-whose-square-overflows']),
    ('nonfinite-training-norm', COMPILER, [('if !norm.isFinite then return Left(KernelBasisError.Evaluation(s"non-finite training norm',
      'if false then return Left(KernelBasisError.Evaluation(s"non-finite training norm')],
     ['training-refuses-finite-values-with-overflowing-column-norm'])
]
for label, path, replacements, tests in mutants:
    original = path.read_text()
    mutated = original
    for old, new in replacements:
        if mutated.count(old) != 1:
            raise RuntimeError('mutation anchor must be unique: ' + label)
        mutated = mutated.replace(old, new)
    if SELECTOR.exists():
        raise RuntimeError('mutation selector already exists')
    try:
        path.write_text(mutated)
        (OUT / (label + '-source.scala')).write_text(mutated)
        names = ', '.join(json.dumps(name) for name in tests)
        selector = f'''package scalafim.fmri.design.hrf

class KernelBasisMutationSuite extends KernelBasisBudgetSuite:
  override def munitTests(): Seq[munit.Test] =
    val names = Set({names})
    val selected = super.munitTests().filter(t => names.contains(t.name))
    require(selected.size == names.size && selected.map(_.name).toSet == names)
    selected
'''
        SELECTOR.write_text(selector)
        (OUT / (label + '-selector.scala')).write_text(selector)
        for platform in ('designJVM', 'designJS'):
            run(label + '-' + platform,
                [f'{platform}/testOnly scalafim.fmri.design.hrf.KernelBasisMutationSuite'], 'test-failure')
    finally:
        path.write_text(original)
        SELECTOR.unlink(missing_ok=True)
run('gates-restored', [f'{p}/testOnly {SUITE}' for p in ('designJVM', 'designJS')], 'pass')
paths = [POLICY, COMPILER, TEST, PARITY]
archive = OUT / 'logs-and-sources.tar.gz'
with tarfile.open(archive, 'w:gz') as tar:
    for path in sorted(OUT.glob('*.log')) + sorted(OUT.glob('*.scala')) + [Path(__file__).resolve(), OUT / 'runs.json'] + ([prior_file] if prior_file.exists() else []):
        tar.add(path, arcname=path.name)
    if (OUT / 'development').exists():
        tar.add(OUT / 'development', arcname='development')
    for path in paths:
        tar.add(path, arcname=str(path.relative_to(ROOT)))
receipt = dict(verified_at_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
    mote='bd-01M47G78N9NB5BF8YX8HW9ZEKX', verification_command=['python3', str(Path(__file__).resolve().relative_to(ROOT)), *sys.argv[1:]], runs=records,
    prior_scientific_gate=prior_scientific,
    mutation_safety='Metadata-only estimates/admission or tiny synthetic arrays under exact inherited selectors; no oversized compile allocations run under mutants.',
    source_sha256={str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in paths},
    archive=archive.name, archive_sha256=hashlib.sha256(archive.read_bytes()).hexdigest(),
    gale_revision='da38f8c429294657d30ec29f06eae3fab636d428', backend='Lexically resolved SpectralBackend.none; pure full/economy SVD',
    scope_limits=['Primitive-cell storage envelope excludes object headers and garbage-collection behavior.',
                  'Spectral work units count paired-rotation/scalar accumulation iterations, not primitive reads/writes or elapsed time.',
                  'Custom family callback and overridden tail diagnostic internal allocations/work cannot be inferred.',
                  'Finite owned chart nodes preserve original IEEE arithmetic, including rounded endpoint overshoots.',
                  'Scientific kernel provenance encoding is unchanged; resource estimates are inspectable metadata.'])
(OUT / 'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
for path in list(OUT.glob('*.log')) + list(OUT.glob('*.scala')) + [OUT / 'runs.json']:
    path.unlink()

import shutil
shutil.rmtree(OUT / "development", ignore_errors=True)
(OUT / "prior-scientific-gate.json").unlink(missing_ok=True)
