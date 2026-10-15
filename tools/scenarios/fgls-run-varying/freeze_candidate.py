#!/usr/bin/env python3
"""Freeze the predeclared choice from one retained development run, never from confirmation."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[3]


def validate_records(path, protocol, profile, engines):
    expected = {(cell['id'], engine, replicate)
                for cell in protocol['cells'] for engine in engines
                for replicate in range(protocol['profiles'][profile]['replicates'])}
    seen = set()
    for line in path.read_text().splitlines():
        if line.startswith('FGLS_STUDY_FAILURE '):
            raise ValueError('a retained fit refusal prevents admission')
        if not line.startswith('FGLS_STUDY_TRIAL '):
            continue
        row = json.loads(line.removeprefix('FGLS_STUDY_TRIAL '))
        key = (row['cell'], row['engine'], row['replicate'])
        if row['profile'] != profile or key not in expected or key in seen:
            raise ValueError('unexpected, duplicated, or wrong-profile trial')
        seen.add(key)
    if seen != expected:
        raise ValueError('missing declared trial slots')
    return len(seen)


def choose(protocol, summary):
    if summary.get('profile') != 'development' or summary.get('failures') != 0:
        raise ValueError('selection requires complete, failure-free development evidence')
    cells = {cell['id'] for cell in protocol['cells'] if cell['role'] == 'admission'}
    eligible = []
    scores = {}
    for candidate in protocol['candidate_engines']:
        rows = [row for row in summary['results'] if row['engine'] == candidate]
        if len(rows) != len(cells) or {row['cell'] for row in rows} != cells:
            continue
        if not all(row['criteria']['complete'] and row['criteria']['no_refusals'] and
                   row['n'] == protocol['profiles']['development']['replicates'] for row in rows):
            continue
        scores[candidate] = sum(sum(row['criteria'].values()) for row in rows)
        eligible.append(candidate)
    if not eligible:
        raise ValueError('no eligible candidate; confirmation remains unconsumed')
    # Python max retains declaration order for ties: 0.75 before 0.50.
    return max(eligible, key=lambda candidate: scores[candidate]), scores


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--development', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    protocol_path = ROOT / 'tools/scenarios/fgls-run-varying/protocol.json'
    protocol_bytes = protocol_path.read_bytes()
    protocol = json.loads(protocol_bytes)
    receipt_path = args.development / 'run.json'
    summary_path = args.development / 'analysis-jvm/summary.json'
    receipt = json.loads(receipt_path.read_text())
    summary = json.loads(summary_path.read_text())
    if receipt.get('profile') != 'development' or not receipt.get('source_unchanged') or receipt.get('source_dirty'):
        raise ValueError('development must execute a committed, unchanged source snapshot')
    if receipt.get('protocol_sha256') != hashlib.sha256(protocol_bytes).hexdigest():
        raise ValueError('development used a different protocol')
    if not receipt.get('targets') or any(t['exit_code'] for t in receipt['targets'] + receipt.get('analyses', [])):
        raise ValueError('development execution or analysis failed')
    if not receipt.get('source_sha256') or any(hashlib.sha256((ROOT / name).read_bytes()).hexdigest() != digest
                                             for name, digest in receipt['source_sha256'].items()):
        raise ValueError('source changed after development; cannot freeze these outcomes')
    validate_records(args.development / 'records-jvm.jsonl', protocol, 'development', protocol['engines'])
    candidate, scores = choose(protocol, summary)
    subprocess.run(['git', 'merge-base', '--is-ancestor', receipt['source_base'], 'HEAD'], cwd=ROOT, check=True)
    frozen = dict(schema_version='scalafim-fgls-run-varying-freeze/v1', candidate=candidate,
                  source_commit=receipt['source_base'], source_sha256=receipt['source_sha256'],
                  protocol_sha256=receipt['protocol_sha256'],
                  development_run_sha256=hashlib.sha256(receipt_path.read_bytes()).hexdigest(),
                  development_summary_sha256=hashlib.sha256(summary_path.read_bytes()).hexdigest(),
                  development_records_sha256=hashlib.sha256((args.development / 'records-jvm.jsonl').read_bytes()).hexdigest(),
                  scores=scores, rule=protocol['candidate_selection'])
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open('x') as stream:
        json.dump(frozen, stream, indent=2)
        stream.write('\n')
    print(candidate)


if __name__ == '__main__':
    main()
