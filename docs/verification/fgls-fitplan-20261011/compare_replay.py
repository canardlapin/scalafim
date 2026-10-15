#!/usr/bin/env python3
"""Audit all retained FitPlan replay slots against the original standalone run."""
import argparse
import gzip
import hashlib
import json
from pathlib import Path

ENGINES = {'known-continuous', 'pooled-continuous-kr'}


def records(directory, platform):
    path = directory / f'records-{platform}.jsonl'
    data = path.read_bytes() if path.exists() else gzip.decompress(path.with_suffix('.jsonl.gz').read_bytes())
    rows = {}
    for line in data.decode().splitlines():
        if line.startswith('FGLS_STUDY_FAILURE '):
            raise ValueError('retained fit refusal: ' + line)
        if not line.startswith('FGLS_STUDY_TRIAL '):
            continue
        row = json.loads(line.removeprefix('FGLS_STUDY_TRIAL '))
        if row['engine'] not in ENGINES:
            continue
        key = (row['cell'], row['engine'], row['replicate'])
        if key in rows or row['profile'] != 'confirmation':
            raise ValueError('duplicate slot or wrong profile')
        rows[key] = row
    return rows, hashlib.sha256(data).hexdigest()


def compare(left, right, expected):
    if set(left) != expected or set(right) != expected:
        raise ValueError('record slots differ from the complete declared population')
    mismatches = []
    maximum = {}
    def value(a, b, field, key):
        if isinstance(a, bool) or isinstance(b, bool):
            if a is not b:
                mismatches.append((key, field))
        elif isinstance(a, (float, int)) and isinstance(b, (float, int)):
            difference = abs(a - b) / (1 + abs(a))
            maximum[field] = max(maximum.get(field, 0.0), difference)
            if not difference <= 1e-9:
                mismatches.append((key, field))
        elif isinstance(a, list) and isinstance(b, list) and len(a) == len(b):
            for i, (x, y) in enumerate(zip(a, b)):
                value(x, y, f'{field}[{i}]', key)
        elif a != b:
            mismatches.append((key, field))
    for key in sorted(expected):
        a, b = left[key], right[key]
        if a.keys() != b.keys():
            mismatches.append((key, 'fields'))
            continue
        for field in a:
            value(a[field], b[field], field, key)
    return dict(records=len(expected), scaled_tolerance=1e-9,
                max_scaled_difference=maximum, mismatches=len(mismatches),
                first_mismatches=mismatches[:20], passed=not mismatches)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--original', type=Path, required=True)
    parser.add_argument('--replay', type=Path, required=True)
    parser.add_argument('--protocol', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    protocol = json.loads(args.protocol.read_text())
    expected = {(cell['id'], engine, replicate) for cell in protocol['cells']
                for engine in ENGINES for replicate in range(protocol['profiles']['confirmation']['replicates'])}
    report = dict(kind='FitPlan integration replay; previously observed streams',
                  protocol_sha256=hashlib.sha256(args.protocol.read_bytes()).hexdigest(), comparisons={}, source_records={})
    retained = {}
    for platform in ('jvm', 'js'):
        old, old_hash = records(args.original, platform)
        new, new_hash = records(args.replay, platform)
        retained[platform] = new
        report['comparisons'][platform + '_against_original'] = compare(old, new, expected)
        report['source_records'][platform] = dict(original_sha256=old_hash, replay_sha256=new_hash)
        original_summary = json.loads((args.original / f'analysis-{platform}/summary.json').read_text())
        replay_summary = json.loads((args.replay / f'analysis-{platform}/summary.json').read_text())
        criteria = lambda summary: {(r['cell'], r['engine']): r['criteria'] for r in summary['results'] if r['engine'] in ENGINES}
        report['comparisons'][platform + '_criteria'] = dict(passed=criteria(original_summary) == criteria(replay_summary))
    report['comparisons']['cross_platform'] = compare(retained['jvm'], retained['js'], expected)
    report['passed'] = all(c['passed'] for c in report['comparisons'].values())
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({key: value['passed'] for key, value in report['comparisons'].items()}))
    raise SystemExit(0 if report['passed'] else 1)


if __name__ == '__main__':
    main()
