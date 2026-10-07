#!/usr/bin/env python3
"""Convert the algebra-only Scala exports for dependency-free base R checks."""
import csv
import gzip
import json
import subprocess
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
def read_text(name):
    path = packet / name
    return path.read_text() if path.exists() else gzip.decompress(path.with_suffix(path.suffix+'.gz').read_bytes()).decode()

records = [json.loads(line) for line in read_text('scala-replay.jsonl').splitlines()]
assert len(records) == 32
selected = [row for row in csv.DictReader(read_text('case-index.tsv').splitlines(), delimiter='\t')
            if row['rank'] == '3' and row['ordinal'] == '0']
assert len(selected) == 16
scratch = root / 'target/umvpa-rank-diagnosis-replay'
scratch.mkdir(exist_ok=True)
checks = []
for i, record in enumerate(records):
    meta = selected[i % 16]
    assert record['scenario_id'] == meta['cell'] and record['ordinal'] == 0
    assert record['purpose'] == 'exposed-input-algebra-only' and record['new_random_draws'] == 0
    n, m = record['rows'], record['residual_rows']
    assert n == int(meta['n']) and m == n - (1 if meta['nuisance'] == 'intercept' else 3)
    assert len(record['basis']) == n * m
    expected = [list(reversed(range(m))), list(range(1, m)) + [0], [1, 0] + list(range(2, m))]
    assert record['actions'] == expected
    assert len(record['null_statistics']) == 3
    for values in record['null_statistics'] + [record['roots'], record['wilks']]:
        assert len(values) == 4
    path = scratch / f'replay-{i:02d}.tsv'
    matrices = [('Q', n, m, record['basis']), ('roots', 1, 4, record['roots']),
                ('wilks', 1, 4, record['wilks']),
                ('actions', 3, m, [x + 1 for row in record['actions'] for x in row]),
                ('nulls', 3, 4, [x for row in record['null_statistics'] for x in row])]
    path.write_text(''.join(f'{name}\t{rows}\t{cols}\t' + ','.join(format(v, '.17g') for v in values) + '\n'
                            for name, rows, cols, values in matrices))
    checks.append(dict(platform='JVM' if i < 16 else 'JS', cell=meta['cell'],
                       input=meta['path'], export=str(path)))
index = scratch / 'index.tsv'
with index.open('w') as output:
    writer = csv.DictWriter(output, fieldnames=list(checks[0]), delimiter='\t', lineterminator='\n')
    writer.writeheader()
    writer.writerows(checks)
subprocess.run(['/usr/local/bin/Rscript', str(packet / 'check_replay.R'), str(index), str(packet)],
               check=True, timeout=120)
