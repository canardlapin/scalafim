"""Analytic matrix and missing-curvature rejection controls, separate from cohort evidence."""
import copy
import importlib.util
import json
from pathlib import Path
import tempfile
import sys

root = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('audit', root / 'analyze_lwu_records.py')
audit = importlib.util.module_from_spec(spec)
spec.loader.exec_module(audit)
D = audit.D
h = [[D(v) for v in row] for row in [[4,1,1],[1,3,0],[1,0,2]]]
i = audit.inverse_spd(h)
assert i is not None
assert max(abs(sum(h[r][k]*i[k][c] for k in range(3))-D(int(r==c))) for r in range(3) for c in range(3)) < D('1e-58')
assert audit.inverse_spd([[D(1),D(2)],[D(2),D(1)]]) is None
raw = Path(sys.argv[1]).read_text()
rows = [json.loads(line.removeprefix('[info] ').strip()) for line in raw.splitlines() if line.removeprefix('[info] ').strip().startswith('{"kind":"lwu-diagnostic"')]
source = rows[0]['sourceId']
index = next(n for n,r in enumerate(rows) if r['status']=='Boundary')
modified = copy.deepcopy(rows)
modified[index]['dataHessian'] = [None]*9
modified[index]['augmentedHessian'] = [None]*9
modified[index]['conditionalSd'] = [None]*3
with tempfile.TemporaryDirectory(prefix='lwu-analyzer-control-') as temporary:
    p = Path(temporary)/'synthetic-records.jsonl'
    def write(): p.write_text(''.join(json.dumps(r,separators=(',',':'))+'\n' for r in modified))
    write()
    result = audit.analyze(p,source)
    assert result['cells'][0]['postfitTerminalChecks']['returnedCurvatureUnavailable']==1
    modified[index]['augmentedHessian'][0] = rows[index]['augmentedHessian'][0]
    write()
    try: audit.analyze(p,source)
    except AssertionError: pass
    else: raise AssertionError('mixed missing curvature escaped audit')
print(json.dumps({'scope':'Analytic and synthetic missing-value controls only; no cohort evidence replacement', 'coupledSpdInverse':True,'indefiniteRejected':True,'fullyUnavailablePreserved':True,'mixedUnavailableRejected':True}))
