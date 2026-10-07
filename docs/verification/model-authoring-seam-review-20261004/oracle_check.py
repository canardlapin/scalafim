import json, re, sys
from pathlib import Path
import numpy as np
import scipy

root = Path(sys.argv[1]) if len(sys.argv) > 1 else Path('/private/tmp/scalafim-seam-review-20261004')
script = root / 'tools/fixtures/generate_spm_informed_basis.py'
namespace = {}
# Load oracle functions without the generator's file-writing block.
exec(compile(script.read_text().split('cases = ')[0], str(script), 'exec'), namespace)
source = (root / 'modules/hrf/shared/src/test/scala/scalafim/fmri/hrf/fixtures/SpmInformedBasisFixture.scala').read_text()
results = []
for name, tr, columns in [('tr2', 2.0, 3), ('tr072', 0.72, 2)]:
    match = re.search(r'val ' + name + r': Array\[Double\] = Array\((.*?)\n  \)', source, re.S)
    expected = np.array([float(x.strip()) for x in match.group(1).split(',')])
    actual = namespace['spm_get_bf'](tr, columns).ravel()
    maximum = float(np.max(np.abs(actual - expected)))
    assert actual.shape == expected.shape
    assert maximum <= 1e-15, (name, maximum)
    results.append({'case': name, 'tr': tr, 'columns': columns, 'entries': len(actual), 'max_absolute_difference': maximum})
print(json.dumps({'numpy': np.__version__, 'scipy': scipy.__version__, 'claim': 'Frozen NumPy SPM reimplementation reproduces stored oracle; no MATLAB execution', 'results': results}, indent=2))
