"""Frozen paired rank-pilot custody and validation; no statistic implementation."""
import hashlib
import json
import math
from pathlib import Path
import subprocess
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from calibration_protocol import child_seed, r_state, validate_rank_provenance

NAMESPACE = 'scalafim/umvpa/rank-method-comparison/v2'
HISTORICAL = 'historical-euclidean-coefficient-permutation/ab811e257dd67f77e8c3b70cb1ea600f274429a3'
PERMUTATION = 'canonical-rank-score-orthogonal-permutation/v2'
GAUSSIAN = 'canonical-rank-gaussian-interlacing-wilks/v1'
METHODS = [HISTORICAL, PERMUTATION, GAUSSIAN]
PROVIDERS = {'historical': 'ab811e257dd67f77e8c3b70cb1ea600f274429a3',
             'current': 'edb05de01401ec0b3aea4dc1190dd3100e70ee51'}
LIMITS = dict(workers=1, cpus=4, heap='3g', rss_bytes=4*1024**3,
              cell_wall_seconds=900, campaign_wall_seconds=14400,
              required_headroom_bytes=8*1024**3, polling_seconds=.2)


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def write_once(path, data):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open('x') as out:
        json.dump(data, out, indent=2, sort_keys=True)
        out.write('\n')


def assignment(phase, scenario, ordinal):
    if phase not in ('fixture', 'pilot') or not scenario or '\0' in scenario or type(ordinal) is not int or ordinal < 0:
        raise ValueError('only explicitly assigned fixture/pilot streams are executable')
    digest = hashlib.sha256(f'{NAMESPACE}\0{phase}\0{scenario}\0{ordinal}'.encode()).digest()
    root = (int.from_bytes(digest[:8], 'big') & (2**63-1)) or 1
    noise = child_seed(root, [(101,0)])
    permutation = child_seed(root, [(103,0)])
    gaussian = child_seed(root, [(103,1)])
    return dict(phase=phase, scenario_id=scenario, dataset_index=ordinal,
                root_seed64=str(root), sha256_utf8=digest.hex(), noise_seed64=str(noise),
                r_noise_state=r_state(noise), permutation_seed64=str(permutation),
                gaussian_hypothesis_seed64=[str(child_seed(gaussian,[(1011,k)])) for k in range(4)])


def inventory(proposal):
    cells=[]
    for c in proposal['cells']:
        ident=f'rank-v2-paired-{c["population"]}-n{c["n"]}-p{c["p"]}-q{c["q"]}-{c["nuisance"]}'
        cells.append(dict(c,id=ident))
    return sorted(cells,key=lambda c:(c['n'],c['p'],c['q'],c['nuisance'],c['population']))


def validate_inventory(cells, proposal):
    if cells != inventory(proposal) or len(cells)!=64 or sum(c['datasets'] for c in cells)!=12800:
        raise ValueError('the original 64-cell/12800-dataset inventory must be retained')
    if any(c['datasets']!=200 or c['draws']!=199 for c in cells):
        raise ValueError('pilot counts changed')
    if len({c['id'] for c in cells})!=64:
        raise ValueError('duplicate cell identity')


def source_locks(root, extra=()):
    paths=['modules/mvpa','modules/scenario-testkit','build.sbt','.jvmopts',
           'project/build.properties','project/plugins.sbt','tools/build/sbt-warm',
           'tools/mvpa-inference',*extra]
    files=subprocess.check_output(['git','ls-files','--',*paths],cwd=root,text=True).splitlines()
    return {p:sha(root/p) for p in sorted(set(files)) if Path(p).suffix not in ('.gz','.bundle')}


def verify_sources(root, locks):
    for path, expected in locks.items():
        if Path(path).is_absolute() or '..' in Path(path).parts or sha(root/path)!=expected:
            raise ValueError('changed source: '+path)


def verify_runtime(locks):
    for path,expected in locks.items():
        if sha(path)!=expected:
            raise ValueError('changed classpath byte: '+path)


def validate_rows(rows, cell, provider, source_commit, source_hash, inputs, phase='pilot'):
    methods=[HISTORICAL] if provider=='historical' else [PERMUTATION,GAUSSIAN]
    expected={(i,m) for i in range(len(inputs)) for m in methods}
    actual=[(r.get('dataset_index'),r.get('rank_method')) for r in rows]
    if len(actual)!=len(set(actual)) or set(actual)!=expected:
        raise ValueError('missing, duplicate or foreign method/dataset slots')
    failures=[]
    for r in rows:
        i=r['dataset_index']
        if r['scenario_id']!=cell['id'] or r['phase']!=phase:
            raise ValueError('wrong cell or phase')
        bound=dict(source_commit=source_commit,source_lock_sha256=source_hash,case_input_sha256=inputs[i]['sha256'])
        if any(r.get(k)!=v for k,v in bound.items()) or r.get('root_seed64')!=inputs[i]['root_seed64']:
            raise ValueError('unbound input/source')
        if r.get('status')!='evaluated':
            failures.append((i,r.get('reason','unknown refusal')))
            continue
        normalized=dict(r)
        if provider=='historical':
            normalized['rank_method']=PERMUTATION  # identical seed/algorithm contract only; method identity stays historical
        validate_rank_provenance(normalized,bound)
        if r.get('completed_draws')!=199 or r.get('completed_compact_fits')!=796:
            raise ValueError('incomplete Monte Carlo arithmetic')
        counts=r.get('raw_exceedances')
        if not isinstance(counts,list) or len(counts)!=4 or any(type(x) is not int or not 0<=x<=199 for x in counts):
            raise ValueError('invalid exceedance vector')
        raw=[(x+1)/200 for x in counts]
        closed=[max(raw[:k+1]) for k in range(4)]
        for key,value in dict(raw_p_values=raw,closed_p_values=closed,p_values=closed,
                              raw_reject=[x<=.05 for x in raw],closed_reject=[x<=.05 for x in closed],
                              reject=[x<=.05 for x in closed]).items():
            if r.get(key)!=value: raise ValueError('inconsistent raw/closed accounting: '+key)
        roots=r.get('observed_roots',[])
        if len(roots)!=4 or any(not math.isfinite(x) or not 0<=x<=1 for x in roots):
            raise ValueError('invalid observed roots')
    return failures
