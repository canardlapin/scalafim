#!/usr/bin/env python3
"""Freeze eight resource fixtures without drawing data or opening pilot streams."""
import hashlib
import importlib.util
import json
import subprocess
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
spec = importlib.util.spec_from_file_location('protocol',root/'tools/mvpa-inference/calibration_protocol.py')
protocol = importlib.util.module_from_spec(spec)
spec.loader.exec_module(protocol)
namespace = 'scalafim/umvpa/rank-method-comparison/v2'
cells = []
for n in (80,640):
    for p,q in ((4,6),(6,4)):
        for nuisance in ('intercept','three-column'):
            name = f'rank-v2-resource-n{n}-p{p}-q{q}-{nuisance}'
            digest = hashlib.sha256((namespace+'\0fixture\0'+name+'\0'+'0').encode()).digest()
            seed = int.from_bytes(digest[:8],'big') & ((1<<63)-1) or 1
            noise = protocol.child_seed(seed,[(102,0)])
            cells.append(dict(id=name,n=n,p=p,q=q,nuisance=nuisance,rho=[.5,.3,.2,0.],
                phase='fixture',ordinal=0,draws=199,root_seed64=str(seed),sha256_utf8=digest.hex(),
                noise_seed64=str(noise),r_noise_state=protocol.r_state(noise)))
paths = subprocess.check_output(['git','ls-files','modules/mvpa','modules/scenario-testkit','tools/build/sbt-warm',
    'tools/mvpa-inference/calibration_protocol.py','tools/mvpa-inference/rank_population.R',
    'tools/mvpa-inference/run_rank_pilot.py','tools/mvpa-inference/run_calibration.py','build.sbt'],cwd=root,text=True).splitlines()
paths += [str(p.relative_to(root)) for p in packet.glob('*.py')]
paths += [str(p.relative_to(root)) for p in packet.glob('*.R')]
paths += ['modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/inference/RankV2ResourceProbeSuite.scala',
          'modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/inference/RankScoreOracleFixtures.scala']
provider = Path('/private/tmp/multivar-umvpa-rank-v2-20261008')
provider_head = subprocess.check_output(['git','rev-parse','HEAD'],cwd=provider,text=True).strip()
assert not subprocess.check_output(['git','status','--porcelain'],cwd=provider,text=True).strip()
manifest = dict(status='source-frozen-resource-fixtures-only',namespace=namespace,
    provider_revision=provider_head,method_identities=['canonical-rank-score-orthogonal-permutation/v2',
    'canonical-rank-gaussian-interlacing-wilks/v1'],unique_datasets=8,expected_records=16,cells=cells,
    source_locks={p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in sorted(set(paths))},
    limits=dict(worker_processes=1,cpus=4,heap='3g',max_wall_seconds=900,max_rss_bytes=4*1024**3),
    source_scope='Full current public adapter for the two new methods; no historical-method timing or rate qualification.',
    confirmation_consumed=False)
with (packet/'probe-manifest.json').open('x') as out:
    out.write(json.dumps(manifest,indent=2)+'\n')
print(json.dumps({'cases':8,'records':16,'provider':provider_head,'source_files':len(manifest['source_locks'])}))
