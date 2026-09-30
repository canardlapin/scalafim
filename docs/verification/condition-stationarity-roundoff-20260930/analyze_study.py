"""Audit retained C0 rows; extended from c0-analyze-study.py in the prior execution evidence."""
import collections,json,math,pathlib,sys
p=pathlib.Path(sys.argv[1]);rs=[json.loads(l) for l in p.read_text().splitlines() if l.startswith('{"kind":')]
cells=[r for r in rs if r['kind']=='cell'];assert cells
assert len({r['sourceId'] for r in rs}) == 1
out=[]
for c in cells:
 vs=[r for r in rs if r['kind']=='voxel' and (r['platform'],r['policy'],r['snr'],r['seed'])==(c['platform'],c['policy'],c['snr'],c['seed'])]
 assert len(vs)==c['voxels'];assert [v['terminal']['voxel'] for v in vs]==list(range(c['voxels']))
 assert dict(collections.Counter(v['terminal']['status'] for v in vs))==c['statuses']
 admitted=[v for v in vs if v['terminal']['status']=='Accepted'];assert len(admitted)==c['admitted']
 cap=13 if '12j' in c['policy'] else 9
 for v in vs:
  t=v['terminal'];w=t['work'];assert w['candidates']<=cap
  assert all(value >= 0 for value in w.values())
  assert w['nodes'] <= 225 and w['jets'] <= (12 if cap == 13 else 8) and w['exact'] <= 2
  assert w['steps'] <= 6 and w['terminal'] <= w['jets'] and w['fallbacks'] <= 1
  assert len(v['reference']['coordinates'])==2 and len(v['reference']['amplitudes'])==3
  if t['auditAvailable']:
   e=t['audit'];h=e['hessian'];g=e['gradient'];x=t['coordinates']
   if e['conditionalSd'] is not None:
    det=h[0]*h[3]-h[1]*h[2];assert h[0]>0 and det>0
    sd=[math.sqrt(2*h[3]/det),math.sqrt(2*h[0]/det)]
    for a,b in zip(e['conditionalSd'],sd):assert math.isclose(a,b,rel_tol=1e-9,abs_tol=1e-9)
   if t['status']=='Accepted':
    assert t['coherent'] and e['conditionalSd'] is not None
    assert all(math.isfinite(a) and 0 < a <= b for a,b in zip(e['conditionalSd'], [.5, 1.]))
    for a,b in [(t['fitEnergy'],e['jetEnergy'])] + list(zip(t['fitHessian'],h)) + list(zip(t['fitAmplitudes'],e['amplitudes'])) + list(zip(t['fitSd'],e['conditionalSd'])):
     assert math.isclose(a,b,rel_tol=1e-9,abs_tol=1e-9)
    free=[i for i,(lower,upper) in enumerate([(3.,8.),(math.log(.8),math.log(3.))]) if not ((x[i]<=lower+1e-12 and g[i]>0) or (x[i]>=upper-1e-12 and g[i]<0))]
    if len(free)==2:
     det=h[0]*h[3]-h[1]*h[2];correction=max(abs((-h[3]*g[0]+h[1]*g[1])/det),abs((h[2]*g[0]-h[0]*g[1])/det))
    elif len(free)==1:correction=abs(g[free[0]]/h[free[0]*2+free[0]])
    else:correction=0.
    assert math.isclose(correction,e['projectedNewtonCorrection'],rel_tol=1e-9,abs_tol=1e-12)
    assert correction<=1e-9
  else:assert t['audit'] is None and t['auditFailure']
 def p95(field):
  xs=sorted(v['accuracy'][field] for v in admitted)
  return xs[max(0,math.ceil(.95*len(xs))-1)] if xs else None
 metrics=[p95('latency'),p95('width'),p95('relativeAmplitude')]
 assert metrics==[c['latencyP95'],c['widthP95'],c['amplitudeP95']]
 gate = (cap == 13 and c['snr'] in [1., .5] and c['voxels'] == 200 and len(admitted) >= 190
         and all(a is not None and math.isfinite(a) and a <= b for a,b in zip(metrics, [.02, .05, 1e-3]))
         and c['oracleUnresolved'] == 0 and all(not v['reference']['failures'] for v in vs))
 assert gate == c['candidateGate']
 out.append({k:c[k] for k in ['platform','policy','snr','seed','voxels','admitted','latencyP95','widthP95','amplitudeP95','auditFailures','oracleUnresolved','candidateGate','work','reference']})
for r in rs:
 if r['kind']=='paired':
  assert len(r['transitions'])==cells[0]['voxels']
  new={t['voxel'] for t in r['transitions'] if t['baseline']!='Accepted' and t['candidate']=='Accepted'}
  assert new==set(r['newlyAdmitted'])=={e['voxel'] for e in r['newlyAdmittedErrors']}
print(json.dumps({'log':str(p),'records':len(rs),'cells':out},indent=2))
