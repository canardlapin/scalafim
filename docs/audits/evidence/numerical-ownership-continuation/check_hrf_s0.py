import hashlib,itertools,json,math,statistics,sys
from pathlib import Path
p=Path(sys.argv[1])
r=json.loads((p/'receipt.json').read_text()); data=json.loads((p/'jmh.json').read_text())
errors=[]
def check(ok,msg):
 if not ok: errors.append(msg)
def key(name,params): return name,tuple(sorted(params.items()))
expected=set();prefix='scalafim.fmri.hrf.'
for method in ['conv','fft','loop']:
 for n,b,q in itertools.product(['300','1200','4800'],['1','3'],['0.33','0.1']): expected.add(key(prefix+'RegressorConvolutionBenchmark.'+method,dict(nScans=n,nbasis=b,precision=q)))
for method in ['conv','fft']:
 for d,n in itertools.product(['0.0','0.25','0.9'],['1200','4800']): expected.add(key(prefix+'DenseDriveBenchmark.'+method,dict(dutyCycle=d,nScans=n)))
for method in ['pointFunctional','exactWindowFunctional','trapezoidWindowFunctional','typedReconstruction','directContraction']: expected.add(key(prefix+'BasisResponseBenchmark.'+method,{}))
actual=[key(x['benchmark'],x.get('params',{})) for x in data]
check(len(actual)==len(set(actual))==53,'duplicate/missing configurations');check(set(actual)==expected,'coverage mismatch')
rows=[]
for x in data:
 name=str(key(x['benchmark'],x.get('params',{})))
 for k,v in {'forks':5,'threads':1,'warmupIterations':3,'measurementIterations':5,'warmupTime':'1 s','measurementTime':'1 s','jdkVersion':'21.0.12.1','jmhVersion':'1.37','mode':'avgt'}.items(): check(x.get(k)==v,name+' '+k)
 check(x['jvmArgs']==['-Xms1g','-Xmx1g','-XX:ActiveProcessorCount=2'],name+' flags')
 for label,metric in [('time',x['primaryMetric']),('allocation',x['secondaryMetrics']['gc.alloc.rate.norm'])]:
  raw=metric['rawData'];check(len(raw)==5 and all(len(f)==5 for f in raw),name+' '+label+' fork coverage')
  check(all(isinstance(v,(float,int)) and math.isfinite(v) and v>=0 for f in raw for v in f),name+' '+label+' finite')
  check(math.isfinite(metric['score']) and math.isfinite(metric['scoreError']),name+' '+label+' summary finite')
 check(x['primaryMetric']['scoreUnit']=='us/op',name+' time unit');check(x['secondaryMetrics']['gc.alloc.rate.norm']['scoreUnit']=='B/op',name+' allocation unit')
 med=[statistics.median(f) for f in x['primaryMetric']['rawData']]
 rows.append({'benchmark':x['benchmark'],'params':x.get('params',{}),'fork_medians_us':med,'fork_median_cv':statistics.stdev(med)/statistics.mean(med),'allocation_B_per_op':x['secondaryMetrics']['gc.alloc.rate.norm']['score']})
check(r['status']=='outputs_require_review' and not r['reasons'] and r['exit_code']==0,'runner exit/status')
check(r['manifest_sha256']=='74b9889d2253b4a5e91ff793dae53bc65fdc35c79b70749796c68db07c6b0231','manifest')
check(r['max_start_end_load']==5,'authorized threshold')
previous=r['host_start']; start=previous; loads=[];gaps=[];maxcpu=0.;samples=0
with (p/'host.jsonl').open() as f:
 for line in f:
  s=json.loads(line);samples+=1
  delta=s['captured_monotonic']-previous['captured_monotonic'];gaps.append(delta);loads.append(s['load'][0]);check(delta>0,'nonpositive sampling interval')
  check(not s['competing'] and not s['throttled'],'host sample refusal');check(s['thermal']==start['thermal'] and s['power']==start['power'],'thermal/power change')
  old={(v['pid'],v['started']):v for v in previous['processes']['java_node']}
  for v in s['processes']['java_node']:
   if v['process_group']==r['owned_pid_and_process_group']:continue
   before=old.get((v['pid'],v['started']));check(before is not None,'new unowned Java/Node')
   if before and delta>0:
    cpu=100*(v['cpu_seconds']-before['cpu_seconds'])/delta;maxcpu=max(maxcpu,cpu);check(cpu<=10,'competing CPU >10%')
  previous=s
for tag in ['host_start','host_end']:
 s=r[tag];check(s['load'][0]<=5 and not s['competing'] and not s['throttled'],tag+' validity');check(s['power']==start['power'] and s['thermal']==start['thermal'],tag+' power/thermal')
check(samples>0,'no host samples')
result={'schema':'scalafim.hrf-s0-review.v1','valid_s0_baseline':not errors,'s4_admitted':False,'errors':errors,'configurations':len(data),'forks_per_configuration':5,'measured_iterations_per_fork':5,'host_samples':samples,'start_load':start['load'][0],'end_load':r['host_end']['load'][0],'during_run_load_range':[min(loads),max(loads)],'maximum_sample_interval_seconds':max(gaps),'maximum_unowned_java_node_cpu_percent':maxcpu,'source_revision':'61df8a4620c9cc0dd33af6b7cabee04b4492e936','raw_sha256':{n:hashlib.sha256((p/n).read_bytes()).hexdigest() for n in ['receipt.json','host.jsonl','jmh.json','jmh.log','supervisor-exit.json','remote-results.tar.gz']},'configuration_summaries':rows,'interpretation':'S0 variance characterization only. No candidate ratio, non-inferiority verdict or frozen S4 sample size.'}
(p/'review.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps({k:v for k,v in result.items() if k not in ['configuration_summaries','raw_sha256']},indent=2));print('fork median CV range',min(x['fork_median_cv'] for x in rows),max(x['fork_median_cv'] for x in rows))
if errors:raise SystemExit(1)
