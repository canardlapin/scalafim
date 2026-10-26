from pathlib import Path
import json,re,subprocess,hashlib,sys,importlib.util,collections
base=Path('/Volumes/My Passport for Mac/codex-work/scalafim-eco-u6-20261022');root=base/'acceptance-clone-25';cache=base/'acceptance-cache-25'
source='92154d81c39b6a384be195dde0e933ed0dff29ca';old='31a42a69004c6282e01ad939c370f249fdf5e604';middle='616860a2e14eb42b800462cd3d2f4fe4203f641a';final='--final' in sys.argv
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()==source
assert not subprocess.check_output(['git','status','--porcelain'],cwd=root,text=True)
def digest(p):return hashlib.sha256(Path(p).read_bytes()).hexdigest()
def gittext(revision,path):return subprocess.check_output(['git','show',revision+':'+path],cwd=root,text=True)
spec=importlib.util.spec_from_file_location('inventory',root/'tools/ci/check_test_inventory.py');inventory=importlib.util.module_from_spec(spec);spec.loader.exec_module(inventory)
build_text=(root/'build.sbt').read_text();runner=(root/'tools/ci/full-repository-tests.sh').read_text();commands=inventory.release_batches(runner);expected=list(commands);assert len(expected)==37
assert build_text==gittext(middle,'build.sbt') and runner==gittext(middle,'tools/ci/full-repository-tests.sh')
old_build=gittext(old,'build.sbt');assert old_build.replace('mvpa4sImagingTestAll','scalafimMvpaImagingTestAll').replace('addCommandAlias("scalafimMvpaImagingTestAll",','// Source builds share sbt command aliases; keep this distinct from the provider alias.\naddCommandAlias("scalafimMvpaImagingTestAll",')==build_text
assert gittext(old,'tools/ci/full-repository-tests.sh').replace('mvpa4sImagingTestAll','scalafimMvpaImagingTestAll')==runner
motion='modules/motion/jvm/src/main/scala/scalafim/fmri/motion/io/MotionBids.scala';motiontest='modules/motion/jvm/src/test/scala/scalafim/fmri/motion/io/MotionIoSuite.scala'
assert gittext(middle,motion).replace('String = ".*"','String = "*"').replace('object MotionBids:','/** Scan selectors use bids4s entity globs; `*` leaves an entity unrestricted. */\nobject MotionBids:')==gittext(source,motion)
changed=subprocess.check_output(['git','diff','--name-only',middle,source],cwd=root,text=True).splitlines();assert changed==[motion,motiontest],changed
standalone='tools/neuroim-composition-consumer/src/main/scala/consumer/NeuroimCompositionConsumer.scala';assert gittext('f0b67d349ec6f444969749845b15d3e58d8996bb',standalone)==gittext(source,standalone)
def expand(target):
 if target=='ecosystemBoundaries' or target.endswith('/test'):return [target]
 block=build_text.split('addCommandAlias("'+target+'",',1)[1].split('addCommandAlias(',1)[0];provider=re.search(r'\$\{(\w+)Build\}',block)[1];seq=re.search(r'Seq\(([^)]*)\)',block)
 projects=re.findall(r'"(\w+)"',seq[1]) if seq else re.findall(r'\}\}(\w+)/test',block);assert projects,target
 return [provider+':'+p+'/test' for p in projects]
expanded={n:[p for t in ts for p in expand(t)] for n,ts in commands.items()};assert sum(map(len,expanded.values()))==144
empty={'mvpa4s:consumerJVM/test':'Pinned pure consumer has main source only; compilation evidence, separate public behavior receipt40.','mvpa4s:consumerJS/test':'Same pure consumer, JS compilation/link evidence only.','neurosurf4s:spatialIoJVM/test':'Pinned spatial IO capability has no own test sources; compiled prerequisite.'}
phases=[('full-34',old,0,17,True),('resume-51',middle,17,8,True),('resume-56',source,25,12,False)];batches=[];segments=[];bodies={}
for name,revision,offset,n,interrupted in phases:
 log=Path('/tmp/scalafim-eco-u6-'+name+'.log')
 text=log.read_text() if log.exists() else '';starts=list(re.finditer(r'^\[full-repository-tests\] running (.+)$',text,re.M));names=[m[1] for m in starts];assert names==expected[offset:offset+len(names)],(name,names)
 if interrupted:
  meta=json.loads(Path(str(log)+'.meta.json').read_text());assert meta['exit_code']==1 and meta['reason']=='exited' and meta['log_bytes']==log.stat().st_size;assert len(starts)==n+1
  count=n
 else:
  if final:
   meta=json.loads(Path(str(log)+'.meta.json').read_text());assert meta['exit_code']==0 and meta['reason']=='exited' and meta['log_bytes']==log.stat().st_size
   assert len(starts)==n and 'RESUME COMPLETE:12/12batches' in text;count=n
  else:meta=None;count=max(0,len(starts)-1)
 if meta is not None:
  assert meta['cwd']==str(root) and meta['log']==str(log)
  if name=='full-34':assert meta['command']==['bash','tools/ci/full-repository-tests.sh']
  else:assert len(meta['command'])==2 and Path(meta['command'][0]).is_absolute() and Path(meta['command'][0]).name.lower().startswith('python') and meta['command'][1]=='/tmp/scalafim-eco-'+name+'.py'
 segment={'name':name,'source_commit':revision,'counted_batches':count,'raw_log':str(log),'raw_log_sha256':digest(log) if log.exists() else None,'meta':meta,'interrupted':interrupted}
 if interrupted:
  tail=text[starts[-1].start():];segment['excluded_failed_batch']=names[-1];segment['excluded_errors']=re.findall(r'^\[error\].+$',tail,re.M);segment['excluded_case_failures']=re.findall(r'^==> X .+$',tail,re.M);segment['qualification']='Count only completed batch prefix. Failed partial batch excluded entirely and retained as adverse evidence.'
 segments.append(segment)
 for i,m in enumerate(starts[:count]):
  part=text[m.end():starts[i+1].start() if i+1<len(starts) else len(text)];name=m[1];summaries=[dict(zip(['total','failed','errors','passed','skipped'],map(int,(a,b,c,d,e or 0)))) for a,b,c,d,e in re.findall(r'Passed: Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)(?:, Skipped (\d+))?',part)]
  assert '[error]' not in part and all(x['failed']==x['errors']==0 and x['passed']>0 and x['total']==x['passed']+x['skipped'] for x in summaries),name
  targets=expanded[name];wanted=0 if name=='ecosystem-boundaries' else sum(t not in empty for t in targets);successes=len(re.findall(r'^\[success\]',part,re.M));assert len(summaries)==wanted and successes==len(targets),(name,len(summaries),wanted,successes,targets)
  nonexecutions=re.findall(r'^==> [si] .+$',part,re.M);assert sum(l.startswith('==> s ') for l in nonexecutions)==sum(x['skipped'] for x in summaries),name
  batches.append({'batch':name,'source_commit':revision,'segment':segments[-1]['name'],'expanded_test_targets':targets,'summaries':summaries,'success_markers':successes,'known_empty_targets':{t:empty[t] for t in targets if t in empty},'passed':sum(s['passed'] for s in summaries),'skipped':sum(s['skipped'] for s in summaries),'declared_nonexecutions':nonexecutions});bodies[name]=part
assert [b['batch'] for b in batches]==expected[:len(batches)]
record={'schema':'scalafim.eco-segmented-campaign.v2','status':'Pass' if final else 'Running','qualified_source_commit':source,'segments':segments,'expected_batch_names':expected,'completed_batches':batches,'passed':sum(b['passed'] for b in batches),'skipped':sum(b['skipped'] for b in batches),'ignored':sum(l.startswith('==> i ') for b in batches for l in b['declared_nonexecutions'])}
if final:
 assert len(batches)==37 and sum(len(b['summaries']) for b in batches)==140 and sum(b['success_markers'] for b in batches)==144
 for a,z,summary,success in [(0,17,56,59),(17,25,33,34),(25,37,51,51)]:assert sum(len(b['summaries']) for b in batches[a:z])==summary and sum(b['success_markers'] for b in batches[a:z])==success
 for filename,segment,start,stop in [('campaign-prefix-34.json',segments[0],0,17),('campaign-prefix-51.json',segments[1],17,25)]:
  captured=json.loads((base/filename).read_text());assert captured['source_commit']==segment['source_commit'] and captured['status']=='Interrupted' and captured['raw_log_sha256']==segment['raw_log_sha256'] and captured['meta']==segment['meta']
  assert [b['batch'] for b in captured['completed_batches']]==expected[start:stop]
  if filename=='campaign-prefix-51.json':assert captured['resume_inputs_sha256']==digest(base/'resume-inputs-51.json')
  for oldbatch,newbatch in zip(captured['completed_batches'],batches[start:stop]):assert oldbatch['summaries']==newbatch['summaries'] and oldbatch['success_markers']==newbatch['success_markers'] and oldbatch['expanded_test_targets']==newbatch['expanded_test_targets']
 common=['sbt','-Djava.awt.headless=true','-Dsbt.boot.directory='+str(cache/'boot'),'-Dsbt.global.base='+str(cache/'global'),'-Dsbt.ivy.home='+str(cache/'ivy'),'-Dsbt.supershell=false']
 for number,revision,offset in [(51,middle,17),(56,source,25)]:
  inputs=json.loads((base/('resume-inputs-'+str(number)+'.json')).read_text());assert inputs['source_commit']==revision and inputs['prefix_source_commit']==old
  assert inputs['prefix_receipt_sha256']==digest(base/'campaign-prefix-34.json') and inputs['resumed_batch_names']==expected[offset:] and inputs['frozen_runner_sha256']==hashlib.sha256(runner.encode()).hexdigest()
  assert inputs['cwd']==str(root) and inputs['cache']==str(cache) and inputs['controlled_JAVA_OPTS']=='-Dsbt.server.autostart=false -XX:ActiveProcessorCount=4' and inputs['free_bytes_at_preflight']>=20_000_000_000
  assert inputs['driver_sha256']==digest('/tmp/scalafim-eco-resume-'+str(number)+'.py') and inputs['commands']=={name:[*common,*commands[name]] for name in expected[offset:]}
  if number==56:assert inputs['middle_source_commit']==middle and inputs['middle_prefix_receipt_sha256']==digest(base/'campaign-prefix-51.json')
 compile_log=Path('/tmp/scalafim-eco-u6-compile-55.log');compile_meta=json.loads(Path(str(compile_log)+'.meta.json').read_text());compile_inputs=json.loads((base/'compile-resume-inputs-55.json').read_text());assert compile_meta['exit_code']==0 and compile_meta['reason']=='exited' and compile_meta['log_bytes']==compile_log.stat().st_size
 assert compile_meta['cwd']==str(root) and compile_meta['log']==str(compile_log)
 assert compile_inputs['source_commit']==source and compile_inputs['cwd']==str(root) and compile_inputs['cache']==str(cache) and compile_inputs['commands'][0][1]==common+['scalafimCompileAll'] and compile_meta['command']==common+['scalafimCompileAll']
 record['compile_all']={'source_commit':source,'raw_log_sha256':digest(compile_log),'meta':compile_meta,'input_receipt_sha256':digest(base/'compile-resume-inputs-55.json')}
 checks={'functional_group':('downstream','known effects survive public first-level fit, bridge, and fixed-effects group inference'),'identified_prediction':('downstream','dataset to identified prediction preserves literal values and training-only probabilities as one scenario'),'identified_rsa':('analysis','native scoped trial relations match independent beta-free signed distances and RSA')};record['existing_family_courts']={}
 for name,(prefix,title) in checks.items():
  courts=[]
  for platform in ['jvm','js']:
   batch=prefix+'-'+platform;matched=re.findall(r'^\s*\+ '+re.escape(title)+r' \d[^\n]*$',bodies[batch],re.M);assert len(matched)==1,(name,platform,matched);courts.append({'platform':platform,'batch':batch,'passed_case':matched[0].strip()})
  record['existing_family_courts'][name]=courts
 wanted=['ecosystem-measurement-scatter','gifti-freesurfer-scalar','ecosystem-linked-volume-projection','ecosystem-atlas-routing'];scenario_results=[]
 for batch,extra in [('examples',['freesurfer-geometry-jvm']),('examples-js',[])]:
  actual=re.findall(r'^scenario=([^ ]+) status=(\w+)$',bodies[batch],re.M);assert collections.Counter(actual)==collections.Counter((name,'Pass') for name in wanted+extra);scenario_results +=[{'batch':batch,'scenario':a,'status':b} for a,b in actual]
 assert len(scenario_results)==9;record['new_scenario_results']=scenario_results
 expected_private=json.loads((base/'acceptance-preflight-25.json').read_text())['private_artifact_inventory'];private=cache/'ivy/local';actual={str(p.relative_to(private)):digest(p) for p in private.rglob('*') if p.is_file()};assert actual==expected_private;record['private_artifact_inventory']=actual
 pins=dict(re.findall(r'lazy val (\w+)Revision\s*=\s*"([a-f0-9]{40})"',build_text));owners={}
 for git in sorted((cache/'global/staging').glob('*/*/.git')):
  owner=git.parent;head=subprocess.check_output(['git','rev-parse','HEAD'],cwd=owner,text=True).strip();dirty=subprocess.check_output(['git','status','--porcelain'],cwd=owner,text=True).strip();assert not dirty,str(owner)
  if owner.name=='spatial4s':pin=re.search(r'lazy val spatial4sRevision\s*=\s*"([a-f0-9]{40})"',next((cache/'global/staging').glob('*/image4s/build.sbt')).read_text())[1]
  elif owner.name=='linops4s':pin=re.search(r'lazy val linop4sRevision\s*=\s*"([a-f0-9]{40})"',next(p for p in (cache/'global/staging').glob('*/*/build.sbt') if p.parent.name.lower()=='alder').read_text())[1]
  else:pin=pins.get(owner.name)
  assert pin is not None and head==pin,(owner,head,pin);owners[str(owner)]={'head':head,'expected_pin':pin,'clean':True}
 assert set(pins)<={Path(p).name for p in owners};record['staged_source_inventory']=owners;record['source_inventory_scope']='Clean staged Git inventory, not actual classpath proof. Actual Compile closures and independent classpaths recorded separately.'
 empty_proof={}
 for target in empty:
  repo=target.split(':')[0];owner=next(Path(p) for p in owners if Path(p).name==repo);prefix='examples/consumer' if repo=='mvpa4s' else 'modules/neurosurf4s-spatial-io';paths=subprocess.check_output(['git','ls-tree','-r','--name-only',owners[str(owner)]['head'],'--',prefix],cwd=owner,text=True).splitlines();assert paths and not any('/src/test/' in p for p in paths)
  empty_proof[target]={'owner':repo,'revision':owners[str(owner)]['head'],'tracked_paths':paths,'qualification':empty[target]}
 record['source_backed_empty_targets']=empty_proof
 record['source_equivalence']={'source_phases':[old,middle,source],'alias_repair':'Only Root test alias and callers renamed; qualified six targets/settings/pins/compile aliases unchanged.','motion_repair':'Only JVM motion BIDS wildcard defaults corrected to provider globs plus discriminating assertions in existing case; strict admission/IO frame identity/numeric algorithms unchanged. Fresh compile-all and motionJVM/JS gates atfinalsource.','standalone':'App tool byte-identical to separately passed f0 witness; no Root dependency.'}
 record['qualification']='Three-segment local headless JVM/Scala.js campaign17+8+12. Both failed runs preserved. Historical native/browser evidence separately qualified; no hosted CI/main merge/new scientific or performance inference.'
 record['input_receipt_hashes']={name:digest(base/name) for name in ['campaign-prefix-34.json','campaign-prefix-51.json','resume-inputs-51.json','resume-inputs-56.json','compile-resume-inputs-55.json']}
path=base/('campaign-receipt-57.json' if final else 'campaign-progress-57.json');path.write_text(json.dumps(record,indent=2)+'\n');print(record['status'],len(batches),'/',len(expected),'passed',record['passed'],'skipped',record['skipped'],'ignored',record['ignored'])
