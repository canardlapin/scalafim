from pathlib import Path
import os,sys,subprocess,shutil,json,time,importlib.util,hashlib
base=Path('/Volumes/My Passport for Mac/codex-work/scalafim-eco-u6-20261022');root=base/'acceptance-clone-25';cache=base/'acceptance-cache-25'
source='92154d81c39b6a384be195dde0e933ed0dff29ca';intermediate='616860a2e14eb42b800462cd3d2f4fe4203f641a';old='31a42a69004c6282e01ad939c370f249fdf5e604'
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()==source
assert not subprocess.check_output(['git','status','--porcelain'],cwd=root,text=True)
assert shutil.disk_usage('/System/Volumes/Data').free>=20_000_000_000,'repository20GBpreflight'
spec=importlib.util.spec_from_file_location('inventory',root/'tools/ci/check_test_inventory.py');inv=importlib.util.module_from_spec(spec);spec.loader.exec_module(inv)
script=(root/'tools/ci/full-repository-tests.sh').read_text();batches=inv.release_batches(script)
oldscript=subprocess.check_output(['git','show',old+':tools/ci/full-repository-tests.sh'],cwd=root,text=True);oldbatches=inv.release_batches(oldscript)
prefix=json.loads((base/'campaign-prefix-34.json').read_text());completed=[r['batch'] for r in prefix['completed_batches']];assert len(completed)==17 and list(batches)[:17]==completed
for name in completed:assert batches[name]==oldbatches[name],name
middle=json.loads((base/'campaign-prefix-51.json').read_text());assert middle['source_commit']==intermediate and middle['status']=='Interrupted'
completed+= [r['batch'] for r in middle['completed_batches']];assert len(completed)==25 and list(batches)[:25]==completed
assert script==subprocess.check_output(['git','show',intermediate+':tools/ci/full-repository-tests.sh'],cwd=root,text=True)
remaining=list(batches)[25:];assert len(remaining)==12 and remaining[0]=='threshold-motion-jvm'
env=os.environ.copy()
for k in ['JAVA_TOOL_OPTIONS','JDK_JAVA_OPTIONS','_JAVA_OPTIONS','JAVA_OPTS','SBT_OPTS','SBT_JAVA_HOME']:env.pop(k,None)
env['JAVA_HOME']='/Users/bbuchsbaum/Library/Java/JavaVirtualMachines/openjdk-22/Contents/Home';env['PATH']=env['JAVA_HOME']+'/bin:'+env['PATH'];env['COURSIER_CACHE']=str(cache/'coursier');env['SCALAFIM_SBT_CACHE_ROOT']=str(cache);env['TERM']='dumb';env['JAVA_OPTS']='-Dsbt.server.autostart=false -XX:ActiveProcessorCount=4'
common=['sbt','-Djava.awt.headless=true','-Dsbt.boot.directory='+str(cache/'boot'),'-Dsbt.global.base='+str(cache/'global'),'-Dsbt.ivy.home='+str(cache/'ivy'),'-Dsbt.supershell=false']
receipt={'driver_sha256':hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),'schema':'scalafim.eco-resume-inputs.v1','source_commit':source,'prefix_source_commit':old,'middle_source_commit':intermediate,'middle_prefix_receipt_sha256':hashlib.sha256((base/'campaign-prefix-51.json').read_bytes()).hexdigest(),'prefix_receipt_sha256':hashlib.sha256((base/'campaign-prefix-34.json').read_bytes()).hexdigest(),'resumed_batch_names':remaining,'commands':{name:[*common,*batches[name]] for name in remaining},'frozen_runner_sha256':hashlib.sha256(script.encode()).hexdigest(),'controlled_JAVA_OPTS':env['JAVA_OPTS'],'cwd':str(root),'cache':str(cache),'free_bytes_at_preflight':shutil.disk_usage('/System/Volumes/Data').free,'started':time.time()}
(base/'resume-inputs-56.json').write_text(json.dumps(receipt,indent=2)+'\n')
# Each command is the exact frozen runner's batch target list and sbt args; fresh JVM per batch.
# This driver resumes an accepted prefix rather than manufacturing a new full-run verdict.
for name in remaining:
 print('[full-repository-tests] running '+name,flush=True)
 code=subprocess.call(receipt['commands'][name],cwd=root,env=env)
 if code:sys.exit(code)
print('RESUME COMPLETE:12/12batches',flush=True)
