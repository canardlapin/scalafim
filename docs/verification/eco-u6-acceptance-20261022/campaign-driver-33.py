from pathlib import Path
import os,sys,subprocess,shutil,json,time
base=Path('/Volumes/My Passport for Mac/codex-work/scalafim-eco-u6-20261022');root=base/'acceptance-clone-25';cache=base/'acceptance-cache-25'
source='31a42a69004c6282e01ad939c370f249fdf5e604'
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()==source
assert not subprocess.check_output(['git','status','--porcelain'],cwd=root,text=True)
assert shutil.disk_usage('/System/Volumes/Data').free>=20*1024**3
focus=json.loads(Path('/tmp/scalafim-eco-u6-focused-31.log.meta.json').read_text());assert focus['exit_code']==0
text=Path('/tmp/scalafim-eco-u6-focused-31.log').read_text()
assert text.count('status=Pass')==9 and 'status=Fail' not in text
assert 'Passed: Total 8, Failed 0, Errors 0, Passed 8' in text and 'Passed: Total 6, Failed 0, Errors 0, Passed 6' in text
env=os.environ.copy()
for k in ['JAVA_TOOL_OPTIONS','JDK_JAVA_OPTIONS','_JAVA_OPTIONS','JAVA_OPTS','SBT_OPTS','SBT_JAVA_HOME']:env.pop(k,None)
env['JAVA_HOME']='/Users/bbuchsbaum/Library/Java/JavaVirtualMachines/openjdk-22/Contents/Home';env['PATH']=env['JAVA_HOME']+'/bin:'+env['PATH'];env['COURSIER_CACHE']=str(cache/'coursier');env['SCALAFIM_SBT_CACHE_ROOT']=str(cache);env['TERM']='dumb'
# Controlled flags only; no inherited source overrides or unknown JVM options.
env['JAVA_OPTS']='-Dsbt.server.autostart=false -XX:ActiveProcessorCount=4'
common=['sbt','-J-XX:ActiveProcessorCount=4','-Dsbt.server.autostart=false','-Djava.awt.headless=true','-Dsbt.boot.directory='+str(cache/'boot'),'-Dsbt.global.base='+str(cache/'global'),'-Dsbt.ivy.home='+str(cache/'ivy'),'-Dsbt.supershell=false']
commands=[('compile-33',[*common,'scalafimCompileAll'],7200),('full-34',['bash','tools/ci/full-repository-tests.sh'],21600)]
receipt={'source_commit':source,'cwd':str(root),'cache':str(cache),'controlled_JAVA_OPTS':env['JAVA_OPTS'],'commands':commands,'started':time.time()}
(base/'campaign-33.json').write_text(json.dumps(receipt,indent=2)+'\n')
for name,command,timeout in commands:
 assert shutil.disk_usage('/System/Volumes/Data').free>=20*1024**3
 log='/tmp/scalafim-eco-u6-'+name+'.log'
 print('BEGIN',name,flush=True)
 proc=subprocess.Popen([sys.executable,'/Users/bbuchsbaum/.agents/skills/lean-logs/scripts/run_logged.py','--log',log,'--cwd',str(root),'--timeout',str(timeout),'--tail-bytes','1600','--',*command],env=env)
 seen=set()
 while proc.poll() is None:
  if Path(log).exists():
   for line in Path(log).read_text(errors='replace').splitlines():
    if line.startswith('[full-repository-tests] running ') and line not in seen:
     seen.add(line);print(line,flush=True)
  time.sleep(5)
 if proc.returncode:sys.exit(proc.returncode)
 print('COMPLETE',name,flush=True)
