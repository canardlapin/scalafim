from pathlib import Path
import os,sys,subprocess,shutil,json,time
base=Path('/Volumes/My Passport for Mac/codex-work/scalafim-eco-u6-20261022');root=base/'acceptance-clone-25';cache=base/'acceptance-cache-25';source='92154d81c39b6a384be195dde0e933ed0dff29ca'
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()==source
assert not subprocess.check_output(['git','status','--porcelain'],cwd=root,text=True)
assert shutil.disk_usage('/System/Volumes/Data').free>=20_000_000_000
env=os.environ.copy()
for k in ['JAVA_TOOL_OPTIONS','JDK_JAVA_OPTIONS','_JAVA_OPTIONS','JAVA_OPTS','SBT_OPTS','SBT_JAVA_HOME']:env.pop(k,None)
env['JAVA_HOME']='/Users/bbuchsbaum/Library/Java/JavaVirtualMachines/openjdk-22/Contents/Home';env['PATH']=env['JAVA_HOME']+'/bin:'+env['PATH'];env['COURSIER_CACHE']=str(cache/'coursier');env['SCALAFIM_SBT_CACHE_ROOT']=str(cache);env['TERM']='dumb';env['JAVA_OPTS']='-Dsbt.server.autostart=false -XX:ActiveProcessorCount=4'
compile_command=['sbt','-Djava.awt.headless=true','-Dsbt.boot.directory='+str(cache/'boot'),'-Dsbt.global.base='+str(cache/'global'),'-Dsbt.ivy.home='+str(cache/'ivy'),'-Dsbt.supershell=false','scalafimCompileAll']
commands=[('compile-55',compile_command,7200),('resume-56',[sys.executable,'/tmp/scalafim-eco-resume-56.py'],14400)]
(base/'compile-resume-inputs-55.json').write_text(json.dumps({'source_commit':source,'cwd':str(root),'cache':str(cache),'controlled_JAVA_OPTS':env['JAVA_OPTS'],'commands':commands,'free_bytes_at_preflight':shutil.disk_usage('/System/Volumes/Data').free,'started':time.time()},indent=2)+'\n')
for name,command,timeout in commands:
 assert shutil.disk_usage('/System/Volumes/Data').free>=20_000_000_000
 log=Path('/tmp/scalafim-eco-u6-'+name+'.log');assert not log.exists();print('BEGIN',name,flush=True)
 p=subprocess.Popen([sys.executable,'/Users/bbuchsbaum/.agents/skills/lean-logs/scripts/run_logged.py','--log',str(log),'--cwd',str(root),'--timeout',str(timeout),'--tail-bytes','1600','--',*command],env=env)
 seen=set()
 while p.poll() is None:
  if log.exists():
   for line in log.read_text(errors='replace').splitlines():
    if line.startswith('[full-repository-tests] running ') and line not in seen:seen.add(line);print(line,flush=True)
  time.sleep(5)
 if p.returncode:sys.exit(p.returncode)
 print('COMPLETE',name,flush=True)
