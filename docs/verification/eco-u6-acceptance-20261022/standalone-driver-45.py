from pathlib import Path
import os,sys,subprocess,shutil,json
base=Path('/Volumes/My Passport for Mac/codex-work/scalafim-eco-u6-20261022');cache=base/'independent-cache-26'
env=os.environ.copy()
for k in ['JAVA_TOOL_OPTIONS','JDK_JAVA_OPTIONS','_JAVA_OPTIONS','JAVA_OPTS','SBT_OPTS','SBT_JAVA_HOME']:env.pop(k,None)
env['JAVA_HOME']='/Users/bbuchsbaum/Library/Java/JavaVirtualMachines/openjdk-22/Contents/Home';env['PATH']=env['JAVA_HOME']+'/bin:'+env['PATH'];env['COURSIER_CACHE']=str(cache/'coursier');env['TERM']='dumb'
common=['sbt','-J-Xmx6g','-J-Xss4m','-J-XX:ActiveProcessorCount=4','-Dsbt.server.autostart=false','-Djava.awt.headless=true','-Dsbt.boot.directory='+str(cache/'boot'),'-Dsbt.global.base='+str(cache/'global'),'-Dsbt.ivy.home='+str(cache/'ivy'),'-Dsbt.supershell=false']
projects={}
for name,ids in projects.items():
 assert shutil.disk_usage('/System/Volumes/Data').free>=20_000_000_000,'host below repository20GB floor'
 cwd=base/'independent-consumers-26'/name
 assert not subprocess.check_output(['git','status','--porcelain'],cwd=cwd,text=True)
 commands=[x+'/test' for x in ids]+['show '+x+'/Compile/fullClasspath' for x in ids]
 log='/tmp/scalafim-eco-u6-independent-41-'+name+'.log'
 print('BEGIN',name,flush=True)
 code=subprocess.run([sys.executable,'/Users/bbuchsbaum/.agents/skills/lean-logs/scripts/run_logged.py','--log',log,'--cwd',str(cwd),'--timeout','5400','--',*common,*commands],env=env).returncode
 if code:sys.exit(code)
 print('COMPLETE',name,flush=True)
# Standalone composition app shares only this owned private build cache; no Root source dependency.
cwd=base/'standalone-composition-27'
code=subprocess.run([sys.executable,'/Users/bbuchsbaum/.agents/skills/lean-logs/scripts/run_logged.py','--log','/tmp/scalafim-eco-u6-standalone-45.log','--cwd',str(cwd),'--timeout','5400','--',*common,'runMain consumer.NeuroimCompositionConsumer','show Compile/fullClasspath'],env=env).returncode
sys.exit(code)
