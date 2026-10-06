import os,sys,json,time,subprocess,hashlib
from pathlib import Path
root=Path(__file__).resolve().parent
work=root/'scalafim'
env=dict(os.environ)
env['JAVA_HOME']='/Users/bbuchsbaum/Library/Java/JavaVirtualMachines/temurin-21.0.12.1+1.jdk/Contents/Home'
env['PATH']=env['JAVA_HOME']+'/bin:'+env.get('PATH','')
env['SBT_WARM_HEAP']='4g'
env['SBT_WARM_CPUS']='4'
env['TEMPLATEFLOW_HOME']='/private/tmp/scalafim-templateflow-20261006'
env['JAVA_TOOL_OPTIONS']=' '.join('-Dscalafim.'+name+'.build='+str(root/name) for name in ('image4s','reframe4s','locus4s'))
record=root/'runs.json'
runs=json.loads(record.read_text()) if record.exists() else []
for label,command in zip(sys.argv[1::2],sys.argv[2::2]):
 print('BEGIN',label,command,flush=True)
 start=time.time();log=root/(label+'.log')
 with log.open('w') as out:
  result=subprocess.run(['python3','tools/build/sbt-warm',command],cwd=work,env=env,stdout=out,stderr=subprocess.STDOUT)
 entry={'label':label,'command':command,'exit':result.returncode,'elapsed_seconds':round(time.time()-start,3),'log':log.name,'sha256':hashlib.sha256(log.read_bytes()).hexdigest(),'JAVA_TOOL_OPTIONS':env['JAVA_TOOL_OPTIONS'],'SBT_WARM_HEAP':'4g','SBT_WARM_CPUS':'4','TEMPLATEFLOW_HOME':env['TEMPLATEFLOW_HOME']}
 runs.append(entry);record.write_text(json.dumps(runs,indent=2)+'\n')
 print('END',label,result.returncode,entry['elapsed_seconds'],flush=True)
 if result.returncode:
  print(log.read_text()[-12000:],flush=True);sys.exit(result.returncode)
