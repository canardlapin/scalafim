import json,os,subprocess,sys,time
from pathlib import Path
root=Path('/Users/bbuchsbaum/code/scala/scalafim');out=Path('/private/tmp/scalafim-lwu-capacity-gates-20261006');label=sys.argv[1]
env=dict(os.environ);env['SBT_WARM_HEAP']='4g';env['SBT_WARM_CPUS']='2';env.pop('JAVA_TOOL_OPTIONS',None)
nodebin='/private/tmp/scalafim-main-reconciliation-readiness-20261006/node-v24.21.0-darwin-arm64/bin';env['PATH']=nodebin+os.pathsep+env['PATH'];env['TERM']='dumb'
command=['python3','tools/build/sbt-warm',*sys.argv[2:]];started=time.time()
with(out/(label+'.log')).open('w') as stream:
 result=subprocess.run(command,cwd=root,env=env,stdin=subprocess.DEVNULL,stdout=stream,stderr=subprocess.STDOUT)
meta={'command':command,'cwd':str(root),'exit_code':result.returncode,'seconds':time.time()-started,'heap':'4g','cpus':2,'gale_override':env.get('JAVA_TOOL_OPTIONS'),'node':subprocess.check_output([nodebin+'/node','--version'],text=True).strip()}
(out/(label+'.json')).write_text(json.dumps(meta,indent=2)+'\n');print(json.dumps(meta),flush=True);print((out/(label+'.log')).read_text()[-6500:],flush=True);sys.exit(result.returncode)
