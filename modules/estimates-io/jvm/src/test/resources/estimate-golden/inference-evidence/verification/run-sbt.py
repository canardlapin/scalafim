import fcntl, os, pathlib, subprocess, sys, time
root=pathlib.Path(__file__).resolve().parent
if len(sys.argv)<4: raise SystemExit('usage: run-sbt.py WORKTREE LOG_NAME TASK...')
work, name, *tasks=sys.argv[1:]
log=root/'logs'/name
log.parent.mkdir(exist_ok=True)
# A project-load failure (sbt could not load build definitions, e.g. a staged
# dependency's compiled build classes were rewritten by an sbt process outside
# this lock) is transient and precedes any task, so it is retried a bounded
# number of times. Each failed attempt's log and metadata are kept as
# LOG.load-failure-N[.meta.json]; every other failure is returned unchanged.
LOAD_RETRIES=2
def load_failure(path):
 try: text=path.read_text(errors='replace')
 except OSError: return False
 return 'Project loading failed' in text and ('NoClassDefFoundError' in text or 'ClassNotFoundException' in text)
with (root/'sbt.lock').open('a') as lock:
 print('Waiting for shared build resource slot', flush=True)
 fcntl.flock(lock, fcntl.LOCK_EX)
 print('Acquired shared build resource slot', flush=True)
 cmd=['python3','/Users/bbuchsbaum/.agents/skills/lean-logs/scripts/run_logged.py','--log',str(log),'--cwd',work,'--timeout','3600','--','sbt','--batch','-J-Xmx3g','-J-XX:ActiveProcessorCount=4',*tasks]
 attempt=0
 while True:
  code=subprocess.call(cmd)
  if code==0 or attempt>=LOAD_RETRIES or not load_failure(log): raise SystemExit(code)
  attempt+=1
  kept=log.with_name(f'{log.name}.load-failure-{attempt}')
  os.replace(log, kept)
  meta=pathlib.Path(str(log)+'.meta.json')
  if meta.exists(): os.replace(meta, pathlib.Path(str(kept)+'.meta.json'))
  print(f'sbt project load failed (transient); kept {kept.name}; retry {attempt}/{LOAD_RETRIES}', flush=True)
  time.sleep(5)
