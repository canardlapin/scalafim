from pathlib import Path
import subprocess,sys,time
log=Path('/tmp/scalafim-eco-u6-resume-51.log');assert not log.exists()
p=subprocess.Popen([sys.executable,'/Users/bbuchsbaum/.agents/skills/lean-logs/scripts/run_logged.py','--log',str(log),'--cwd','/Volumes/My Passport for Mac/codex-work/scalafim-eco-u6-20261022/acceptance-clone-25','--timeout','14400','--tail-bytes','1600','--',sys.executable,'/tmp/scalafim-eco-resume-51.py'])
seen=set()
while p.poll() is None:
 if log.exists():
  for line in log.read_text(errors='replace').splitlines():
   if line.startswith('[full-repository-tests] running ') and line not in seen:seen.add(line);print(line,flush=True)
 time.sleep(5)
sys.exit(p.returncode)
