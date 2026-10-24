import subprocess,json,hashlib,datetime,plistlib,sys,shutil
from pathlib import Path
root=Path('/Volumes/My Passport for Mac/codex-work/module-simplification-20261024/retired-worktrees')
record=Path('/Volumes/EcoBrainHFS20261023/work/worktree-cleanup-20261024/removals.json')
info=plistlib.loads(subprocess.check_output(['diskutil','info','-plist','/Volumes/My Passport for Mac']))
assert info['VolumeUUID']=='51B72C66-64BB-3C5F-8853-2219CA2FFA9F'
def sha(path):
 h=hashlib.sha256()
 with path.open('rb') as f:
  for b in iter(lambda:f.read(1024*1024),b''):h.update(b)
 return h.hexdigest()
removed=json.loads(record.read_text()) if record.exists() else []
for name in sys.argv[1:]:
 c=json.loads((root/name/'custody.json').read_text());source=Path(c['source']);assert source.exists() and not source.is_symlink()
 assert c['status']=='archive-verified-source-retained'
 assert sha(Path(c['archive']))==c['archive_sha256']
 assert sha(root/name/'source-manifest.json')==c['manifest_sha256']
 assert subprocess.check_output(['git','-C',str(source),'rev-parse','HEAD'],text=True).strip()==c['head']
 assert not subprocess.check_output(['git','-C',str(source),'status','--porcelain=v1','--untracked-files=all'],text=True).strip()
 subprocess.run(['git','-C',c['repo'],'merge-base','--is-ancestor',c['head'],'origin/main'],check=True)
 handle=subprocess.run(['lsof','-n','-F','pn'],capture_output=True,text=True);assert handle.returncode==0
 pid=None;matches=[]
 for line in handle.stdout.splitlines():
  if line.startswith('p'):pid=line[1:]
  elif line.startswith('n') and (line[1:]==str(source) or line[1:].startswith(str(source)+'/')):matches.append({'pid':pid,'path':line[1:]})
 assert not matches,matches
 # Fresh byte/type/mode check of every archived entry and absence of new entries.
 import os,stat
 expected=json.loads((root/name/'source-manifest.json').read_text());seen=set()
 for parent,dirs,files in os.walk(source,followlinks=False):
  for entry in dirs+files:seen.add((Path(parent)/entry).relative_to(source).as_posix())
 assert seen=={x['path'] for x in expected}
 for row in expected:
  f=source/row['path'];s=f.lstat();assert stat.S_IMODE(s.st_mode)==row['mode']
  if row['kind']=='file':assert f.is_file() and not f.is_symlink() and s.st_size==row['bytes'] and sha(f)==row['sha256'],str(f)
  elif row['kind']=='symlink':assert f.is_symlink() and os.readlink(f)==row['target'],str(f)
  else:assert f.is_dir() and not f.is_symlink(),str(f)
 before=shutil.disk_usage('/private/tmp').free
 subprocess.run(['git','-C',c['repo'],'worktree','remove','--',str(source)],check=True)
 assert not source.exists()
 if c.get('branch'):assert subprocess.check_output(['git','-C',c['repo'],'rev-parse',c['branch']],text=True).strip()==c['head']
 result={'source':str(source),'head':c['head'],'branch_retained':c.get('branch'),'archive':c['archive'],'archive_sha256':c['archive_sha256'],'entries_verified_immediately_before_removal':len(expected),'removed_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'free_before':before,'free_after':shutil.disk_usage('/private/tmp').free,'normal_git_worktree_remove':True}
 removed.append(result);record.write_text(json.dumps(removed,indent=2)+'\n');print(json.dumps(result),flush=True)
