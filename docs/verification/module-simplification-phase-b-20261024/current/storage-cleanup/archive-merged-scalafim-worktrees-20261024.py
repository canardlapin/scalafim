import subprocess,json,datetime,hashlib,tarfile,os,stat,plistlib
from pathlib import Path
AUDIT=Path('/Volumes/EcoBrainHFS20261023/work/worktree-cleanup-20261024/audit.json')
ROOT=Path('/Volumes/My Passport for Mac/codex-work/module-simplification-20261024/retired-worktrees')
info=plistlib.loads(subprocess.check_output(['diskutil','info','-plist','/Volumes/My Passport for Mac']))
assert info['VolumeUUID']=='51B72C66-64BB-3C5F-8853-2219CA2FFA9F' and info['Writable']
ROOT.mkdir(parents=True,exist_ok=True)
def sha_stream(f):
 h=hashlib.sha256()
 for chunk in iter(lambda:f.read(1024*1024),b''):h.update(chunk)
 return h.hexdigest()
def digest(path):
 with path.open('rb') as f:return sha_stream(f)
def manifest(path):
 rows=[]
 for parent,dirs,files in os.walk(path,followlinks=False):
  for name in sorted(dirs+files):
   p=Path(parent)/name;s=p.lstat();rel=p.relative_to(path).as_posix()
   row={'path':rel,'mode':stat.S_IMODE(s.st_mode)}
   if p.is_symlink():row.update(kind='symlink',target=os.readlink(p))
   elif p.is_file():row.update(kind='file',bytes=s.st_size,sha256=digest(p))
   elif p.is_dir():row['kind']='directory'
   else:raise ValueError('Unsupported file: '+str(p))
   rows.append(row)
 return sorted(rows,key=lambda r:r['path'])
def git(path,*args):return subprocess.check_output(['git','-C',str(path),*args],text=True).strip()
rows=[x for x in json.loads(AUDIT.read_text())['existing_internal_candidates'] if x['ancestor_of_origin_main'] and x['dirty_lines']==0]
assert len(rows)==6
results=[]
for row in rows:
 source=Path(row['worktree']);name=source.name;folder=ROOT/name;folder.mkdir(exist_ok=False)
 assert git(source,'rev-parse','HEAD')==row['HEAD']
 assert not git(source,'status','--porcelain=v1','--untracked-files=all')
 subprocess.run(['git','-C',row['repo'],'merge-base','--is-ancestor',row['HEAD'],'origin/main'],check=True)
 before=manifest(source)
 (folder/'source-manifest.json').write_text(json.dumps(before,indent=2)+'\n')
 archive=folder/(name+'.tar.gz')
 print('Archiving',source,len(before),'entries',flush=True)
 with tarfile.open(archive,'w:gz',compresslevel=1,dereference=False) as tar:tar.add(source,arcname=name,recursive=True)
 expected={x['path']:x for x in before};seen=set()
 with tarfile.open(archive,'r:gz') as tar:
  for member in tar:
   if member.name==name:continue
   prefix=name+'/'
   assert member.name.startswith(prefix),member.name
   key=member.name[len(prefix):];r=expected[key];assert key not in seen,key;seen.add(key)
   assert member.mode==r['mode'],key
   if r['kind']=='file':
    assert member.isfile() and member.size==r['bytes'],key
    with tar.extractfile(member) as f:assert sha_stream(f)==r['sha256'],key
   elif r['kind']=='symlink':assert member.issym() and member.linkname==r['target'],key
   else:assert member.isdir(),key
 assert seen==set(expected)
 assert manifest(source)==before,'Source changed during archive'
 result={'schema':'merged-worktree-custody.v1','source':str(source),'repo':row['repo'],'head':row['HEAD'],'branch':row.get('branch'),'main_containing_head':row['origin_main'],'archive':str(archive),'archive_bytes':archive.stat().st_size,'archive_sha256':digest(archive),'manifest_sha256':digest(folder/'source-manifest.json'),'entries_verified':len(before),'volume_uuid':info['VolumeUUID'],'verified_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'status':'archive-verified-source-retained','restoration':'Keep the existing branch/ref. For a usable Git checkout, git worktree add --detach <fresh-path> <head>, then restore archived ignored artifacts as needed. This archive retains every source, ignored artifact and original gitdir pointer; do not reuse that pointer after retirement.'}
 (folder/'custody.json').write_text(json.dumps(result,indent=2)+'\n');results.append(result)
 print('VERIFIED',name,result['archive_bytes'],result['archive_sha256'],flush=True)
(ROOT/'custody-index.json').write_text(json.dumps({'schema':'merged-worktree-custody-index.v1','entries':results,'no_sources_removed':True},indent=2)+'\n')
print('All six archives verified; no source deletion performed.',flush=True)
