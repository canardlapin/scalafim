from pathlib import Path
import subprocess,hashlib,json,os,stat,tarfile,shutil,time,fcntl,uuid
home=Path('/Users/bbuchsbaum/.cache/scalafim-sbt');out=Path('/Volumes/My Passport for Mac/codex-work/scalafim-eco-u6-20261022/completed-unused-templates-53');out.mkdir(exist_ok=True)
keys=['1d56318c0de63526','3622033f1a4fb9e9','4659ce7086d55ce5','ca12d05708478403','d64f3fa3bdfac650','330a22c0af39d155','0d46f5365b79916d','e954889cde38f831','b044acc04306f3ae','6ea912fcd9f2445f','0bd280a402eb3d78','5df1fb6ef24aff27','c67051ee3a8cd07a','b34d8b789dddea77','d7275d754c0b95da']
receipt={'scope':'Only own-created immutable optional template seeds, no modern/legacy current base references. External byte/mode/directory/link custody, global wrapper lock for fresh recheck+quarantine. No symlinks; existing bases unchanged; absent seed uses normal cold bootstrap.','before_free_bytes':shutil.disk_usage('/System/Volumes/Data').free,'started':time.time(),'contexts':[]}
def sha(p):
 h=hashlib.sha256()
 with p.open('rb') as f:
  while b:=f.read(1024*1024):h.update(b)
 return h.hexdigest()
def inventory(root):
 entries={}
 for d,dirs,names in os.walk(root,followlinks=False):
  paths=[Path(d)] + [Path(d)/name for name in names] + [Path(d)/name for name in dirs if (Path(d)/name).is_symlink()]
  for p in paths:
   st=p.lstat();rel=str(p.relative_to(root));r={'mode':stat.S_IMODE(st.st_mode),'mtime':st.st_mtime}
   if stat.S_ISREG(st.st_mode):r.update(kind='file',sha256=sha(p),bytes=st.st_size)
   elif stat.S_ISDIR(st.st_mode):r.update(kind='directory')
   elif stat.S_ISLNK(st.st_mode):r.update(kind='symlink',target=os.readlink(p))
   else:raise RuntimeError('unsupported/live special entry '+str(p))
   entries[rel]=r
 return entries
def references(modern,legacy):
 hits=[]
 for d in (home/'bases').iterdir():
  if not d.is_dir():continue
  for name in ['.pin-key-v2','.pin-key']:
   p=d/name
   if p.is_file() and p.read_text().strip() in [modern,legacy]:hits.append(str(p))
 return hits
def no_open_files(source):
 r=subprocess.run(['lsof','-Fn','+D',str(source)],capture_output=True,text=True,timeout=120)
 assert r.returncode==1 and not r.stdout and not r.stderr,(source,r.returncode,r.stdout,r.stderr)
logs=list(Path('/tmp').glob('*eco-u5*.log'))
for key in keys:
 if shutil.disk_usage('/System/Volumes/Data').free>=22_000_000_000:break
 source=home/'templates'/key;assert source.is_dir() and not source.is_symlink();assert (source/'.pin-key-v2').read_text().strip()==key
 legacy=(source/'.pin-key').read_text().strip();assert not references(key,legacy),(key,references(key,legacy))
 evidence=next(p for p in logs if '[sbt-warm] saving pin template '+key+' ' in p.read_text());no_open_files(source)
 identity=(source.stat().st_dev,source.stat().st_ino);print('ARCHIVE UNUSED OWN SEED',key,flush=True)
 inv=inventory(source);manifest=out/(key+'.inventory.json');archive=out/(key+'.tar.gz');assert not archive.exists();manifest.write_text(json.dumps(inv,sort_keys=True,indent=2)+'\n')
 with tarfile.open(archive,'w:gz',compresslevel=3,dereference=False) as tf:tf.add(source,arcname=key)
 seen={}
 with tarfile.open(archive,'r|gz') as tf:
  for member in tf:
   rel='.' if member.name==key else member.name.removeprefix(key+'/');r=inv[rel]
   assert member.mode==r['mode'] and abs(member.mtime-r['mtime'])<0.000001,(rel,'metadata')
   if member.isdir():assert r['kind']=='directory'
   elif member.issym():assert r['kind']=='symlink' and member.linkname==r['target']
   elif member.islnk():
    assert r['kind']=='file' and member.linkname in seen
    assert seen[member.linkname]['sha256']==r['sha256'] and seen[member.linkname]['bytes']==r['bytes']
   else:
    assert member.isfile() and r['kind']=='file';f=tf.extractfile(member);assert f is not None;h=hashlib.sha256();n=0
    while b:=f.read(1024*1024):h.update(b);n+=len(b)
    assert h.hexdigest()==r['sha256'] and n==r['bytes'],rel
   seen[member.name]=r
 assert {'.' if p==key else p.removeprefix(key+'/') for p in seen}==set(inv)
 assert inventory(source)==inv,'source changed while archiving'
 record={'original_template':str(source),'modern_key':key,'legacy_key':legacy,'creation_log':str(evidence),'creation_log_sha256':sha(evidence),'archive':str(archive),'archive_sha256':sha(archive),'inventory':str(manifest),'inventory_sha256':sha(manifest),'entries':len(inv),'regular_files':sum(x['kind']=='file' for x in inv.values()),'byte_mode_directory_link_verified':True,'internal_copy_removed':False}
 receipt['contexts'].append(record);(out/'custody.json').write_text(json.dumps(receipt,indent=2)+'\n')
 with (home/'bases.lock').open('a') as lock:
  fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
  assert (source.stat().st_dev,source.stat().st_ino)==identity
  assert not references(key,legacy),(key,'new base reference')
  no_open_files(source)
  quarantine=source.with_name('.retained-'+key+'-'+uuid.uuid4().hex);assert not quarantine.exists();source.rename(quarantine)
  assert (quarantine.stat().st_dev,quarantine.stat().st_ino)==identity
  record['captured_quarantine']=str(quarantine);record['quarantine_identity']=identity
  (out/'custody.json').write_text(json.dumps(receipt,indent=2)+'\n')
 # Never touch original source name after unlocking: another process may recreate it.
 def writable(func,p,exc):os.chmod(p,stat.S_IRUSR|stat.S_IWUSR|stat.S_IXUSR);func(p)
 shutil.rmtree(quarantine,onexc=writable);record['internal_copy_removed']=True;record['after_free_bytes']=shutil.disk_usage('/System/Volumes/Data').free
 (out/'custody.json').write_text(json.dumps(receipt,indent=2)+'\n');print('VERIFIED+RETAINED UNUSED SEED',key,'entries',len(inv),'freebytes',record['after_free_bytes'],flush=True)
receipt['finished']=time.time();receipt['after_free_bytes']=shutil.disk_usage('/System/Volumes/Data').free;(out/'custody.json').write_text(json.dumps(receipt,indent=2)+'\n');print('DONE',receipt['after_free_bytes'],'contexts',len(receipt['contexts']),flush=True)
