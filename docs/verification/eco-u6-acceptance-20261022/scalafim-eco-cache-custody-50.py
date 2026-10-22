from pathlib import Path
import subprocess,hashlib,json,os,stat,tarfile,shutil,time
base=Path('/Users/bbuchsbaum/.cache/scalafim-sbt/bases');out=Path('/Volumes/My Passport for Mac/codex-work/scalafim-eco-u6-20261022/completed-provider-caches-50');out.mkdir(exist_ok=True)
candidates=[('4d49f7713c287e21','/tmp/neuroatlas4s-eco-a1-consumer-08.log'),('cce42795ee783aae','/tmp/neurosurf4s-eco-u5-routing-consumer-42.log'),('ddb33e003b110ae5','/tmp/neurosurf4s-eco-u5-remote-consumer-15.log'),('49c01347942f5e83','/tmp/neurosurf4s-eco-u5-view-consumer-21.log'),('b89bb3362d80ed97','/tmp/neuroatlas4s-eco-u5-coherent-pins-01.log'),('9b17b6229e059640','/tmp/neuroim4s-eco-u5-coherent-pins-01.log'),('73b276cc4567d890','/tmp/neuroim4s-eco-u5-placement-01.log'),('cdfa76ab99aa7737','/tmp/reframe4s-eco-u5-pin-01.log'),('de5f463ac44160d5','/tmp/image4s-eco-u5-pin-01.log'),('c91ac93854f270ba','/tmp/mesh4s-eco-u5-pin-01.log'),('305c810418f781ec','/tmp/spatial4s-eco-u5-xyz-02.log')]
receipt={'scope':'Only session-owned stopped warm bases, exact metadata plus prior gate evidence. Full archives verified before removing internal copies. No other worktrees/cache contexts touched.','adverse_attempt':'Initial first-archive verifier interrupted during gzip backward seeks; no internal source copy had been removed. Resumed with sequential verification of every archive member.', 'started':time.time(),'before_free_bytes':shutil.disk_usage('/System/Volumes/Data').free,'contexts':[]}
def hfile(p):
 h=hashlib.sha256()
 with p.open('rb') as f:
  while b:=f.read(1024*1024):h.update(b)
 return h.hexdigest()
def inventory(p):
 files={};links={};sockets=[]
 for d,dirs,names in os.walk(p,followlinks=False):
  for name in dirs+names:
   q=Path(d)/name;key=str(q.relative_to(p));mode=q.lstat().st_mode
   if stat.S_ISLNK(mode):links[key]=os.readlink(q)
   elif stat.S_ISREG(mode):files[key]={'sha256':hfile(q),'bytes':q.stat().st_size}
   elif stat.S_ISSOCK(mode):sockets.append(key)
   else:assert stat.S_ISDIR(mode),(q,mode)
 return {'files':files,'links':links,'dead_sockets_omitted':sockets}
for key,evidence in candidates:
 if shutil.disk_usage('/System/Volumes/Data').free>=22_000_000_000:break
 source=base/key;assert source.is_dir() and not source.is_symlink();worktree=(source/'.worktree').read_text().strip()
 ev=Path(evidence)
 if key=='cce42795ee783aae' and not ev.exists():
  ev=Path('/Users/bbuchsbaum/code/scala/scalafim-eco-surface-20261021/docs/verification/eco-u5-20261021/routing-consumer-qualification.json')
 assert ev.exists() and key in ev.read_text(),(key,ev)
 openfiles=subprocess.run(['lsof','-Fn','+D',str(source)],capture_output=True,text=True,timeout=120)
 assert openfiles.returncode==1 and not openfiles.stdout,(key,openfiles.stdout,openfiles.stderr)
 print('ARCHIVE',key,worktree,flush=True)
 inv=inventory(source);manifest=out/(key+'.inventory.json');archive=out/(key+'.tar.gz')
 if archive.exists():
  assert manifest.exists() and json.loads(manifest.read_text())==inv,'retained archive/source inventory mismatch'
 else:
  manifest.write_text(json.dumps(inv,sort_keys=True,indent=2)+'\n')
  with tarfile.open(archive,'w:gz',compresslevel=3,dereference=False) as tf:
   def filt(member):return None if member.name.removeprefix(key+'/') in inv['dead_sockets_omitted'] else member
   tf.add(source,arcname=key,filter=filt)
 seen_files={};seen_links=set()
 with tarfile.open(archive,'r|gz') as tf:
  for member in tf:
   rel=member.name.removeprefix(key+'/')
   if member.isdir():continue
   if member.issym():
    assert rel in inv['links'] and member.linkname==inv['links'][rel],rel
    seen_links.add(rel);continue
   assert rel in inv['files'],rel
   record=inv['files'][rel]
   if member.islnk():
    assert member.linkname in seen_files and seen_files[member.linkname]==record,(rel,member.linkname)
   else:
    assert member.isfile(),member.name
    f=tf.extractfile(member);assert f is not None
    h=hashlib.sha256();n=0
    while b:=f.read(1024*1024):h.update(b);n+=len(b)
    assert h.hexdigest()==record['sha256'] and n==record['bytes'],rel
   seen_files[member.name]=record
 assert {p.removeprefix(key+'/') for p in seen_files}==set(inv['files'])
 assert seen_links==set(inv['links'])
 assert inventory(source)==inv,'source changed during archive'
 openfiles=subprocess.run(['lsof','-Fn','+D',str(source)],capture_output=True,text=True,timeout=120);assert openfiles.returncode==1 and not openfiles.stdout
 record={'source':str(source),'worktree':worktree,'evidence':str(ev),'evidence_sha256':hfile(ev),'inventory':str(manifest),'inventory_sha256':hfile(manifest),'archive':str(archive),'archive_sha256':hfile(archive),'regular_files':len(inv['files']),'symlinks':len(inv['links']),'verified':True,'internal_copy_removed':False}
 receipt['contexts'].append(record);(out/'custody.json').write_text(json.dumps(receipt,indent=2)+'\n')
 def writable(func,p,exc):os.chmod(p,stat.S_IRUSR|stat.S_IWUSR|stat.S_IXUSR);func(p)
 shutil.rmtree(source,onexc=writable);record['internal_copy_removed']=True;record['after_free_bytes']=shutil.disk_usage('/System/Volumes/Data').free
 (out/'custody.json').write_text(json.dumps(receipt,indent=2)+'\n');print('VERIFIED+RETAINED',key,'files',len(inv['files']),'freebytes',record['after_free_bytes'],flush=True)
receipt['finished']=time.time();receipt['after_free_bytes']=shutil.disk_usage('/System/Volumes/Data').free;(out/'custody.json').write_text(json.dumps(receipt,indent=2)+'\n')
print('DONE',receipt['after_free_bytes'],'contexts',len(receipt['contexts']),flush=True)
