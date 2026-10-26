from pathlib import Path
import subprocess,re,hashlib,json
base=Path('/Volumes/My Passport for Mac/codex-work/scalafim-eco-u6-20261022');cache=base/'independent-cache-26';store=cache/'global/staging'
config={'neuroim':('29',[8,8,2,2,8,8]),'mvpa':('29',[4,4,1,1,1,1]),'atlas':('29',[1,1,1,1,1,1,1]),'surface':('37',[1,1,3,3,2,2]),'surface-routing':('41',[2,2])}
owners={};records={}
def owner_of(path):
 p=Path(path)
 for x in [p,*p.parents]:
  if (x/'.git').exists():
   if x not in owners:
    owners[x]={'head':subprocess.check_output(['git','rev-parse','HEAD'],cwd=x,text=True).strip(),'dirty':subprocess.check_output(['git','status','--porcelain'],cwd=x,text=True).strip()}
    assert not owners[x]['dirty'],str(x)
   return x
 raise AssertionError('unowned source class directory '+path)
for name,(run,counts) in config.items():
 p=Path('/tmp/scalafim-eco-u6-independent-'+run+'-'+name+'.log');meta=Path(str(p)+'.meta.json')
 assert meta.exists() and json.loads(meta.read_text())['exit_code']==0,name
 text=p.read_text();results=[tuple(map(int,m)) for m in re.findall(r'Passed: Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)',text)]
 assert results==[(n,0,0,n) for n in counts],(name,results)
 entries=re.findall(r'^\[info\] \* Attributed\((.+)\)$',text,re.M);sections=[]
 for entry in entries:
  if '/independent-consumers-26/'+name+'/' in entry:sections.append({'root':entry,'entries':[]})
  assert sections
  assert '/test-classes' not in entry and '/testkit' not in entry.lower() and not Path(entry).name.startswith('scalafim-'),entry
  sections[-1]['entries'].append(entry)
 assert len(sections)==len(counts),(name,len(sections))
 for section in sections:
  source=[]
  for entry in section['entries'][1:]:
   if entry.endswith('/classes'):
    own=owner_of(entry);assert store in own.parents,entry
    assert own.name!='scalafim'
    source.append((own.name,str(Path(entry).relative_to(own))))
   else:assert cache in Path(entry).parents,entry
  section['source_modules']=source
  section['entries_sha256']=hashlib.sha256('\n'.join(section['entries']).encode()).hexdigest()
 # Minimal core courts are first two (JVM andJS), and must keep optional edges absent.
 if name in ['neuroim','mvpa','atlas','surface']:
  for section in sections[:2]:
   modules=section['source_modules']
   if name=='neuroim':assert not any(repo in ['neuroatlas4s','neurosurf4s','mvpa4s','intaglio'] or (repo=='neuroim4s' and not ('neuroim4s-core/' in mod or 'neuroim4s-spaces/' in mod)) for repo,mod in modules),modules
   if name=='mvpa':assert not any(repo in ['image4s','mesh4s','neuroim4s','neuroatlas4s','neurosurf4s','intaglio'] for repo,mod in modules),modules
   if name=='atlas':assert not any(repo in ['image4s','mesh4s','neuroim4s','neurosurf4s','mvpa4s','intaglio'] or (repo=='neuroatlas4s' and 'neuroatlas4s-core/' not in mod) for repo,mod in modules),modules
   if name=='surface':assert not any(repo in ['neuroatlas4s','mvpa4s','intaglio'] or (repo=='neuroim4s' and 'neuroim4s-spaces/' not in mod) or (repo=='neurosurf4s' and 'neurosurf4s-core/' not in mod) for repo,mod in modules),modules
 records[name]={'test_counts':counts,'total_passed':sum(counts),'raw_log_sha256':hashlib.sha256(p.read_bytes()).hexdigest(),'classpath_sections':sections,'meta':json.loads(meta.read_text())}
build=(base/'acceptance-clone-25/build.sbt').read_text()
pins=dict(re.findall(r'lazy val (\w+)Revision\s*=\s*"([a-f0-9]{40})"',build))
for directory,record in owners.items():
 if directory.name=='spatial4s':
  image=next(d for d in owners if d.name=='image4s')
  expected_pin=re.search(r'lazy val spatial4sRevision\s*=\s*"([a-f0-9]{40})"',(image/'build.sbt').read_text()).group(1)
 else:expected_pin=pins[directory.name]
 assert record['head']==expected_pin,(str(directory),record['head'],expected_pin)
 record['expected_pin']=expected_pin
expected=json.loads((base/'acceptance-preflight-25.json').read_text())['private_artifact_inventory'];private=cache/'ivy/local'
actual={str(p.relative_to(private)):hashlib.sha256(p.read_bytes()).hexdigest() for p in private.rglob('*') if p.is_file()};assert actual==expected
manifest_path=base/'independent-inputs-29.json'
input_manifest=json.loads(manifest_path.read_text())
for name,record in input_manifest.items():
 harness=base/'independent-consumers-26'/name
 assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=harness,text=True).strip()==record['commit']
 assert not subprocess.check_output(['git','status','--porcelain'],cwd=harness,text=True)
 tracked=[p for p in subprocess.check_output(['git','ls-files','-z'],cwd=harness).decode().split('\0') if p]
 assert set(tracked)==set(record['input_files'])
 assert all(hashlib.sha256((harness/name).read_bytes()).hexdigest()==digest for name,digest in record['input_files'].items())
out={'schema':'scalafim.eco-independent-consumers.v1','input_manifest_sha256':hashlib.sha256(manifest_path.read_bytes()).hexdigest(),'scope':'Compile classpaths; test-internals conclusion additionally reviewed from build mappings and sources','total_passed':sum(x['total_passed'] for x in records.values()),'harnesses':records,'source_owners':{str(k):v for k,v in owners.items()},'private_artifact_inventory':actual}
assert out['total_passed']==71
(base/'independent-receipt-40.json').write_text(json.dumps(out,indent=2)+'\n');print('independent cases',out['total_passed'],'sourceowners',len(owners),'classpath sections',sum(len(r['classpath_sections']) for r in records.values()))
