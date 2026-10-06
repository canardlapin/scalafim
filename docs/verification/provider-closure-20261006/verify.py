from pathlib import Path
import hashlib,json,re,tarfile
root=Path(__file__).resolve().parent
receipt=json.loads((root/'receipt.json').read_text())
archive=root/receipt['archive']
assert hashlib.file_digest(archive.open('rb'),'sha256').hexdigest()==receipt['archive_sha256']
with tarfile.open(archive,'r:gz') as tar:
 def data(name):
  f=tar.extractfile(name)
  assert f is not None,name
  return f.read()
 manifest=json.loads(data('source-manifest.json'))
 for path,wanted in manifest['verified_module_source_sha256'].items():
  assert hashlib.sha256(data('sources/'+path)).hexdigest()==wanted,path
 runs=json.loads(data('runs.json'))
 totals={'jvm':0,'js':0};skips={'jvm':0,'js':0};test_gates=0
 for run in runs:
  assert run['exit']==0,run['label']
  blob=data('logs/'+run['log'])
  assert hashlib.sha256(blob).hexdigest()==run['sha256'],run['log']
  text=blob.decode()
  assert '[warn]' not in text and '[error]' not in text,run['log']
  if run['label'].endswith(('-jvm','-js')):
   rows=re.findall(r'Passed: Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)(?:, Skipped (\d+))?',text)
   assert len(rows)==1,run['label']
   total,failed,errors,passed,skip=rows[0]
   assert failed==errors=='0'
   skip=int(skip or 0);assert int(total)==int(passed)+skip
   platform=run['label'].rsplit('-',1)[1];totals[platform]+=int(passed);skips[platform]+=skip;test_gates+=1
 assert totals==receipt['passed'] and skips==receipt['skipped']
 assert test_gates==34
 for platform in ('jvm','js'):
  text=data('logs/atlas-'+platform+'-classpath.log').decode()
  lines=[line for line in text.splitlines()if line.startswith('/private/tmp/')]
  assert len(lines)==1
  paths=lines[0].split(':');owners={}
  for path in paths:
   m=re.search(r'/modules/((?:image4s|locus4s)-[^/]+)/',path)
   if m and path.endswith('/classes'):owners.setdefault(m[1],[]).append(path)
  assert all(len(entries)==1 for entries in owners.values())
  for name in ('image4s-core','image4s-geometry','locus4s-core','locus4s-data'):assert len(owners[name])==1
  for name,entries in owners.items():
   repo='image4s' if name.startswith('image4s') else 'locus4s'
   assert entries[0].startswith(manifest['local_overrides'][repo]+'/')
 native=data('logs/atlas-jvm.log').decode().split('scalafim.atlas.io.MniTemplateBridgeFilesSuite:')[-1]
 assert len(re.findall(r'^  \+ ',native,re.M))==8
 for path,wanted in receipt['portable_artifact_sha256'].items():
  assert hashlib.sha256(data(path)).hexdigest()==wanted,path
print('Verified local candidate records: '+str(sum(totals.values()))+' passing tests; skips '+str(skips)+'; '+str(len(manifest['verified_module_source_sha256']))+' frozen source/fixture files')
