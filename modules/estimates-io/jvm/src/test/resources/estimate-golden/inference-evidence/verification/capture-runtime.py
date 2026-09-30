import pathlib,subprocess,json,re,hashlib,os
root=pathlib.Path('/private/tmp/scalafim-execution-20260929')
log=(root/'logs/inference-evidence-jvm-final-r2.log').read_text()
entries=re.findall(r'\[info\] \* Attributed\(([^\n]+)\)',log)
assert entries,'No actual classpath entries in completed JVM gate'
records=[]
for name in entries:
 p=pathlib.Path(name)
 if p.is_file():
  records.append({'path':name,'kind':'file','sha256':hashlib.sha256(p.read_bytes()).hexdigest(),'bytes':p.stat().st_size})
 elif p.is_dir():
  files={str(f.relative_to(p)):{'sha256':hashlib.sha256(f.read_bytes()).hexdigest(),'bytes':f.stat().st_size} for f in sorted(p.rglob('*')) if f.is_file()}
  data=json.dumps(files,sort_keys=True,separators=(',',':')).encode()
  records.append({'path':name,'kind':'directory','contentManifestSha256':hashlib.sha256(data).hexdigest(),'files':files})
 else: raise AssertionError(f'Classpath entry missing: {name}')
assert not any('/modules/fit/' in p or '/modules/fit-estimates/' in p for p in entries),'Fresh reader classpath includes fitter'
(root/'inference-evidence-runtime-closure.json').write_text(json.dumps({'actualSbtLauncher':'Homebrew Java25.0.1; sbt1.11.7; Scala3.7.4','node':subprocess.check_output(['node','--version'],text=True).strip(),'classpath':records},sort_keys=True,indent=2)+'\n')
java_homes=re.findall(r'\[info\] ans: String = ([^\n]+)',log)
java_home=next((p for p in java_homes if pathlib.Path(p,'bin/java').is_file()),None)
assert java_home,'Missing actual sbt Java home eval receipt'
cmd=[str(pathlib.Path(java_home)/'bin/java'),'-cp',os.pathsep.join(entries),'scalafim.estimates.io.InferenceEvidenceReadbackProbe',str(root/'inference-evidence/modules/estimates-io/jvm/src/test/resources/estimate-golden/inference-evidence')]
(root/'inference-evidence-readback-argv.json').write_text(json.dumps(cmd,indent=2)+'\n')
raise SystemExit(subprocess.call(['python3','/Users/bbuchsbaum/.agents/skills/lean-logs/scripts/run_logged.py','--log',str(root/'logs/inference-evidence-fresh-readback-final.log'),'--cwd',str(root/'inference-evidence'),'--timeout','120','--',*cmd]))
