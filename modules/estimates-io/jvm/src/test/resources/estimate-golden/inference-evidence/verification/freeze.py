import hashlib,json,pathlib,subprocess,sys,datetime
work=pathlib.Path('/private/tmp/scalafim-execution-20260929/inference-evidence')
owned=pathlib.Path('/private/tmp/scalafim-execution-20260929/inference-evidence-paths.txt').read_text().splitlines()
tracked=subprocess.check_output(['git','-C',str(work),'ls-files','-z']).decode().split('\0')
paths={p for p in tracked if p}
for name in owned:
 p=work/name
 if p.is_dir(): paths.update(str(x.relative_to(work)) for x in p.rglob('*') if x.is_file() and 'verification' not in x.relative_to(p).parts)
 elif p.is_file(): paths.add(name)
records={p:{'sha256':hashlib.sha256((work/p).read_bytes()).hexdigest(),'bytes':(work/p).stat().st_size} for p in sorted(paths) if (work/p).is_file()}
result={'createdUtc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'worktree':str(work),'base':subprocess.check_output(['git','-C',str(work),'rev-parse','HEAD'],text=True).strip(),'files':records,'ownedPaths':owned,'documentaryExclusions':'Generated verification receipts excluded to avoid recursive hashes; final report is documentary and may be completed after gates.'}
pathlib.Path(sys.argv[1]).write_text(json.dumps(result,sort_keys=True,indent=2)+'\n')
print('Frozen',len(records),'source/input files')
