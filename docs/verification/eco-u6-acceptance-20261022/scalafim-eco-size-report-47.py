import json,re,subprocess
from pathlib import Path
repos={
 'scalafim':('/Users/bbuchsbaum/code/scala/scalafim-eco-surface-20261021','86b3acf1e64d762be99b52ae146fa115b9050356','92154d81c39b6a384be195dde0e933ed0dff29ca'),
 'neuroim4s':('/Users/bbuchsbaum/code/scala/neuroim4s-eco-u5-20261021',None,'4f75c2bf141449d032f9fe3c558ef13a22a4d59f'),
 'neuroatlas4s':('/Users/bbuchsbaum/code/scala/neuroatlas4s-eco-surface-20261022',None,'4df875d8a1cd20afd3b83ed49446422bc507e5ec'),
 'mvpa4s':('/Users/bbuchsbaum/code/scala/mvpa4s-eco-u6-20261022',None,'c27469484bf760c34cd896dabe4fe1c384a83b45'),
 'neurosurf4s':('/Users/bbuchsbaum/code/scala/neurosurf4s-eco-routing-20261021',None,'73a244fbe5d7b4db2f0c946d98fa2a3c00101ab7')}
def git(repo,*args): return subprocess.check_output(['git','-C',repo,*args])
def count(repo,rev):
 paths=git(repo,'ls-tree','-r','--name-only',rev).decode().splitlines()
 sources=[p for p in paths if re.match(r'^modules/[^/]+/(?:shared|jvm|js)/src/main/scala/.*\.scala$',p)]
 roots=sorted({p.split('/')[1] for p in sources})
 process=subprocess.Popen(['git','-C',repo,'cat-file','--batch'],stdin=subprocess.PIPE,stdout=subprocess.PIPE)
 # communicate cannot be used here: batch protocol includes binary blobs and headers.
 loc=0; bymodule={m:{'files':0,'physical_lines':0} for m in roots}
 for path in sources:
  process.stdin.write((rev+':'+path+'\n').encode());process.stdin.flush()
  header=process.stdout.readline().decode().strip().split(); size=int(header[-1]); data=process.stdout.read(size); process.stdout.read(1)
  lines=len(data.splitlines()); loc+=lines; m=path.split('/')[1]; bymodule[m]['files']+=1;bymodule[m]['physical_lines']+=lines
 process.stdin.close();process.wait()
 build=git(repo,'show',rev+':build.sbt').decode(); code='\n'.join(line.split('//',1)[0] for line in build.splitlines())
 # Strip // comments without destroying URL-containing ProjectRef declarations: no dependency calls on URLs in these builds.
 dependencies={'dependsOn_call_sites':len(re.findall(r'\.dependsOn\s*\(',code)), 'ProjectRef_call_sites':len(re.findall(r'\bProjectRef\s*\(',code)), 'libraryDependencies_mentions':len(re.findall(r'\blibraryDependencies\b',code))}
 return {'revision':rev,'module_roots_count':len(roots),'module_roots':roots,'production_scala_files':len(sources),'physical_production_scala_lines':loc,'per_module':bymodule,'build_sbt_literal_dependency_syntax':dependencies}
out={'method':{'scope':'tracked modules/<module>/{shared,jvm,js}/src/main/scala/**/*.scala only; excludes tests, examples and generated/untracked sources','lines':'physical lines including blanks and comments, counted with Python bytes.splitlines','module_roots':'distinct module directory names represented by at least one in-scope Scala source','dependencies':'lexical counts in root build.sbt after stripping // comments: .dependsOn call sites, ProjectRef call sites, libraryDependencies mentions; not distinct dependency edges or resolved graphs','baseline':'four providers are absent from root baseline build.sbt; no provider baseline pin can be inferred from that build and no zero baseline is asserted','limitations':'module roots are source-bearing directories rather than sbt projects; physical LOC is not net functionality and moved code may be counted at new ownership; lexical dependency counts are not resolved edges and exclude generated, transitive, plugin and helper/macro expansion'},'repositories':{}}
for name,(repo,before,after) in repos.items():
 out['repositories'][name]={'source_git_repository':repo,'before':count(repo,before) if before else None,'after':count(repo,after)}
Path('/tmp/scalafim-eco-size-report-47.json').write_text(json.dumps(out,indent=2)+'\n')
for name,v in out['repositories'].items():
 for phase in ('before','after'):
  x=v[phase]
  if x: print(name,phase,x['module_roots_count'],x['production_scala_files'],x['physical_production_scala_lines'],x['build_sbt_literal_dependency_syntax'])
