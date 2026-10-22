import json,re,subprocess
from pathlib import Path
baseline='86b3acf1e64d762be99b52ae146fa115b9050356'
repo='/Users/bbuchsbaum/code/scala/scalafim-eco-surface-20261021'
prior=json.loads(Path('/tmp/scalafim-eco-size-report-47.json').read_text())
slices={
 'neuroim4s':(['image','image-view','image-view-canvas','image-view-java2d','image-view-javafx','transform'],['image','imageView','imageViewCanvas','imageViewJava2d','imageViewJavafx','transform']),
 'neuroatlas4s':(['atlas'],['atlas']),
 'mvpa4s':(['mvpa'],['mvpa']),
 'neurosurf4s':(['surface','surface-view','surface-view-raster','surface-view-javafx','surface-view-three'],['surface','surfaceView','surfaceViewRaster','surfaceViewJavafx','surfaceViewThree'])}
build=subprocess.check_output(['git','-C',repo,'show',baseline+':build.sbt']).decode()
matches=list(re.finditer(r'(?m)^lazy val (\w+)[ \t]*=',build))
sections={m.group(1):build[m.end():matches[i+1].start() if i+1 < len(matches) else len(build)] for i,m in enumerate(matches)}
out={'method':{'baseline':baseline,'scope':'source-bearing selected entire modules, same physical LOC and path rules as report 47; ownership area not exact extraction inventory','dependency_count':'lexical call sites/mentions inside named baseline lazy val project definition sections only; excludes alias definitions and later settings, generated/project helper and transitive edges; provider after counts in report 47 cover whole build.sbt, so are not comparable to these slice figures','excluded':'mixed modules/spatial and mvpa-spatial; functional mvpa-fit, mvpa-dataset, mvpa-group, mvpa-artifacts and surface-view-connectivity; tests, examples and assets','qualification':'image and surface modules include generic machinery destined for other providers; atlas and mvpa modules include policy and integration that may remain or move elsewhere. Provider standalone totals include new work and optional integrations. These are area comparisons, not equivalent implementations or net ecosystem shrink.'},'slices':{}}
for name,(modules,projects) in slices.items():
 counts=prior['repositories']['scalafim']['before']['per_module'];missing=[x for x in modules if x not in counts]; assert not missing,missing
 sections_missing=[p for p in projects if p not in sections];assert not sections_missing,sections_missing
 code='\n'.join(sections[p] for p in projects)
 dep={'dependsOn_call_sites':len(re.findall(r'\.dependsOn\s*\(',code)),'ProjectRef_call_sites':len(re.findall(r'\bProjectRef\s*\(',code)),'libraryDependencies_mentions':len(re.findall(r'\blibraryDependencies\b',code))}
 after=prior['repositories'][name]['after']
 out['slices'][name]={'baseline_included_paths':[f'modules/{m}/{{shared,jvm,js}}/src/main/scala/**/*.scala' for m in modules],'baseline_module_roots':len(modules),'baseline_files':sum(counts[m]['files'] for m in modules),'baseline_physical_lines':sum(counts[m]['physical_lines'] for m in modules),'baseline_project_definitions':projects,'baseline_project_definition_dependency_syntax':dep,'standalone_after_revision':after['revision'],'standalone_after_module_roots':after['module_roots_count'],'standalone_after_files':after['production_scala_files'],'standalone_after_physical_lines':after['physical_production_scala_lines'],'standalone_after_whole_build_dependency_syntax':after['build_sbt_literal_dependency_syntax']}
Path('/tmp/scalafim-eco-ownership-slices-48.json').write_text(json.dumps(out,indent=2)+'\n')
for name, v in out['slices'].items():print(name, v['baseline_module_roots'],v['baseline_files'],v['baseline_physical_lines'], v['baseline_project_definition_dependency_syntax'],'=>',v['standalone_after_module_roots'],v['standalone_after_files'],v['standalone_after_physical_lines'],v['standalone_after_whole_build_dependency_syntax'])
