import os,subprocess,sys
from pathlib import Path
root=Path('/Users/bbuchsbaum/code/scala/scalafim-eco-mesh-kernels-20261020');cache=Path('/Volumes/My Passport for Mac/codex-work/scalafim-eco-01a153cc-20261021/root-full-campaign-cache-10')
os.chdir(root)
assert subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()=='18c7b7725629a78cee5955df5c4448c4863dcb62'
assert subprocess.run(['git','diff','--quiet']).returncode==0
base=['sbt','-Djava.awt.headless=true',f'-Dsbt.boot.directory={cache}/boot',f'-Dsbt.global.base={cache}/global',f'-Dsbt.ivy.home={cache}/ivy','-Dsbt.supershell=false']
batches=[('analysis-js-tail',['mvpaPipelineJS/test','mvpaFitJS/test']),('downstream-jvm',['connectivityJVM/test','mvpaDatasetJVM/test','mvpaArtifactsJVM/test','mvpaSpatialJVM/test','mvpaFoundationAdmissionJVM/test','groupJVM/test','mvpaGroupJVM/test','fmriWorkflowJVM/test','archiveZarrJVM/test','datasetZarrJVM/test']),('downstream-js',['connectivityJS/test','mvpaDatasetJS/test','mvpaArtifactsJS/test','mvpaSpatialJS/test','mvpaFoundationAdmissionJS/test','groupJS/test','mvpaGroupJS/test','fmriWorkflowJS/test','archiveZarrJS/test','datasetZarrJS/test']),('examples',['surfaceExamplesJVM/test','atlasExamplesJVM/test','workflowExamplesJVM/test']),('examples-js',['workflowExamplesJS/test'])]
for name,tasks in batches:
 print('[full-repository-tests] resuming '+name,flush=True)
 result=subprocess.run(base+tasks)
 if result.returncode:sys.exit(result.returncode)
