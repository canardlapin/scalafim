"""Compile the public-API probe against an explicit ScalaFIM source/classpath."""
import argparse,pathlib,subprocess,hashlib,json,os
P=pathlib.Path(__file__).resolve().parent
p=argparse.ArgumentParser();p.add_argument('--source-root',type=pathlib.Path,required=True);p.add_argument('--compiler-classpath-file',type=pathlib.Path,required=True);p.add_argument('--runtime-classpath-file',type=pathlib.Path,required=True);p.add_argument('--java',default='java');args=p.parse_args()
sources=sorted((args.source_root/'modules/group/shared/src/main/scala').rglob('*.scala'))
assert sources
classes=P/'classes';classes.mkdir(exist_ok=True)
runtime=args.runtime_classpath_file.read_text().strip()
command=[args.java,'-Xmx1g','-XX:ActiveProcessorCount=3','-cp',args.compiler_classpath_file.read_text().strip(),'dotty.tools.dotc.Main','-classpath',runtime,'-d',str(classes),*[str(s) for s in sources],str(P/'NativePmCalibration.scala')]
proc=subprocess.run(command,text=True,capture_output=True)
record=dict(command=command,exitCode=proc.returncode,stdout=proc.stdout,stderr=proc.stderr,sources={str(s):hashlib.sha256(s.read_bytes()).hexdigest() for s in sources},probeSha256=hashlib.sha256((P/'NativePmCalibration.scala').read_bytes()).hexdigest())
(P/'native-compile.json').write_text(json.dumps(record,indent=2));proc.check_returncode()
(P/'runtime-classpath.txt').write_text(str(classes)+os.pathsep+runtime)
