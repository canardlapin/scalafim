from pathlib import Path
import subprocess,sys
cp=Path('/private/tmp/plsneuro-main-adoption-20261016/modules/adapters/target/streams/test/fullClasspath/_global/streams/export').read_text().strip()
cp='/private/tmp/plsneuro-main-providers-20261016/scalafim-baseline/modules/surface-view/jvm/target/scala-3.7.4/classes:'+cp
c=Path('/Users/bbuchsbaum/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang')
compiler=[c/f'{a}/3.7.4/{a}-3.7.4.jar' for a in ('scala3-compiler_3','scala3-interfaces','tasty-core_3')]+[c/'modules/scala-asm/9.8.0-scala-1/scala-asm-9.8.0-scala-1.jar',c.parent/'scala-sbt/compiler-interface/1.10.7/compiler-interface-1.10.7.jar']
out=Path('.agent-work/frozen-surface-classes');out.mkdir(exist_ok=True)
r=subprocess.run(['java','-cp',':'.join(map(str,compiler))+':'+cp,'dotty.tools.dotc.Main','-classpath',cp,'-d',str(out),'.agent-work/FreezeCanonicalScene.scala'])
if r.returncode:sys.exit(r.returncode)
r=subprocess.run(['java','-cp',str(out)+':'+cp,'scalafim.surface.view.FreezeCanonicalScene','.agent-work/canonical-v8-surface.json'])
if r.returncode:sys.exit(r.returncode)
print(Path('.agent-work/canonical-v8-surface.json').read_text())
