from pathlib import Path
import numpy as np,json
base=Path(__file__).parent
n,p,m=24,4,7
i=np.arange(n);s=np.arange(m)
x=np.column_stack([np.ones(n),((i%2)*2-1), (i-(n-1)/2)/n,np.sin(i*.7)])
y=x@np.array([[.4,.8,-.2,1,0,.5,-.7],[.3,-.2,.9,0,.8,-.1,.2],[1,-.4,.2,-.3,.6,.5,.1],[-.2,.4,0,.7,-.5,.8,.6]])+np.cos((i[:,None]+1)*(s[None,:]+2)*.37)*.8
v=.04+(.03*(i[:,None]%5+1))*(1+.15*s[None,:])
for key,a in [('x',x),('y',y),('v',v)]:np.savetxt(base/(key+'.csv'),a,delimiter=',',fmt='%.17g')
# Scala literals freeze the same numeric fixture on JVM and Scala.js.
def vec(a):return 'Vector('+','.join(format(float(z),'.17g') for z in a)+')'
def mat(a):return 'Vector('+','.join(vec(row) for row in a)+')'
(base/'GroupAuditFixture.scala').write_text('package scalafim.fmri.group\nobject GroupAuditFixture:\n  val x = '+mat(x)+'\n  val y = '+mat(y)+'\n  val v = '+mat(v)+'\n')
