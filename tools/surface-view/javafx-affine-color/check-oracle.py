import json,sys,numpy as np
from pathlib import Path
from oracle import reference
folder=Path(sys.argv[1]);root=folder.parent;ys,xs=np.indices((256,256));x=xs.ravel();y=ys.ravel()
errors={}
for family in ['diagonal','rgb','ramp32']:
 r=reference(folder/(family+'-original-front-DISABLED.bin'));mask=r['full']
 u=(x+.5-20)/216;v=(y+.5-20)/216
 if family=='diagonal':
  center=np.repeat((255*np.minimum(1,u+v))[:,None],3,axis=1)
  low=np.repeat((255*np.minimum(1,(x+y-40)/216))[:,None],3,axis=1)
  high=np.repeat((255*np.minimum(1,(x+y+2-40)/216))[:,None],3,axis=1)
 elif family=='ramp32':
  center=np.repeat((32+192*u)[:,None],3,axis=1)
  low=np.repeat((32+192*(x-20)/216)[:,None],3,axis=1)
  high=np.repeat((32+192*(x+1-20)/216)[:,None],3,axis=1)
 else:
  center=np.stack([255*np.abs(1-u-v),255*u,255*v],axis=1)
  lo=(x+y-40)/216;hi=(x+y+2-40)/216
  redLo=np.where((lo<=1)&(hi>=1),0,np.minimum(np.abs(1-lo),np.abs(1-hi)))
  redHi=np.maximum(np.abs(1-lo),np.abs(1-hi))
  low=255*np.stack([redLo,(x-20)/216,(y-20)/216],axis=1)
  high=255*np.stack([redHi,(x+1-20)/216,(y+1-20)/216],axis=1)
 errors[family]={k:float(np.max(np.abs(r[k][mask]-expected[mask]))) for k,expected in [('expected',center),('low',low),('high',high)]}
 assert max(errors[family].values())<1e-10,(family,errors[family])
(root/'oracle-self-check.json').write_text(json.dumps({'kind':'independent closed-form fields validate the generic triangle/pixel intersection oracle','passed':True,'errors':errors},indent=2)+'\n');print(errors)
