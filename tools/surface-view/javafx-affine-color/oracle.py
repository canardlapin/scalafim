from pathlib import Path
import hashlib,json,sys
import numpy as np
from PIL import Image

ROOT=Path(__file__).parent
SOURCE_SHA=hashlib.sha256(Path(__file__).read_bytes()).hexdigest()

TOLERANCE=2.0  # frozen in plan.json before the first native rendering

def read_mesh(path):
 data=path.read_bytes();header=np.frombuffer(data,dtype='>i4',count=7)
 version,nv,nf=header[:3];assert version in (23,24)
 off=28;points=np.frombuffer(data,dtype='>f4',count=nv*3,offset=off).astype(float).reshape(-1,3);off+=nv*12
 packed=np.frombuffer(data,dtype='>u4',count=nv,offset=off);off+=nv*4
 rgb=np.stack([(packed>>shift)&255 for shift in (24,16,8)],axis=1).astype(float)
 assert np.all((packed&255)==255)
 triangles=np.frombuffer(data,dtype='>i4',count=nf*3,offset=off).astype(int).reshape(-1,3)
 off+=nf*12
 if version==24:
  normals=np.frombuffer(data,dtype='>f4',count=nv*3,offset=off).astype(float).reshape(-1,3);off+=nv*12
  ambient,diffuse,dx,dy,dz=np.frombuffer(data,dtype='>f8',count=5,offset=off);off+=40
  assert np.all(np.isfinite(normals)) and np.max(np.abs(np.linalg.norm(normals,axis=1)-1))<1e-6
  assert 0<=ambient<=1 and 0<=diffuse<=1 and abs(dx*dx+dy*dy+dz*dz-1)<1e-12
  # Independent vectorized Lambert calculation from original normals and RGB.
  # JavaFX's contract rounds shaded vertex colours before affine interpolation.
  factor=np.minimum(1,ambient+diffuse*np.maximum(0,normals @ np.array([dx,dy,dz])))
  rgb=np.floor(rgb*factor[:,None]+0.5)
 assert off==len(data)
 return points[:,:2],rgb,triangles,header[3:7]

def reference(path):
 cache_directory=path.parent.parent/'oracle-cache';cache_directory.mkdir(exist_ok=True)
 key=hashlib.sha256(path.read_bytes()+SOURCE_SHA.encode()+np.__version__.encode()).hexdigest();cache=cache_directory/(key+'.npz')
 if cache.exists():
  with np.load(cache) as a:return {k:a[k] for k in a.files}
 points,colors,faces,boundary=read_mesh(path)
 tri=points[faces];rgb=colors[faces]
 lower=np.floor(tri.min(axis=1)).astype(int).clip(0,255)
 upper=np.floor(tri.max(axis=1)).astype(int).clip(0,255)
 widths=upper[:,0]-lower[:,0]+1;heights=upper[:,1]-lower[:,1]+1
 counts=widths*heights;fi=np.repeat(np.arange(len(faces)),counts)
 local=np.arange(counts.sum())-np.repeat(np.cumsum(counts)-counts,counts)
 x=lower[fi,0]+local%widths[fi];y=lower[fi,1]+local//widths[fi]
 xy=np.stack([x,y],axis=1).astype(float);pid=y*256+x
 p=tri[fi];c=rgb[fi];lo=np.full((len(fi),3),np.inf);hi=np.full((len(fi),3),-np.inf)
 a=p[:,0];ab=p[:,1]-a;ac=p[:,2]-a;det=ab[:,0]*ac[:,1]-ab[:,1]*ac[:,0]
 assert np.all(det>0)
 def bary(q):
  delta=q-a
  b=(delta[:,0]*ac[:,1]-delta[:,1]*ac[:,0])/det
  cc=(ab[:,0]*delta[:,1]-ab[:,1]*delta[:,0])/det
  return np.stack([1-b-cc,b,cc],axis=1)
 def include(value,valid):
  np.minimum(lo,np.where(valid[:,None],value,np.inf),out=lo)
  np.maximum(hi,np.where(valid[:,None],value,-np.inf),out=hi)
 # The vertices of triangle intersect pixel are: contained triangle vertices,
 # contained pixel corners, and triangle-edge/pixel-edge intersections.
 for vertex in range(3):
  inside=np.all((p[:,vertex]>=xy-1e-10)&(p[:,vertex]<=xy+1+1e-10),axis=1)
  include(c[:,vertex],inside)
 for corner in ((0,0),(1,0),(1,1),(0,1)):
  w=bary(xy+corner)
  include(np.sum(w[:,:,None]*c,axis=1),np.all(w>=-1e-10,axis=1))
 for edge in range(3):
  start=p[:,edge];delta=p[:,(edge+1)%3]-start
  for axis in range(2):
   other=1-axis
   for side in (0,1):
    t=np.divide(xy[:,axis]+side-start[:,axis],delta[:,axis],out=np.full(len(fi),np.nan),where=delta[:,axis]!=0)
    at=start[:,other]+t*delta[:,other]
    valid=(t>=-1e-10)&(t<=1+1e-10)&(at>=xy[:,other]-1e-10)&(at<=xy[:,other]+1+1e-10)
    include(c[:,edge]+t[:,None]*(c[:,(edge+1)%3]-c[:,edge]),valid)
 low=np.full((256*256,3),np.inf);high=np.full_like(low,-np.inf)
 hits=np.zeros(256*256,dtype=int)
 valid=np.all(np.isfinite(lo),axis=1)
 np.minimum.at(low,pid[valid],lo[valid]);np.maximum.at(high,pid[valid],hi[valid]);np.add.at(hits,pid[valid],1)
 w=bary(xy+.5);centers=np.all(w>=-1e-10,axis=1)
 expected=np.full_like(low,np.nan)
 expected[pid[centers]]=np.sum(w[centers,:,None]*c[centers],axis=1)
 # Use only pixel squares strictly inside the original outer quadrilateral.
 # This excludes partial background coverage but never masks internal edges.
 ys,xs=np.indices((256,256));origin=np.stack([xs.ravel(),ys.ravel()],axis=1)
 full=np.ones(256*256,dtype=bool);border=points[boundary]
 for corner in ((0,0),(1,0),(1,1),(0,1)):
  q=origin+corner
  for edge in range(4):
   v=border[(edge+1)%4]-border[edge];d=q-border[edge]
   full &= v[0]*d[:,1]-v[1]*d[:,0]>1e-7
 assert full.sum()>10000
 assert np.all(np.isfinite(expected[full]))
 assert np.all(low[full]<=high[full]+1e-8)
 result=dict(low=low,high=high,expected=expected,full=full,edges=hits>1)
 np.savez_compressed(cache,**result)
 return result

def assess(folder):
 results=[]
 for binary in sorted(folder.glob('*.bin')):
  ref=reference(binary);image=np.asarray(Image.open(binary.with_suffix('.png')).convert('RGB'),dtype=float).reshape(-1,3)
  full=ref['full'];excess=np.maximum(np.maximum(ref['low']-image,image-ref['high']),0)
  error=np.abs(image-ref['expected'])
  aa=binary.stem.endswith('BALANCED')
  populations={}
  for name,mask in [('all',full),('internalEdges',full&ref['edges']),('triangleInteriors',full&~ref['edges'])]:
   n=int(mask.sum())
   populations[name]={'pixels':n,'maxFootprintExcess':float(excess[mask].max()) if n else None,
    'outOfFootprintBudgetPixels':int(np.any(excess[mask]>TOLERANCE,axis=1).sum()),
    'centerChannelRms':float(np.sqrt(np.mean(error[mask]**2))) if n else None,
    'maxCenterChannelError':float(error[mask].max()) if n else None,
    'outOfCenterBudgetPixels':int(np.any(error[mask]>TOLERANCE,axis=1).sum())}
  passed=populations['all']['outOfFootprintBudgetPixels']==0 and (aa or populations['all']['outOfCenterBudgetPixels']==0)
  results.append({'case':binary.stem,'passed':passed,'msaa':aa,'metrics':populations})
  print(binary.stem,passed,'excess',round(populations['all']['maxFootprintExcess'],4),'centerMax',round(populations['all']['maxCenterChannelError'],4),flush=True)
 permutations=[]
 for family in ['diagonal','rgb','ramp32','ramp128']:
  for transform in ['front','oblique']:
   for aa in ['DISABLED','BALANCED']:
    original=folder/f'{family}-original-{transform}-{aa}.png'
    if not original.exists():continue
    a=np.asarray(Image.open(original).convert('RGB'),dtype=float)
    mask=reference(original.with_suffix('.bin'))['full'].reshape(256,256)
    for order in ['cyclic','shuffled']:
     b=np.asarray(Image.open(folder/f'{family}-{order}-{transform}-{aa}.png').convert('RGB'),dtype=float)
     error=np.abs(a-b)[mask]
     permutations.append({'family':family,'transform':transform,'aa':aa,'order':order,'maxChannelDifference':float(error.max()),'outOfBudgetPixels':int(np.any(error>TOLERANCE,axis=1).sum())})
 config_path=folder/'fixture-config.json'
 config=json.loads(config_path.read_text()) if config_path.exists() else {'maxTextureSize':256,'lighting':'Unlit'}
 report={'fixtureConfig':config,'productionPlanSha256':hashlib.sha256((ROOT/'production-plan.json').read_bytes()).hexdigest() if config['maxTextureSize']!=256 or config['lighting']!='Unlit' else None,'schema':'plsneuro.javafx-affine-gpu-color-evidence.v1','planSha256':hashlib.sha256((ROOT/'plan.json').read_bytes()).hexdigest(),'tolerance':TOLERANCE,'cases':results,'permutations':permutations,'passed':len(results)==48 and all(x['passed'] for x in results) and all(x['outOfBudgetPixels']==0 for x in permutations)}
 (folder/'color-report.json').write_text(json.dumps(report,indent=2)+'\n')
 print('SUMMARY',folder.name,'pass',report['passed'],'cases',sum(x['passed'] for x in results),'of',len(results),'permutation failures',sum(x['outOfBudgetPixels']>0 for x in permutations),flush=True)
 return report

if __name__=='__main__':
 if len(sys.argv)!=2:raise SystemExit('usage: oracle.py NATIVE_FIXTURE_DIRECTORY')
 raise SystemExit(0 if assess(Path(sys.argv[1]))['passed'] else 1)
