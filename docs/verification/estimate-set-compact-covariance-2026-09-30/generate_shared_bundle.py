"""Independent Core-2 specimen. Python standard library only; no Scala encoder.

U matrices are literal symmetric strictly diagonally dominant positive matrices.
All entries/scales are binary rational numbers. NIfTI scale bytes implement
logical variance = 2 * stored + 1, independently of the residual multiplier.
"""
from pathlib import Path
from fractions import Fraction
import hashlib
import json
import struct
import sys

OUTPUT = Path(sys.argv[1])
D = '00000000-0000-4000-8000-000000000091'
M = '00000000-0000-4000-8000-000000000092'
UNIT = '00000000-0000-4000-8000-000000000093'
REV = '00000000-0000-4000-8000-000000000094'
PREFIX = f'units/{REV}'
AXIS = ['z', 'a/β', 'M']
OBS = ['row-z', 'row-a']
SUPPORT = [0, 3, 5]
UPPER = [[4., -1., .5, 9., 2., 16.], [1., .25, -.5, 2., .75, 3.]]
SCALES = [[2., 0., 5.], [3., 7., 0.]]
REFS = []

def encoded(value):
    return (json.dumps(value, indent=2, ensure_ascii=False, allow_nan=False) + '\n').encode('utf-8')

def document(kind, content, compact=False):
    return encoded({'ProfileVersion':'0.2.0', 'Schema':f'scalafim-estimates-core-nifti-{2 if compact else 1}',
        'DocumentKind':kind, 'Content':content, 'WireVersion':'2.0.0' if compact else '1.0.0'})

def save(name, data):
    p = OUTPUT/name
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_bytes(data)
    ref = {'Path':name, 'SHA256':hashlib.sha256(data).hexdigest(), 'Bytes':len(data)}
    REFS.append(ref)
    return ref

def nifti(dtype, payload, slope=1., intercept=0.):
    h = bytearray(352)
    struct.pack_into('<i',h,0,348)
    struct.pack_into('<8h',h,40,4,2,3,1,3,1,1,1)
    struct.pack_into('<h',h,70,dtype)
    struct.pack_into('<h',h,72,64 if dtype==64 else 8)
    struct.pack_into('<8f',h,76,*([1.]*8))
    struct.pack_into('<3f',h,108,352.,slope,intercept)
    h[123] = 2
    struct.pack_into('<h',h,254,1)
    for offset,row in [(280,(1.,0.,0.,0.)),(296,(0.,1.,0.,0.)),(312,(0.,0.,1.,0.))]:
        struct.pack_into('<4f',h,offset,*row)
    h[344:348] = b'n+1\0'
    return bytes(h)+payload

catalog = {'model':M,'entries':[{'id':i,'label':'repeated label','kind':'Coefficient','units':'signal',
    'normalization':'unit','definition':i} for i in AXIS]}
catalog_ref = save(f'{PREFIX}/estimands.json',document('catalog',catalog))
et = save(f'{PREFIX}/estimands.tsv',('index\testimand_id\n'+''.join(f'{k}\t{i}\n' for k,i in enumerate(AXIS))).encode())
ot = save(f'{PREFIX}/observations.tsv',('index\tobservation_id\n'+''.join(f'{k}\t{i}\n' for k,i in enumerate(OBS))).encode())
reps = []
for kind in ['effect','scale']:
    for o,obs in enumerate(OBS):
        values=[]; codes=[]
        for target in range(3):
            for sample in range(6):
                codes.append(0 if sample in SUPPORT else 1)
                logical = (100*o+10*target+sample) if kind=='effect' else (SCALES[o][SUPPORT.index(sample)] if sample in SUPPORT else 0.)
                values.append(logical if kind=='effect' else (logical-1.)/2.)
        v=save(f'{PREFIX}/{kind}-{o}-values.nii',nifti(64,struct.pack('<18d',*values),1. if kind=='effect' else 2.,0. if kind=='effect' else 1.))
        c=save(f'{PREFIX}/{kind}-{o}-validity.nii',nifti(2,bytes(codes)))
        reps.append({'Tag':'Nifti','Content':{'product':kind,'observation':obs,'values':v,'validity':c,
            'precision':'Float64','slope':1. if kind=='effect' else 2.,'intercept':0. if kind=='effect' else 1.,
            'volumeOrder':AXIS,'selectedTransform':'scanner-sform','pairOrder':[],
            'qformAlternativeFrame':None,'storedDatatype':'Float64'}})
for o,obs in enumerate(OBS):
    pairs=[]; ordinal=0
    for i in range(3):
        for j in range(i,3):
            pairs.append({'First':i,'Second':j,'Value':UPPER[o][ordinal],'Validity':0})
            ordinal+=1
    table={'Schema':'scalafim-estimates-shared-normalized-upper-triangle-1','WireVersion':'1.0.0',
        'Product':'U','Observation':obs,'Estimands':AXIS,'Precision':'Float64',
        'ValidityBroadcast':'SupportedSamples','Pairs':pairs}
    ref=save(f'{PREFIX}/U-{o}.json',encoded(table))
    reps.append({'Tag':'SharedNormalizedUpperTriangle','Content':{'product':'U','observation':obs,'table':ref,
        'estimands':AXIS,'precision':'Float64','validityBroadcast':'SupportedSamples'}})
products=[{'id':p,'kind':k,'precision':'Float64','observations':OBS,'targets':{'$type':t,'ids':AXIS},
    'pooling':'Run','units':u} for p,k,t,u in [('effect','Effect','Scalar','signal'),
    ('scale','ResidualVariance','Scalar','signal^2'),('U','Covariance','UpperTriangle','unitless')]]
unknown={'$type':'Unknown','reason':'independent fixture'}
content={'dataset':D,'unit':UNIT,'revision':REV,'domain':{'Dimensions':[2,3,1],
    'VoxelToWorldRASMillimetres':[1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1],'WorldFrame':'scanner','Support':SUPPORT},
    'observations':[{'id':o,'participant':{'dataset':D,'label':o},'acquisitions':['run']} for o in OBS],
    'bindings':[],'products':products,'outcomes':[[p['id'],{'$type':'Available','product':p['id']}] for p in products],
    'estimability':{'$type':'Unknown','reason':'imported independent fixture'},
    'provenance':{'producer':'independent fixture','version':'1','executionId':'literal','estimator':unknown,
        'noise':unknown,'nuisance':unknown,'runCombination':unknown,'scans':[],'inputs':[]},
    'covariance':[{'product':'U','effects':'effect','equation':{'$type':'Normalized','varianceScale':'scale'},
        'invariantObservations':False,'invariantSamples':True}], 'statistics':[], 'degreesOfFreedom':[],
    'marginalUncertainty':[],'Catalog':catalog_ref,'Representations':reps,
    'Tables':{'estimands':et,'observations':ot},'ModelRevisionId':M}
manifest=save(f'{PREFIX}/estimates.json',document('unit',content,True))
expected={'upper':UPPER,'scales':SCALES,'support':SUPPORT,'estimands':AXIS,'observations':OBS,
    'SigmaUpper':[[[float(Fraction(u)*Fraction(s)) for u in UPPER[o]] for s in SCALES[o]] for o in range(2)],
    'manifest':manifest}
save('expected.json',encoded(expected))
(OUTPUT/'SHA256SUMS').write_text(''.join(f"{r['SHA256']}  {r['Path']}\n" for r in REFS))
print(json.dumps({'files':len(REFS),'manifest':manifest},ensure_ascii=False))
