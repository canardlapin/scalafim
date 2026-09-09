from controls import CORRECTED
import pathlib,json
P=pathlib.Path(__file__).resolve().parent
rows=[json.loads(x) for x in (P/'pilot.jsonl').read_text().splitlines()]
assert len(rows)==41
selected=[];reasons=[]
def add(row,reason):
    cell=row['cell']
    if cell not in selected:selected.append(cell)
    reasons.append(dict(cell=cell,reason=reason))
for method in CORRECTED:
    for family in ('normal','skew'):
        row=max((r for r in rows if r['cell']['error']==family),key=lambda r:r['results']['null'][method]/r['studies'])
        add(row,method+' maximum pilot null rate, '+family)
    row=min((r for r in rows if r['cell']['error']=='normal'),key=lambda r:r['results']['power'][method]/r['studies'])
    add(row,method+' minimum pilot normal nominal-threshold power')
for n,group,profile,family in [(20,'quarter','reverse','normal'),(8,'balanced','reverse','normal'),(80,'quarter','reverse','skew'),(20,'balanced','flat','normal')]:
    add(next(r for r in rows if r['cell']==dict(n=n,group=group,nuisance='smooth',variance=profile,error=family,tau2=0.)), 'mandatory control or previous failure')
assert len(selected)<=16
(P/'confirmation-cells.json').write_text(json.dumps(selected,indent=2));(P/'confirmation-selection.json').write_text(json.dumps(reasons,indent=2));print('Confirmation cells:',len(selected));print(json.dumps(selected,indent=2))
