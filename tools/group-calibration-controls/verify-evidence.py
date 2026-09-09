"""Fail on stale/missing visual evidence, inconsistent counts or changed native source."""
import pathlib,json,hashlib
from PIL import Image
from scipy.stats import beta
P=pathlib.Path(__file__).resolve().parent
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
def main():
    v=json.loads((P/'visual-review.json').read_text());discovered={p.name for p in P.iterdir() if p.suffix in ['.png','.svg']}
    assert set(v['inventory'])==discovered
    assert len(v['reviews'])==len(discovered)==len(set(r['artifact'] for r in v['reviews']))
    assert v['unresolvedCurrentDefects']==[]
    for r in v['reviews']:
        assert r['actualImageInspected'] and r['critique'] and r['reviewer']
        assert sha(P/r['artifact'])==r['sha256']
        assert sha(P/r['recipe'])==r['recipeSha256']
        assert sha(P/'summary.json')==r['dataSha256']
        assert sha(P/r['renderedArtifact'])==r['renderedSha256']
        assert list(Image.open(P/r['renderedArtifact']).size)==r['dimensions']
        if r['disposition']=='pass':assert min(r['ratings'].values())>=4
        else:assert r['supersededBy'] in discovered
    s=json.loads((P/'summary.json').read_text());assert s['totalIndependentStudies']==642000
    for stage,num in [('pilot',41),('confirmation',8),('estimated',24),('feasible-weighting',24)]:
        assert s[stage]['cells']==num
        for row in s[stage]['rows']:
            for m,r in row['methods'].items():
                assert abs(r['coverage']+r['nullRate']-1)<1e-12
                assert 0<=r['powerAtNominalAlpha']<=1
        normals=[r for r in s[stage]['rows'] if r['cell']['error']=='normal']
        control_count=sum(sum(m in ['oracle_WLS_t','oracle_WLS_z'] for m in row['methods']) for row in normals)
        alpha=.001/control_count
        for row in normals:
            for m,r in row['methods'].items():
                if m in ['oracle_WLS_t','oracle_WLS_z']:
                    n=row['studies'];k=round(r['nullRate']*n)
                    lo=0 if k==0 else beta.ppf(alpha/2,k,n-k+1)
                    hi=1 if k==n else beta.ppf(1-alpha/2,k+1,n-k)
                    assert lo<=.05<=hi,(stage,row['cell'],m,lo,hi)
    assert len(json.loads((P/'estimated-replay-audit.json').read_text()))==24
    assert sum(r['fixtures'] for r in json.loads((P/'native-smoke.json').read_text()))==72
    print('Inventory, artifact/source hashes, all stage counts and positive controls passed.')
if __name__=='__main__':main()
