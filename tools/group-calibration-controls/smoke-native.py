"""Re-execute the 72 explicit PM fixtures via the current compiled native API."""
from estimated import *

def main():
    fixtures=json.loads((P/'estimated-reference-input.json').read_text());reference=json.loads((P/'estimated-reference.json').read_text())['results']
    cp=(P/'runtime-classpath.txt').read_text().strip();reports=[]
    for n in [20,80]:
        pairs=[(f,r) for f,r in zip(fixtures,reference,strict=True) if len(f['y'])==n]
        x=np.array(pairs[0][0]['X']);assert all(np.array_equal(x,f['X']) for f,_ in pairs)
        y=np.array([f['y'] for f,_ in pairs]);v=np.array([f['vhat'] for f,_ in pairs])
        inp=P/f'smoke-{n}.input.bin';out=P/f'smoke-{n}.output.bin';write_native(inp,x,y,v)
        command=[JAVA,'-Xmx1g','-XX:ActiveProcessorCount=3','-cp',cp,'scalafim.fmri.group.NativePmCalibration',str(inp),str(out)]
        process=subprocess.run(command,text=True,capture_output=True);process.check_returncode();a=read_native(out)
        assert np.all(np.isfinite(a))
        expected=np.array([[r[k] for k in ['estimate','se','t','p','tau2']] for _,r in pairs])
        recorded=np.array([[f['native'][k] for k in ['estimate','se','t','p','tau2']] for f,_ in pairs])
        error=float(np.max(np.abs(a-expected)));repeat=float(np.max(np.abs(a-recorded)))
        assert error<1e-7 and repeat<1e-12
        reports.append(dict(n=n,fixtures=len(pairs),maxIndependentRError=error,maxHistoricalOutputDelta=repeat,command=command,stdout=process.stdout,stderr=process.stderr,inputSha256=hashlib.sha256(inp.read_bytes()).hexdigest(),outputSha256=hashlib.sha256(out.read_bytes()).hexdigest()))
    (P/'native-smoke.json').write_text(json.dumps(reports,indent=2));print('72 native fixtures passed')
if __name__=='__main__':main()
