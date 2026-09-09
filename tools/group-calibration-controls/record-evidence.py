"""Record exact artifact, runtime, source and review identities after human/agent inspection."""
import pathlib,hashlib,json,subprocess,platform,sys,datetime,importlib.metadata
from PIL import Image
P=pathlib.Path(__file__).resolve().parent
ROOT=pathlib.Path('/Users/bbuchsbaum/code/scala/scalafim')
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
# Restore superseded recipes from the exact recorded transformations.
source=(P/'plot.py').read_text()
v2=source.replace("fmt={'native_PM_mKH':'o-','plugin_GLS_t':'s--','plugin_HC3_Satt':'D-.','oracle_WLS_t':'^:'}[m]","fmt='o-'")
v1=v2.replace(",size=12,y=1.10,pad=8)",",size=12)").replace('Feasible precision weighting: Gaussian evidence and the cost of noisy weights','Feasible precision weighting: good Gaussian calibration, visible power costs')
v1=v1.replace("d=r['methods'][m];power=100*d['powerAtNominalAlpha'];lo,hi=np.array(d['powerCI95'])*100\n            axs[row,1].errorbar(power,i+off,xerr=[[power-lo],[hi-power]],fmt=marker,color=col,ms=4.5,capsize=2,lw=1.0)","axs[row,1].plot(100*r['methods'][m]['powerAtNominalAlpha'],i+off,marker,color=col,ms=4.5)")
v1=v1.replace('Bars: pointwise 95% binomial intervals. Power uses','Left bars: pointwise 95% binomial intervals. Power uses')
(P/'review-v1-plot.py').write_text(v1);(P/'review-v2-plot.py').write_text(v2)
records=[]
for f in sorted([*P.glob('*.png'),*P.glob('*.svg')]):
    stem=f.stem.removesuffix('-svg-render');old=stem.startswith('review-');is_first=stem.startswith('review-v1');overview='overview' in stem
    recipe='review-v1-plot.py' if is_first else 'review-v2-plot.py' if old else 'plot.py'
    if is_first and overview:
        critique='Title and subtitle overlap in both panels, a hard layout failure; preserve and supersede.';ratings=dict(scientificTruth=4,explanatoryClarity=3,composition=2,typographyAxes=2,colorMarks=3,densityComparison=4,layoutAccessibility=2)
    elif old and overview:
        critique='Subtitle collision fixed; strengthen non-color distinctions among the four right-panel series before acceptance.';ratings=dict(scientificTruth=4,explanatoryClarity=4,composition=4,typographyAxes=4,colorMarks=3,densityComparison=4,layoutAccessibility=3)
    elif old:
        critique='Readable aligned facets, but the headline overstates calibration and power lacks uncertainty bars; revise both.';ratings=dict(scientificTruth=3,explanatoryClarity=3,composition=4,typographyAxes=4,colorMarks=4,densityComparison=4,layoutAccessibility=4)
    elif overview:
        critique='Separated title/subtitle hierarchy, readable method labels and uncertainty. Gaussian/skew pairs and variance-information curves reveal the different failures. Shape and line patterns supplement color; controls and nominal target are labeled. No clipping at the inspected export size.';ratings=dict(scientificTruth=4,explanatoryClarity=4,composition=4,typographyAxes=4,colorMarks=4,densityComparison=4,layoutAccessibility=4)
    else:
        critique='All 24 cells visible on aligned axes with consistent row order. Power and null intervals remain distinguishable; small-sample conservatism is apparent. Restrained two-color shapes and Gaussian-specific headline avoid a universal claim. Dense rows remain legible at the declared full export size.';ratings=dict(scientificTruth=4,explanatoryClarity=4,composition=4,typographyAxes=4,colorMarks=4,densityComparison=4,layoutAccessibility=4)
    render=f if f.suffix=='.png' else P/(f.stem+'-svg-render.png')
    records.append(dict(plotId=stem,family='scientific-validation',probe='controlled-group-calibration',runId='2026-09-08-controls',artifact=f.name,sha256=sha(f),dimensions=list(Image.open(render).size),renderTarget='Matplotlib Agg PNG at 170 dpi' if f.suffix=='.png' and not f.stem.endswith('-svg-render') else 'Matplotlib SVG rendered through CairoSVG 2.9.1 at scale 1.5',
        renderedArtifact=render.name,renderedSha256=sha(render),recipe=recipe,recipeSha256=sha(P/recipe),dataSha256=sha(P/'summary.json'),samplingUnit='independent simulated study',quantity='null rejection and/or nominal-threshold power',units='percent',interval='pointwise 95% Clopper-Pearson',
        reviewer='Codex root image-capable agent; implementer, not independent milestone reviewer',reviewRecordedAt=datetime.datetime.now(datetime.timezone.utc).isoformat(),actualImageInspected=True,critique=critique,ratings=ratings,interaction='not applicable: standalone static scientific export',disposition='revise' if old else 'pass',supersededBy=('calibration-overview'+f.suffix if overview else 'feasible-weighting'+f.suffix) if old else None,
        scope='This plot review is not native UI, a responsive-window matrix, formal CVD simulation, or full-product milestone acceptance.'))
(P/'visual-review.json').write_text(json.dumps(dict(inventory=[r['artifact'] for r in records],reviews=records,unresolvedCurrentDefects=[],recipeReconstruction='Superseded recipe source restored by reversing the recorded exact source substitutions; artifact bytes are original.'),indent=2))
compile=json.loads((P/'native-compile-original.json').read_text());snapshot={}
for name,expected in compile['sources'].items():
    f=pathlib.Path(name);assert sha(f)==expected
    snapshot[str(f.relative_to(ROOT))]=dict(sha256=expected,text=f.read_text())
(P/'native-source-snapshot.json').write_text(json.dumps(snapshot,indent=2))
classpath=[]
for entry in (P/'runtime-classpath.txt').read_text().strip().split(':'):
    q=pathlib.Path(entry)
    if q.is_file():classpath.append(dict(path=entry,sha256=sha(q),bytes=q.stat().st_size))
    elif q.is_dir():
        files=sorted(f for f in q.rglob('*') if f.is_file());lines=[str(f.relative_to(q))+'\t'+sha(f) for f in files]
        classpath.append(dict(path=entry,files=len(files),manifestSha256=hashlib.sha256('\n'.join(lines).encode()).hexdigest(),bytes=sum(f.stat().st_size for f in files)))
    else:raise FileNotFoundError(entry)
from matplotlib import font_manager
font=pathlib.Path(font_manager.findfont('DejaVu Sans'))
record=dict(nativeHead=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),sourceStatus='uncommitted candidate; exact native group sources captured',python=sys.version,platform=platform.platform(),packages={n:importlib.metadata.version(n) for n in ['numpy','scipy','matplotlib','Pillow']},rng='NumPy PCG64 with SeedSequence([roleRoot,cellIndex])',font=dict(path=str(font),sha256=sha(font)),java=subprocess.run(['/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home/bin/java','-version'],capture_output=True,text=True).stderr,runtimeClasspath=classpath,scratchBytesAtSnapshot=sum(f.stat().st_size for f in P.rglob('*') if f.is_file()),peakRss='not measured; concurrent-host times are not performance evidence')
(P/'environment.json').write_text(json.dumps(record,indent=2));print('Reviewed artifacts',len(records),'source identity',len(snapshot),'runtime entries',len(classpath))
