#!/usr/bin/env node
// Realistic retained-resource probe. The reference calibration is completed and
// persisted before candidate cycles. This does not qualify unmeasured families.
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile, realpath } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import {calibrateRetainedHeap, validWarmSamples, cleanupOwned} from './resource-contracts.mjs';
const here = dirname(fileURLToPath(import.meta.url));
const arg = name => { const i = process.argv.indexOf(name); return i < 0 ? null : process.argv[i + 1]; };
const modulePath = resolve(arg('--module')), inputPath = resolve(arg('--input'));
const reference = resolve(arg('--reference-root')), bundle = resolve(arg('--reference-bundle'));
const out = resolve(arg('--out'));
const sha = x => createHash('sha256').update(x).digest('hex');
const json = (p,v) => writeFile(resolve(out,p), JSON.stringify(v,null,2) + '\n');
const fsUrl = p => '/@fs/' + resolve(p);
const audit = () => {
  const r = spawnSync(process.execPath,[resolve(process.env.HOME,'.local/share/agent-policy/browser-automation-guard.mjs'),'--audit'],{encoding:'utf8'});
  if (![0,1].includes(r.status)) throw new Error('browser audit unavailable');
  return {exitCode:r.status,stdout:r.stdout,stderr:r.stderr};
};
await mkdir(out,{recursive:true});
const spec = JSON.parse(await readFile(inputPath,'utf8'));
const provenance = {schema:'surface-resource-cycles.v1', startedAt:new Date().toISOString(),
  moduleSha256:sha(await readFile(modulePath)), inputSha256:sha(await readFile(inputPath)),
  referenceBundleSha256:sha(await readFile(bundle)), runnerSha256:sha(await readFile(fileURLToPath(import.meta.url))),
  adapterSha256:sha(await readFile(resolve(here,'reference-view.mjs'))),
  resourceContractsSha256:sha(await readFile(resolve(here,'resource-contracts.mjs'))),
  threeSha256:sha(await readFile(resolve(reference,'node_modules/three/build/three.module.js'))),
  auditBefore:audit(), browserProfile:'lean-browser-playwright', requestedGpu:'ANGLE Metal'};
let server,browser,context;
let originalError;
try {
  const {createServer} = await import(pathToFileURL(resolve(reference,'node_modules/vite/dist/node/index.js')));
  const {chromium} = await import(pathToFileURL(resolve(reference,'node_modules/playwright/index.mjs')));
  server = await createServer({configFile:false,root:reference,optimizeDeps:{noDiscovery:true},
    server:{host:'127.0.0.1',port:0,fs:{allow:[here,reference,await realpath(resolve(reference,'node_modules')),dirname(bundle),dirname(modulePath)]}}});
  await server.listen();
  browser = await chromium.launch({headless:true,args:['--use-angle=metal','--enable-gpu','--enable-precise-memory-info']});
  provenance.browserVersion=browser.version();
  context=await browser.newContext({viewport:{width:spec.width,height:spec.height},deviceScaleFactor:1});
  const page=await context.newPage();
  await page.goto(new URL(fsUrl(resolve(here,'index.html')),server.resolvedUrls.local[0]).href);
  await page.evaluate(()=>{window.exports={};document.body.style.cssText='margin:0';});
  await page.addScriptTag({url:new URL(fsUrl(modulePath),server.resolvedUrls.local[0]).href});
  await page.evaluate(async ({spec,refUrl,threeUrl,adapterUrl})=>{
    window.resourceInput=spec; window.resourceRef=await import(refUrl); window.resourceThree=await import(threeUrl);
    window.resourceAdapter=await import(adapterUrl);
  },{spec,refUrl:fsUrl(bundle),threeUrl:fsUrl(resolve(reference,'node_modules/three/build/three.module.js')),adapterUrl:fsUrl(resolve(here,'reference-view.mjs'))});
  const gc=await context.newCDPSession(page);
  async function cycle(route) {
    const evidence=await page.evaluate(route=>{
      const contexts=new Set(), original=HTMLCanvasElement.prototype.getContext;
      HTMLCanvasElement.prototype.getContext=function(kind,...args){const c=original.call(this,kind,...args); if(c&&['webgl','webgl2','experimental-webgl'].includes(kind))contexts.add(c); return c;};
      let result;
      try {
        if(route==='reference') {
          const root=document.createElement('div');document.body.append(root);
          const view=window.resourceAdapter.mountReferenceView(window.resourceRef,window.resourceThree,root,window.resourceInput);
          try { let capture; for(let i=0;i<5;i++)capture=view.capture(); result={renderer:capture.receipt.renderer}; }
          finally {view.dispose();root.remove();}
        } else {
          const native=document.createElement('canvas'), raster=document.createElement('canvas');document.body.append(native,raster);
          try {
            const c=window.exports.captureScalafimQualification(window.resourceThree,native,raster,JSON.stringify(window.resourceInput),'preserved');
            result={nativeStatus:c.native.status,disposal:c.disposal,resizeBackExact:c.raster.resizeBackExact,
              warmUploadBytes:c.native.warmUploadBytes,warmResourceCounts:c.native.warmResourceCounts,
              nativeSnapshotNanos:c.native.warmSnapshotSamplesNanos,rasterNanos:c.raster.warmSamplesNanos};
          } finally {native.remove();raster.remove();}
        }
        return {...result,contexts:contexts.size,lostContexts:[...contexts].filter(c=>c.isContextLost()).length,remainingCanvases:document.querySelectorAll('canvas').length};
      } finally {HTMLCanvasElement.prototype.getContext=original;}
    },route);
    await gc.send('HeapProfiler.collectGarbage');
    evidence.retainedHeapBytes=await page.evaluate(()=>performance.memory.usedJSHeapSize);
    return evidence;
  }
  const calibration=[];
  for(let i=0;i<8;i++)calibration.push(await cycle('reference'));
  const calibrated=calibrateRetainedHeap(calibration);
  const allowance=calibrated.maximumRetainedGrowthBytes;
  const budget={...calibrated, referenceCycles:calibration, candidateCycles:20, warmupCycles:5, maximumRetainedGrowthBytes:allowance,
    measurement:'After five candidate warmups, compare final five-cycle median with initial five-cycle median. Require all owned GL contexts lost, no remaining canvases, no warm uploads and stable resource count.',
    frozenBeforeCandidate:true};
  await json('calibration.json',budget);
  if (!calibrated.valid) throw new Error('reference retained-heap calibration is unstable');
  const samples=[];
  for(let i=0;i<20;i++)samples.push(await cycle('candidate'));
  const median=a=>[...a].sort((x,y)=>x-y)[Math.floor(a.length/2)];
  const growth=median(samples.slice(-5).map(x=>x.retainedHeapBytes))-median(samples.slice(5,10).map(x=>x.retainedHeapBytes));
  const checks={referenceContextsReleased:calibration.every(x=>x.contexts>0&&x.contexts===x.lostContexts&&x.remainingCanvases===0),
    candidateContextsReleased:samples.every(x=>x.contexts>0&&x.contexts===x.lostContexts&&x.remainingCanvases===0),
    hostReleased:samples.every(x=>x.disposal?.canvasPresentedFrameReleased&&x.disposal.backendResourceCount===0),
    nativeAdmitted:samples.every(x=>x.nativeStatus==='captured'),resizeBackExact:samples.every(x=>x.resizeBackExact),
    noWarmUploads:samples.every(x=>validWarmSamples(x.warmUploadBytes,n=>n===0)),
    stableWarmResources:samples.every(x=>validWarmSamples(x.warmResourceCounts)&&new Set(x.warmResourceCounts).size===1),
    boundedRetainedGrowth:growth<=allowance};
  const result={status:Object.values(checks).every(Boolean)?'pass':'fail',checks,retainedGrowthBytes:growth,budget,samples,
    limitation:'One source specimen and twenty same-page create/render/resize/dispose cycles; JS heap only; detached canvases, backing stores, GPU memory and native RSS are not measured. Unequal work per route; native resize and warmup retention are not qualified. No claim about untested families or long-run leakage.'};
  await json('resource-cycles.json',result);console.log(JSON.stringify({status:result.status,checks,retainedGrowthBytes:growth,allowanceBytes:allowance}));
  if(result.status!=='pass')process.exitCode=1;
  await gc.detach();
} catch (error) { originalError=error; provenance.failure=String(error); process.exitCode=1; }
finally {
  const cleanupFailures=await cleanupOwned([
    ['context',async()=>{if(context)await context.close();}],
    ['browser',async()=>{if(browser)await browser.close();}],
    ['server',async()=>{if(server)await server.close();}],
    ['audit',async()=>{provenance.auditAfter=audit();}]
  ]);
  provenance.finishedAt=new Date().toISOString();provenance.cleanupFailures=cleanupFailures;
  if(cleanupFailures.length) process.exitCode=1;
  await json('provenance.json',provenance);
  if(originalError) throw originalError;
  if(cleanupFailures.length) throw new Error('owned resource cleanup failed: '+JSON.stringify(cleanupFailures));
}
