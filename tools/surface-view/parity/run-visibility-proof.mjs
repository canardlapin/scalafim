#!/usr/bin/env node
// Bounded P4 source-fixture comparison, separate from frozen cortex qualification.
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { compareRgba } from './compare.mjs';
const here=dirname(fileURLToPath(import.meta.url));
const arg=name=>{const i=process.argv.indexOf(name);return i<0?null:process.argv[i+1];};
const repo=resolve(arg('--candidate-root')??resolve(here,'../../..'));
const reference='/Users/bbuchsbaum/code/jscode/surfviewjs';
const modulePath=arg('--scalafim-module');
const out=resolve(arg('--out')??'/private/tmp/scalafim-visibility-proof');
if(!modulePath)throw new Error('missing --scalafim-module');
const sha=v=>createHash('sha256').update(v).digest('hex');
const json=(path,v)=>writeFile(path,JSON.stringify(v,null,2)+'\n');
const command=(name,args,cwd)=>{const r=spawnSync(name,args,{cwd,encoding:'utf8'});if(r.status!==0)throw new Error(r.stderr||r.stdout);return r.stdout.trim();};
const audit=()=>{const r=spawnSync(process.execPath,[resolve(process.env.HOME,'.local/share/agent-policy/browser-automation-guard.mjs'),'--audit'],{encoding:'utf8'});if(![0,1].includes(r.status))throw new Error(r.stderr);return {exitCode:r.status,stdout:r.stdout,stderr:r.stderr};};
const fsUrl=path=>'/@fs/'+resolve(path);
await mkdir(out,{recursive:true});
await json(resolve(out,'guard-before.json'),audit());
const provenance={purpose:'16 bounded analytic P4 specimens; not frozen cortex matrix qualification',
  candidateCommit:command('git',['rev-parse','HEAD'],repo),referenceCommit:command('git',['rev-parse','HEAD'],reference),
  candidateTrackedDiffSha256:sha(command('git',['diff','--binary','HEAD','--','modules','build.sbt'],repo)),
  moduleSha256:sha(await readFile(modulePath)),runnerSha256:sha(await readFile(fileURLToPath(import.meta.url))),
  fixtureSha256:sha(await readFile(resolve(repo,'modules/surface-view/shared/src/main/scala/scalafim/surface/view/SurfaceVisibilityFixture.scala'))),
  referenceAdapterSha256:sha(await readFile(resolve(here,'reference-view.mjs'))),startedAt:new Date().toISOString()};
if(command('git',['status','--porcelain','--','src','package.json','package-lock.json'],reference))throw new Error('reference source dirty');
const {PNG}=(await import(pathToFileURL(resolve(reference,'node_modules/playwright-core/lib/utilsBundle.js')))).default;
const decode=url=>PNG.sync.read(Buffer.from(url.split(',')[1],'base64'));
let server,browser,context;
try {
  const {createServer,build}=await import(pathToFileURL(resolve(reference,'node_modules/vite/dist/node/index.js')));
  const bundle=resolve(out,'reference-source-build');
  await build({configFile:false,root:reference,logLevel:'warn',build:{outDir:bundle,emptyOutDir:false,minify:false,
    lib:{entry:resolve(reference,'src/index.ts'),formats:['es'],fileName:()=>'reference.mjs'}}});
  provenance.referenceBundleSha256=sha(await readFile(resolve(bundle,'reference.mjs')));
  server=await createServer({configFile:false,root:reference,cacheDir:resolve(out,'vite-cache'),
    resolve:{alias:[{find:/^three$/,replacement:resolve(reference,'node_modules/three/build/three.module.js')}]},
    optimizeDeps:{noDiscovery:true},server:{host:'127.0.0.1',port:0,fs:{allow:[here,repo,reference,out,dirname(resolve(modulePath))]}}});
  await server.listen();
  const {chromium}=await import(pathToFileURL(resolve(reference,'node_modules/playwright/index.mjs')));
  browser=await chromium.launch({headless:true,args:['--use-angle=metal','--enable-gpu']});
  provenance.browserVersion=browser.version();
  context=await browser.newContext({viewport:{width:900,height:600},deviceScaleFactor:1});
  const page=await context.newPage(); const errors=[];
  page.on('pageerror',e=>errors.push(String(e)));
  page.on('console',m=>{if(m.type()==='error')errors.push(m.text());});
  await page.goto(new URL(fsUrl(resolve(here,'index.html')),server.resolvedUrls.local[0]).href);
  await page.evaluate(()=>{window.exports={};document.body.style.cssText='margin:0;background:white';});
  await page.addScriptTag({url:new URL(fsUrl(modulePath),server.resolvedUrls.local[0]).href});
  const proof=await page.evaluate(async({threeUrl,refUrl,adapterUrl})=>{
    const three=await import(threeUrl),ref=await import(refUrl),{mountReferenceView}=await import(adapterUrl);
    const canvas=document.createElement('canvas'),raster=document.createElement('canvas');
    document.body.append(canvas,raster);
    const result=window.exports.runScalafimVisibilityProbe(three,canvas,raster);
    canvas.remove();raster.remove();
    for(const c of result.captures){
      const root=document.createElement('div');document.body.append(root);
      const layers=c.curvature!==null||c.name==='bivariate-confidence'||c.name.startsWith('threshold-')?[]:[{id:'rgba',kind:'rgba',values:c.rgba,opacity:c.opacity}];
      if(c.name.startsWith('threshold-'))layers.push({id:'threshold',kind:'scalar',values:c.name==='threshold-equality'?[-.5,.5,.50001,NaN]:[-.25,.75,.75,-.25],range:[-1,1],threshold:[-.5,.5],palette:[[1,0,0],[1,0,0]],opacity:c.opacity});
      if(c.name==='plus-lighter')layers.push({id:'green',kind:'rgba',values:Array.from({length:4},()=>[0,128/255,0,1]),blendMode:'additive'});
      const spec={width:256,height:256,camera:{projection:'perspective',eye:c.eye,target:[0,0,0],up:Math.abs(c.eye[2]/Math.hypot(...c.eye))>.9?[0,1,0]:[0,0,1],fovDegrees:35,near:c.near,far:c.far,aspect:1},
        profile:{name:'controlled-lit',ambientIntensity:Math.PI*c.ambient,directionalIntensity:Math.PI*c.diffuse,direction:[0,0,1]},
        surfaces:[{id:'plane',vertices:c.vertices,faces:c.faces,layers,baseColor:c.base,
          showCurvature:c.curvature!==null,...(c.curvature!==null?{curvature:Array(4).fill(c.curvature)}:{}),
          ...(c.name==='world-clip'?{clip:{normal:[1,0,0],point:[0,0,0]}}:{})}]};
      const mounted=mountReferenceView(ref,three,root,spec);
      try{
        if(c.name==='bivariate-confidence')mounted.viewer.surfaces.get('plane').addLayer(new ref.TwoDataLayer('bivariate',[-1,-.25,.25,1],[0,.25,.75,1],null,ref.ColorMap2D.fromPreset('confidence',256),{rangeX:[-1,1],rangeY:[0,1]}));
        const capture=mounted.capture();c.reference=capture.image;c.referenceReceipt=capture.receipt;c.specification=spec;}
      finally{mounted.dispose();root.remove();}
    }
    for(const c of result.alphaCaptures){
      const root=document.createElement('div');document.body.append(root);
      const surfaces=[{id:'front-red',z:.2,rgb:[1,0,0]},{id:'back-blue',z:-.2,rgb:[0,0,1]}].map(p=>({id:p.id,
        vertices:[[-1,-1,p.z],[1,-1,p.z],[1,1,p.z],[-1,1,p.z]],faces:[[0,1,2],[0,2,3]],
        layers:[{id:p.id+'-color',kind:'rgba',values:Array.from({length:4},()=>[...p.rgb,1])}]}));
      if(c.reverse)surfaces.reverse();
      const spec={width:256,height:256,camera:{projection:'perspective',eye:[0,0,4],target:[0,0,0],up:[0,1,0],fovDegrees:35,near:.01,far:1000,aspect:1},
        profile:{name:'controlled-unlit',ambientIntensity:Math.PI,directionalIntensity:0},surfaces};
      const mounted=mountReferenceView(ref,three,root,spec);
      try{
        mounted.viewer.surfaces.get('front-red').updateConfig({alpha:c.opacity});
        const capture=mounted.capture();c.reference=capture.image;c.referenceReceipt=capture.receipt;
      }finally{mounted.dispose();root.remove();}
    }
    return result;
  },{threeUrl:fsUrl(resolve(reference,'node_modules/three/build/three.module.js')),refUrl:fsUrl(resolve(bundle,'reference.mjs')),adapterUrl:fsUrl(resolve(here,'reference-view.mjs'))});
  const cases=[];
  for(const c of proof.captures){
    const imgs=Object.fromEntries(['reference','native','raster'].map(k=>[k,decode(c[k])]));
    for(const [kind,img]of Object.entries(imgs))await writeFile(resolve(out,c.name+'-'+kind+'.png'),PNG.sync.write(img));
    const side=new PNG({width:768,height:256});
    for(const [n,img]of Object.values(imgs).entries())PNG.bitblt(img,side,0,0,256,256,n*256,0);
    await writeFile(resolve(out,c.name+'-side-by-side.png'),PNG.sync.write(side));
    const comparisons={};
    for(const name of ['native','raster']){
      const img=imgs[name]; let interiorMax=0,interiorCount=0,interiorSum=0;
      // Fixed central ROI, away from silhouette and triangle clipping edges.
      for(let y=96;y<160;y++)for(let x=c.name==='world-clip'?136:96;x<160;x++){
        const i=(y*256+x)*4;interiorCount++;
        for(let k=0;k<3;k++){const e=Math.abs(img.data[i+k]-imgs.reference.data[i+k]);interiorMax=Math.max(interiorMax,e);interiorSum+=e;}
      }
      const full=compareRgba({reference:imgs.reference.data,candidate:img.data,width:256,height:256,
        tolerances:{minimumForegroundIoU:.995,maximumChannelError:2,background:[255,255,255,255],backgroundTolerance:0,colorPolicy:{calibrated:true}}});
      comparisons[name]={full,interiorMax,interiorMean:interiorSum/(3*interiorCount),interiorPass:interiorMax<=2};
      const diff=new PNG({width:256,height:256});
      for(let i=0;i<diff.data.length;i+=4){for(let k=0;k<3;k++)diff.data[i+k]=Math.min(255,Math.abs(img.data[i+k]-imgs.reference.data[i+k])*8);diff.data[i+3]=255;}
      await writeFile(resolve(out,c.name+'-'+name+'-error-x8.png'),PNG.sync.write(diff));
    }
    cases.push({name:c.name,comparisons,picks:c.picks,renderNanos:c.renderNanos,referenceReceipt:c.referenceReceipt,specification:c.specification});
  }
  const alphaCases=[];
  for(const c of proof.alphaCaptures){
    const reference=decode(c.reference),raster=decode(c.raster);
    for(const [kind,img]of [['reference',reference],['raster',raster]])await writeFile(resolve(out,c.name+'-'+kind+'.png'),PNG.sync.write(img));
    const side=new PNG({width:512,height:256});PNG.bitblt(reference,side,0,0,256,256,0,0);PNG.bitblt(raster,side,0,0,256,256,256,0);
    await writeFile(resolve(out,c.name+'-side-by-side.png'),PNG.sync.write(side));
    const center=img=>[...img.data.subarray((128*256+128)*4,(128*256+128)*4+4)];
    const expected=c.opacity===0?[0,0,255,255]:[128,0,128,255];
    const oracle=img=>center(img).every((v,i)=>Math.abs(v-expected[i])<=1);
    alphaCases.push({name:c.name,reference:center(reference),raster:center(raster),expected,nativeRefused:c.nativeRefused,
      picked:c.picked,pass:oracle(reference)&&oracle(raster)&&c.nativeRefused&&c.picked===(c.opacity===0?'back-blue':'front-red')});
  }
  const status=alphaCases.every(c=>c.pass)&&errors.length===0&&cases.every(c=>Object.values(c.comparisons).every(v=>v.interiorPass))?'interior-pass':'fail';
  await json(resolve(out,'visibility-proof.json'),{status,scope:'Fixed central-ROI color gate only. Full-frame metrics retained independently; not cortex qualification.',errors,cases,alphaCases});
  await writeFile(resolve(out,'index.html'),'<!doctype html><meta charset="utf-8"><title>Surface visibility comparison</title><style>body{font:16px system-ui;max-width:850px;margin:2em auto}img{width:768px;max-width:100%;border:1px solid #aaa}pre{white-space:pre-wrap}</style><h1>Surface mapping and visibility</h1><p>Each row: SurfViewJS / ScalaFIM Three.js / ScalaFIM Canvas reference raster. Synthetic planes, controlled lighting. Full-frame edge differences remain recorded. This is not complete cortex parity.</p>'+cases.map(c=>'<h2>'+c.name+'</h2><img src="'+c.name+'-side-by-side.png"><pre>'+JSON.stringify(Object.fromEntries(Object.entries(c.comparisons).map(([k,v])=>[k,{interiorMax:v.interiorMax,interiorMean:v.interiorMean,full:v.full}])),null,2)+'</pre>').join(''));
  console.log(JSON.stringify({status,errors,alphaCases,cases:cases.map(c=>({name:c.name,native:c.comparisons.native.interiorMax,raster:c.comparisons.raster.interiorMax,renderMs:c.renderNanos/1e6}))}));
  if(status==='fail')process.exitCode=1;
}finally{
  if(context)await context.close();if(browser)await browser.close();if(server)await server.close();
  await json(resolve(out,'guard-after.json'),audit());await json(resolve(out,'provenance.json'),provenance);
}
