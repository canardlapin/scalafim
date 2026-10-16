#!/usr/bin/env node
// Actual host integration replay; temporal/morph reference inputs are fixed here.
import {spawnSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {mkdir,readFile,writeFile} from 'node:fs/promises';
import {dirname,resolve} from 'node:path';
import {fileURLToPath,pathToFileURL} from 'node:url';
import {compareRgba} from './compare.mjs';
const here=dirname(fileURLToPath(import.meta.url));
const arg=name=>{const i=process.argv.indexOf(name);return i<0?null:process.argv[i+1];};
const repo=resolve(arg('--candidate-root')??resolve(here,'../../..'));
const reference='/Users/bbuchsbaum/code/jscode/surfviewjs';
const modulePath=arg('--scalafim-module'),bundle=arg('--reference-bundle');
const out=resolve(arg('--out')??'/private/tmp/scalafim-linked-proof');
if(!modulePath||!bundle)throw new Error('required --scalafim-module and --reference-bundle');
const sha=v=>createHash('sha256').update(v).digest('hex');
const json=(p,v)=>writeFile(p,JSON.stringify(v,null,2)+'\n');
const audit=()=>{const r=spawnSync(process.execPath,[resolve(process.env.HOME,'.local/share/agent-policy/browser-automation-guard.mjs'),'--audit'],{encoding:'utf8'});if(![0,1].includes(r.status))throw new Error(r.stderr);return {exitCode:r.status,stdout:r.stdout,stderr:r.stderr};};
const fsUrl=p=>'/@fs/'+resolve(p);
const {PNG}=(await import(pathToFileURL(resolve(reference,'node_modules/playwright-core/lib/utilsBundle.js')))).default;
const decode=url=>PNG.sync.read(Buffer.from(url.split(',')[1],'base64'));
await mkdir(out,{recursive:true});await json(resolve(out,'guard-before.json'),audit());
const provenance={moduleSha256:sha(await readFile(modulePath)),referenceBundleSha256:sha(await readFile(bundle)),
 runnerSha256:sha(await readFile(fileURLToPath(import.meta.url))),fixtureSha256:sha(await readFile(resolve(repo,'modules/surface-view/shared/src/main/scala/scalafim/surface/view/SurfaceLinkedFixture.scala'))),startedAt:new Date().toISOString()};
let server,browser,context;
try{
 const {createServer}=await import(pathToFileURL(resolve(reference,'node_modules/vite/dist/node/index.js')));
 server=await createServer({configFile:false,root:reference,cacheDir:resolve(out,'vite-cache'),optimizeDeps:{noDiscovery:true},
  resolve:{alias:[{find:/^three$/,replacement:resolve(reference,'node_modules/three/build/three.module.js')}]},
  server:{host:'127.0.0.1',port:0,fs:{allow:[here,repo,reference,out,dirname(resolve(bundle)),dirname(resolve(modulePath))]}}});await server.listen();
 const {chromium}=await import(pathToFileURL(resolve(reference,'node_modules/playwright/index.mjs')));
 browser=await chromium.launch({headless:true,args:['--use-angle=metal','--enable-gpu']});
 context=await browser.newContext({viewport:{width:900,height:700},deviceScaleFactor:1});const page=await context.newPage();const errors=[];
 page.on('pageerror',error=>errors.push(String(error)));page.on('console',m=>{if(m.type()==='error')errors.push(m.text());});
 await page.goto(new URL(fsUrl(resolve(here,'index.html')),server.resolvedUrls.local[0]).href);
 await page.evaluate(()=>{window.exports={};document.body.style.cssText='margin:0;background:white';});
 await page.addScriptTag({url:new URL(fsUrl(modulePath),server.resolvedUrls.local[0]).href});
 const proof=await page.evaluate(async({refUrl,threeUrl,adapterUrl})=>{
  const ref=await import(refUrl),three=await import(threeUrl),{mountReferenceView}=await import(adapterUrl);
  const canvas=document.createElement('canvas'),flat=document.createElement('canvas');document.body.append(canvas,flat);
  const candidate=window.exports.runScalafimLinkedViewProbe(three,canvas,flat);canvas.remove();flat.remove();
  const vertices=[[-1,-1,0],[1,-1,0],[1,1,0],[-1,1,0]],faces=[[0,1,2],[0,2,3]];
  const frames=[[-1,0,.25,-.5],[0,1,.75,.25],[-.25,-1,.5,1]];
  // SurfViewJS treats palette entries as a discrete lookup, not color stops.
  // Sample the same blue-to-orange ramp at its standard 256-entry resolution.
  const palette=Array.from({length:256},(_,i)=>[i/255,(32+Math.round(64*i/255))/255,(255-i)/255]);
  const eyes=[[0,0,4],[-1,.8,4],[1,.5,4],[0,-1,4]];
  for(const c of candidate.captures){
   c.references=[];
   for(const [index,originalEye] of eyes.entries()){
    const moved=index===0&&['morph-endpoint','restored','resumed'].includes(c.name);
    const eye=moved?[.15,.05,4]:originalEye,target=moved?[.15,.05,0]:[0,0,0];
    const root=document.createElement('div');document.body.append(root);
    const mounted=mountReferenceView(ref,three,root,{width:256,height:256,
     camera:{projection:'perspective',eye,target,up:[0,1,0],fovDegrees:35,near:.01,far:1000,aspect:1},
     profile:{name:'controlled-unlit',ambientIntensity:Math.PI,directionalIntensity:0},
     surfaces:[{id:'plane',vertices,faces,morph:{vertices:vertices.map(p=>p.map(v=>v*1.2)),weight:c.morph},
      layers:[{id:'effect',kind:'temporal',frames,times:[0,1,2],range:[-1,1],palette}]}]});
    try{const low=Math.floor(c.time),high=Math.min(2,low+1);mounted.time('plane','effect',low,high,c.time-low);c.references.push(mounted.capture().image);}
    finally{mounted.dispose();root.remove();}
   }
  }
  return candidate;
 },{refUrl:fsUrl(bundle),threeUrl:fsUrl(resolve(reference,'node_modules/three/build/three.module.js')),adapterUrl:fsUrl(resolve(here,'reference-view.mjs'))});
 const rows=[];
 for(const c of proof.captures){
  const ref=new PNG({width:512,height:512});for(const [i,url]of c.references.entries())PNG.bitblt(decode(url),ref,0,0,256,256,(i%2)*256,Math.floor(i/2)*256);
  const candidate=decode(c.surfaceImage),decorated=decode(c.decoratedImage);
  const comparison=compareRgba({reference:ref.data,candidate:candidate.data,width:512,height:512,
   tolerances:{minimumForegroundIoU:.995,maximumChannelError:2,background:[255,255,255,255],backgroundTolerance:0,colorPolicy:{calibrated:true}}});
  for(const [name,img] of [['reference',ref],['candidate',candidate],['markers',decorated],['flat',decode(c.flatImage)]])await writeFile(resolve(out,c.name+'-'+name+'.png'),PNG.sync.write(img));
  const plate=new PNG({width:1024,height:512});PNG.bitblt(ref,plate,0,0,512,512,0,0);PNG.bitblt(candidate,plate,0,0,512,512,512,0);
  await writeFile(resolve(out,c.name+'-side-by-side.png'),PNG.sync.write(plate));
  rows.push({name:c.name,time:c.time,morph:c.morph,selected:c.selected,readout:c.readout,markerCount:c.markerCount,cameraKeys:c.cameraKeys,comparison});
 }
 const before=proof.captures.find(c=>c.name==='morph-endpoint'),after=proof.captures.find(c=>c.name==='restored');
 const restoreExact=before.surfaceImage===after.surfaceImage&&before.decoratedImage===after.decoratedImage&&before.flatImage===after.flatImage;
 const status=errors.length===0&&restoreExact&&rows.every(row=>row.comparison.pass)?'pass':'fail';
 await json(resolve(out,'linked-proof.json'),{status,errors,restoreExact,rows});
 await writeFile(resolve(out,'playback.json'),proof.playbackDocument);await writeFile(resolve(out,'composition.json'),proof.compositionDocument);
 await writeFile(resolve(out,'index.html'),'<!doctype html><meta charset="utf-8"><title>Linked surface replay</title><style>body{font:16px system-ui;margin:2em;max-width:1100px}img{max-width:100%;border:1px solid #bbb}pre{white-space:pre-wrap}</style><h1>Linked four-view replay</h1><p>Surface plates: SurfViewJS left, ScalaFIM right. Separate ScalaFIM marker/flat-map captures prove linked selections and morph anchors; marker appearance is not part of these surface pixel comparisons.</p>'+rows.map(r=>'<h2>'+r.name+'</h2><img src="'+r.name+'-side-by-side.png"><p><img width="512" src="'+r.name+'-markers.png"><img width="200" src="'+r.name+'-flat.png"></p><pre>'+JSON.stringify(r,null,2)+'</pre>').join(''));
 console.log(JSON.stringify({status,errors,restoreExact,rows}));if(status!=='pass')process.exitCode=1;
}finally{if(context)await context.close();if(browser)await browser.close();if(server)await server.close();await json(resolve(out,'guard-after.json'),audit());await json(resolve(out,'provenance.json'),provenance);}
