#!/usr/bin/env node
// Public FlatMapView side-by-side interaction replay. Explicit reference ROI
// updates preserve the same map coordinates after resize, using its public API.
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
const out=resolve(arg('--out')??'/private/tmp/scalafim-flatmap-proof');
if(!modulePath||!bundle)throw new Error('required --scalafim-module and --reference-bundle');
const sha=v=>createHash('sha256').update(v).digest('hex');
const json=(p,v)=>writeFile(p,JSON.stringify(v,null,2)+'\n');
const audit=()=>{const r=spawnSync(process.execPath,[resolve(process.env.HOME,'.local/share/agent-policy/browser-automation-guard.mjs'),'--audit'],{encoding:'utf8'});if(![0,1].includes(r.status))throw new Error(r.stderr);return {exitCode:r.status,stdout:r.stdout,stderr:r.stderr};};
const fsUrl=p=>'/@fs/'+resolve(p);
const command=(cmd,args,cwd)=>{const r=spawnSync(cmd,args,{cwd,encoding:'utf8'});if(r.status!==0)throw new Error(r.stderr);return r.stdout.trim();};
const {PNG}=(await import(pathToFileURL(resolve(reference,'node_modules/playwright-core/lib/utilsBundle.js')))).default;
const decode=url=>PNG.sync.read(Buffer.from(url.split(',')[1],'base64'));
await mkdir(out,{recursive:true});await json(resolve(out,'guard-before.json'),audit());
const provenance={candidateCommit:command('git',['rev-parse','HEAD'],repo),referenceCommit:command('git',['rev-parse','HEAD'],reference),
 moduleSha256:sha(await readFile(modulePath)),referenceBundleSha256:sha(await readFile(bundle)),runnerSha256:sha(await readFile(fileURLToPath(import.meta.url))),
 startedAt:new Date().toISOString(),referenceResizePolicy:'public updateROI reprojects stored map-space outline after resize'};
let server,browser,context;
try{
 const {createServer}=await import(pathToFileURL(resolve(reference,'node_modules/vite/dist/node/index.js')));
 server=await createServer({configFile:false,root:reference,cacheDir:resolve(out,'vite-cache'),optimizeDeps:{noDiscovery:true},server:{host:'127.0.0.1',port:0,fs:{allow:[here,repo,reference,out,dirname(resolve(bundle)),dirname(resolve(modulePath))]}}});await server.listen();
 const {chromium}=await import(pathToFileURL(resolve(reference,'node_modules/playwright/index.mjs')));
 browser=await chromium.launch({headless:true,args:['--use-angle=metal','--enable-gpu']});
 context=await browser.newContext({viewport:{width:500,height:300},deviceScaleFactor:1});const page=await context.newPage();const errors=[];
 page.on('pageerror',error=>errors.push(String(error)));page.on('console',m=>{if(m.type()==='error')errors.push(m.text());});
 await page.goto(new URL(fsUrl(resolve(here,'index.html')),server.resolvedUrls.local[0]).href);
 await page.evaluate(()=>{window.exports={};document.body.style.cssText='margin:0;background:white';});
 await page.addScriptTag({url:new URL(fsUrl(modulePath),server.resolvedUrls.local[0]).href});
 const captured=await page.evaluate(async refUrl=>{
  const ref=await import(refUrl);const canvas=document.createElement('canvas');document.body.append(canvas);
  const candidate=window.exports.runScalafimThreeFlatMapProbe(canvas);canvas.remove();
  const root=document.createElement('div');document.body.append(root);
  const style=candidate.style;
  const view=new ref.FlatMapView(root,{surfaceId:'fixture',vertices:[0,0,0,2,0,0,2,1,0,0,1,0],faces:[0,1,2,0,2,3]},
   {width:100,height:100,padding:10,fit:'contain',background:style.background,fillStyle:style.fill,strokeStyle:style.stroke,
    hoverStyle:style.hover,selectionStyle:style.selected,pointRadius:style.pointRadius});
  const references=[];const capture=stage=>{view.render();references.push({stage,width:view.canvas.width,height:view.canvas.height,image:view.canvas.toDataURL('image/png')});};
  try{
   capture('plain');const hover=view.pickVertexAt(10,70,6);view.setHover(hover);capture('hover');view.setSelection(hover);capture('selected');
   const polygon=[{x:8,y:72},{x:92,y:72},{x:92,y:28},{x:8,y:28}];
   const roi=view.createROIFromPolygon(polygon,{name:'resize-stable',color:style.roi});capture('polygon-roi');
   view.resize(200,100);view.updateROI(roi.id,{polygon:[{x:16,y:94},{x:184,y:94},{x:184,y:6},{x:16,y:6}]});capture('resized');
   view.setTime(1);view.setSelection(2);capture('synced-time-selection');
   const lasso=view.createROIFromPolygon([{x:16,y:94},{x:104,y:94},{x:104,y:6},{x:16,y:6}],{name:'lasso-resize-stable',color:style.roi});capture('lasso-roi');
   return {candidate,references,referencePicks:{hover,polygon:roi.vertexIndices,lasso:lasso.vertexIndices,selected:2,timepoint:1}};
  }finally{view.dispose();root.remove();}
 },fsUrl(bundle));
 const rows=[];
 for(const c of captured.candidate.captures){
  const r=captured.references.find(r=>r.stage===c.stage);if(!r)throw new Error('missing reference stage '+c.stage);
  const left=decode(r.image),right=decode(c.image);const comparison=compareRgba({reference:left.data,candidate:right.data,width:r.width,height:r.height,candidateWidth:c.width,candidateHeight:c.height,
   tolerances:{minimumForegroundIoU:.995,maximumChannelError:2,background:[0,0,0,0],backgroundTolerance:0,colorPolicy:{calibrated:true}}});
  for(const [kind,img] of [['reference',left],['candidate',right]])await writeFile(resolve(out,c.stage+'-'+kind+'.png'),PNG.sync.write(img));
  const side=new PNG({width:left.width+right.width,height:left.height});PNG.bitblt(left,side,0,0,left.width,left.height,0,0);PNG.bitblt(right,side,0,0,right.width,right.height,left.width,0);
  await writeFile(resolve(out,c.stage+'-side-by-side.png'),PNG.sync.write(side));rows.push({stage:c.stage,comparison});
 }
 const same=(a,b)=>JSON.stringify(a)===JSON.stringify(b);
 const interactions={polygon:same(captured.candidate.polygonRoiVertices,captured.referencePicks.polygon),lasso:same(captured.candidate.lassoRoiVertices,captured.referencePicks.lasso),
  selection:captured.candidate.selection===2,timepoint:captured.candidate.timepoint===1,resize:same(captured.candidate.roiOutlineAfterResize,{x:16,y:94})};
 const status=errors.length===0&&rows.every(r=>r.comparison.pass)&&Object.values(interactions).every(Boolean)?'pass':'fail';
 await json(resolve(out,'flatmap-proof.json'),{status,errors,interactions,rows,candidate:{...captured.candidate,captures:undefined},reference:captured.referencePicks});
 await writeFile(resolve(out,'index.html'),'<!doctype html><meta charset="utf-8"><title>Flat-map display comparison</title><style>body{font:16px system-ui;margin:2em;max-width:900px}img{border:1px solid #999;image-rendering:auto}pre{white-space:pre-wrap}</style><h1>Linked flat maps</h1><p>SurfViewJS left / ScalaFIM right. Identical source geometry and styles; reference ROI updated through its public API on resize.</p>'+rows.map(r=>'<h2>'+r.stage+'</h2><img src="'+r.stage+'-side-by-side.png"><pre>'+JSON.stringify(r.comparison,null,2)+'</pre>').join(''));
 console.log(JSON.stringify({status,errors,interactions,rows}));if(status!=='pass')process.exitCode=1;
}finally{if(context)await context.close();if(browser)await browser.close();if(server)await server.close();await json(resolve(out,'guard-after.json'),audit());await json(resolve(out,'provenance.json'),provenance);}
