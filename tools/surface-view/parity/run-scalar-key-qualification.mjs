#!/usr/bin/env node
// Supplementary public-export evidence only. It deliberately does not qualify C10.
import {spawnSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {mkdir,readFile,writeFile} from 'node:fs/promises';
import {dirname,resolve} from 'node:path';
import {fileURLToPath,pathToFileURL} from 'node:url';
import {cleanupOwned} from './resource-contracts.mjs';

const here=dirname(fileURLToPath(import.meta.url));
const arg=name=>{const index=process.argv.indexOf(name);return index<0?null:process.argv[index+1]};
const reference=resolve(arg('--reference-root')??'/private/tmp/surfviewjs-frozen-fb60f370-20261015');
const bundle=arg('--reference-bundle'),out=resolve(arg('--out')??'/private/tmp/scalar-key-qualification');
if(!bundle)throw new Error('required --reference-bundle');
const sha=value=>createHash('sha256').update(value).digest('hex');
const json=(path,value)=>writeFile(path,JSON.stringify(value,null,2)+'\n');
const audit=()=>{const value=spawnSync(process.execPath,[resolve(process.env.HOME,'.local/share/agent-policy/browser-automation-guard.mjs'),'--audit'],{encoding:'utf8'});if(![0,1].includes(value.status))throw new Error(value.stderr);return {exitCode:value.status,stdout:value.stdout,stderr:value.stderr}};
const fsUrl=path=>'/@fs/'+resolve(path);
const palette=(first,last)=>Array.from({length:256},(_,index)=>first.map((value,channel)=>Math.round(value*(1-index/255)+last[channel]*index/255)/255));
const variants=[
 {id:'baseline',range:[-4,8],threshold:[0,0],palette:palette([0,32,255],[255,96,0]),endpoints:[-4,8],thresholdEndpoints:null},
 {id:'changed',range:[-8,16],threshold:[-2,4],palette:palette([0,210,190],[255,150,20]),endpoints:[-8,16],thresholdEndpoints:[-2,4]}
];
const sizes=[{id:'manuscript-single-column',width:1200,height:1200},{id:'manuscript-double-column',width:2400,height:1400}];
// Fixed before rendering: comparison never crops, aligns, or derives masks from output.
const geometryPolicy={normalization:'none',crop:'none',alignment:'none',keyComparison:'source-derived endpoints, interior palette samples, and threshold interval',fullFrame:'retain unmodified pixels and glyph differences'};
await mkdir(out,{recursive:true});await json(resolve(out,'guard-before.json'),audit());
const provenance={referenceBundleSha256:sha(await readFile(bundle)),runnerSha256:sha(await readFile(fileURLToPath(import.meta.url))),inputMappingSha256:sha(JSON.stringify({variants,sizes,geometryPolicy})),startedAt:new Date().toISOString()};
provenance.sourceFiles={};
await mkdir(resolve(out,'runner-source'),{recursive:true});
for(const name of ['run-scalar-key-qualification.mjs','reference-view.mjs','resource-contracts.mjs','index.html']){
 const bytes=await readFile(resolve(here,name));provenance.sourceFiles[name]=sha(bytes);
 await writeFile(resolve(out,'runner-source',name),bytes);
}
let server,browser,context;
try {
 const {createServer}=await import(pathToFileURL(resolve(reference,'node_modules/vite/dist/node/index.js')));
 server=await createServer({configFile:false,root:reference,cacheDir:resolve(out,'vite-cache'),optimizeDeps:{noDiscovery:true},server:{host:'127.0.0.1',port:0,fs:{allow:[here,reference,out,dirname(resolve(bundle))]}}});await server.listen();
 const {chromium}=await import(pathToFileURL(resolve(reference,'node_modules/playwright/index.mjs')));
 browser=await chromium.launch({headless:true,args:['--use-angle=metal','--enable-gpu']});context=await browser.newContext({viewport:{width:800,height:600},deviceScaleFactor:1});const page=await context.newPage();
 await page.goto(new URL(fsUrl(resolve(here,'index.html')),server.resolvedUrls.local[0]).href);await page.evaluate(()=>document.body.replaceChildren());
 const captures=await page.evaluate(async ({referenceUrl,threeUrl,adapterUrl,variants,sizes})=>{
  const ref=await import(referenceUrl),three=await import(threeUrl),{mountReferenceView}=await import(adapterUrl);const rows=[];
  for(const variant of variants) for(const size of sizes){const root=document.createElement('div');document.body.append(root);const mounted=mountReferenceView(ref,three,root,{width:800,height:600,camera:{projection:'perspective',eye:[0,0,4],target:[0,0,0],up:[0,1,0],fovDegrees:35,near:.01,far:1000,aspect:4/3},profile:{name:'controlled-unlit',ambientIntensity:Math.PI,directionalIntensity:0},surfaces:[{id:'scalar-key-surface',vertices:[[-1,-1,0],[1,-1,0],[1,1,0],[-1,1,0]],faces:[[0,1,2],[0,2,3]],layers:[{id:'scalar-key',kind:'scalar',values:[-4,-1,2,8],range:variant.range,threshold:variant.threshold,palette:variant.palette}]}]});try{const viewer=mounted.viewer,before={width:viewer.width,height:viewer.height,renderer:[viewer.renderer.domElement.width,viewer.renderer.domElement.height]};const key=viewer.getFigureColorbarSource('scalar-key');const image=viewer.exportPNG({colorbar:true,colorbarLayer:'scalar-key',width:size.width,height:size.height,roiLabels:false,scaleBar:false});const after={width:viewer.width,height:viewer.height,renderer:[viewer.renderer.domElement.width,viewer.renderer.domElement.height]};rows.push({variant:variant.id,size,image,key,before,after,restored:JSON.stringify(before)===JSON.stringify(after)})}finally{mounted.dispose();root.remove()}}return rows;
 },{referenceUrl:fsUrl(bundle),threeUrl:fsUrl(resolve(reference,'node_modules/three/build/three.module.js')),adapterUrl:fsUrl(resolve(here,'reference-view.mjs')),variants,sizes});
 const {PNG}=(await import(pathToFileURL(resolve(reference,'node_modules/playwright-core/lib/utilsBundle.js')))).default;
 for(const row of captures)await writeFile(resolve(out,`${row.variant}-${row.size.id}-reference.png`),PNG.sync.write(PNG.sync.read(Buffer.from(row.image.split(',')[1],'base64'))));
 const css=color=>`rgb(${Math.round(color[0]*255)}, ${Math.round(color[1]*255)}, ${Math.round(color[2]*255)})`;
 for(const row of captures){const variant=variants.find(value=>value.id===row.variant);row.keyScientific={rangePass:JSON.stringify(row.key?.range)===JSON.stringify(variant.range),thresholdPass:JSON.stringify(row.key?.threshold??null)===JSON.stringify(variant.thresholdEndpoints),endpointColorsExpected:[css(variant.palette[0]),css(variant.palette.at(-1))],endpointColorsObserved:row.key?[row.key.colors[0],row.key.colors.at(-1)]:null};row.keyScientific.endpointColorsPass=JSON.stringify(row.keyScientific.endpointColorsObserved)===JSON.stringify(row.keyScientific.endpointColorsExpected);row.keyScientific.pass=row.restored&&row.keyScientific.rangePass&&row.keyScientific.thresholdPass&&row.keyScientific.endpointColorsPass}
 for(const row of captures){
  const variant=variants.find(value=>value.id===row.variant);
  row.keyScientific.interior=[63,127,191].map(index=>{
   const value=variant.range[0]+(variant.range[1]-variant.range[0])*index/255;
   const paletteIndex=Math.floor((value-variant.range[0])/(variant.range[1]-variant.range[0])*255);
   const expected=css(variant.palette[paletteIndex]),observed=row.key?.colors[index];
   return {index,value,paletteIndex,expected,observed,pass:expected===observed};
  });
  row.keyScientific.pass &&= row.keyScientific.interior.every(sample=>sample.pass);
 }
 const endpointPass=captures.every(row=>row.keyScientific.pass);
 const missingChecks=['custom-ticks','custom-key-sizes','bivariate-viewer-export','cross-renderer-calibration','independent-per-element-masks'];
 await json(resolve(out,'scalar-key-qualification.json'),{status:endpointPass?'incomplete':'fail',qualification:'supplementary-supported-public-export; C10 remains incomplete',missingChecks,geometryPolicy,provenance,variants,sizes,captures});
 if(!endpointPass)process.exitCode=1;
} finally {const failures=await cleanupOwned([['context',async()=>{if(context)await context.close()}],['browser',async()=>{if(browser)await browser.close()}],['server',async()=>{if(server)await server.close()}],['guard-after',async()=>await json(resolve(out,'guard-after.json'),audit())],['provenance',async()=>await json(resolve(out,'provenance.json'),provenance)]]);if(failures.length){await json(resolve(out,'cleanup-failures.json'),failures);process.exitCode=1}}
