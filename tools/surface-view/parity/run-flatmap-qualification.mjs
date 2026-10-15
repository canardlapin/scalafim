#!/usr/bin/env node
// C11 owns its browser lifecycle.  It compares a fresh public FlatMapView with
// the actual ScalaFIM Canvas host and records the separate linked 3-D marker.
import {spawnSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {mkdir,readFile,writeFile} from 'node:fs/promises';
import {dirname,resolve} from 'node:path';
import {fileURLToPath,pathToFileURL} from 'node:url';
import {compareRgba} from './compare.mjs';
import {cleanupOwned} from './resource-contracts.mjs';

const here=dirname(fileURLToPath(import.meta.url));
const arg=name=>{const i=process.argv.indexOf(name);return i<0?null:process.argv[i+1];};
const candidateRoot=resolve(arg('--candidate-root')??resolve(here,'../../..'));
const reference=resolve(arg('--reference-root')??'/private/tmp/surfviewjs-frozen-fb60f370-20261015');
const modulePath=arg('--scalafim-module'),bundle=arg('--reference-bundle');
const out=resolve(arg('--out')??'/private/tmp/scalafim-flatmap-qualification');
const jvmDir=arg('--jvm-dir')===null?null:resolve(arg('--jvm-dir'));
if(!modulePath||!bundle)throw new Error('required --scalafim-module and --reference-bundle');
const sha=value=>createHash('sha256').update(value).digest('hex');
const writeJson=(path,value)=>writeFile(path,JSON.stringify(value,null,2)+'\n');
const command=(cmd,args,cwd)=>{const result=spawnSync(cmd,args,{cwd,encoding:'utf8'});if(result.status!==0)throw new Error(result.stderr);return result.stdout.trim();};
const audit=()=>{const result=spawnSync(process.execPath,[resolve(process.env.HOME,'.local/share/agent-policy/browser-automation-guard.mjs'),'--audit'],{encoding:'utf8'});if(![0,1].includes(result.status))throw new Error(result.stderr);return {exitCode:result.status,stdout:result.stdout,stderr:result.stderr};};
const source={surfaceId:'flatmap-square2tri',vertices:[[-1,-1,0],[1,-1,0],[-1,1,0],[1,1,0]],faces:[[0,1,2],[1,3,2]]};
const style={background:'#050505',fill:'#d9d9d9',stroke:'#888888',hover:'#58c4ff',selected:'#ffd166',roi:'#ffd166',pointRadius:4};
const polygon=[[-1,-1],[.1,-1],[.1,.1],[-1,.1]];
const lasso=[[-1.1,-1.1],[.1,-1.1],[.1,.1],[-1.1,.1],[-1.1,-1.1]];
const checkpoints=['hover','pick','polygon','lasso','linked-marker','linked-time'];
const toScreen=([x,y])=>({x:95+(x+1)*205,y:20+(1-y)*205});
const fsUrl=path=>'/@fs/'+resolve(path);
const frozenPath=resolve(here,'frozen-inputs.json');
const frozen=JSON.parse(await readFile(frozenPath,'utf8'));
if(!Array.isArray(frozen.contracts))throw new Error('frozen inputs has no contracts array');
const c11=frozen.contracts.filter(row=>row.rowId==='C11-BR-flatmap');
if(JSON.stringify(c11.map(row=>row.checkpoint))!==JSON.stringify(checkpoints))throw new Error('frozen C11 browser checkpoints drifted');
for(const row of c11){if(row.viewport.size[0]!==600||row.viewport.size[1]!==450||row.parameters.paddingPixels!==20||row.parameters.flatProjection!=='xy; contain; equal-scale'||row.parameters.triangleFill!=='#d9d9d9'||row.parameters.stroke!=='#888888')throw new Error('frozen C11 parameters drifted');}
const {PNG}=(await import(pathToFileURL(resolve(reference,'node_modules/playwright-core/lib/utilsBundle.js')))).default;
const decode=url=>PNG.sync.read(Buffer.from(url.split(',')[1],'base64'));
const nonblank=image=>{
 let nonTransparentPixels=0,nonZeroPixels=0,distinctFromFirstPixels=0;
 const first=[image.data[0],image.data[1],image.data[2],image.data[3]];
 for(let index=0;index<image.data.length;index+=4){
  if(image.data[index+3]!==0)nonTransparentPixels+=1;
  if(image.data[index]!==0||image.data[index+1]!==0||image.data[index+2]!==0||image.data[index+3]!==0)nonZeroPixels+=1;
  if(image.data[index]!==first[0]||image.data[index+1]!==first[1]||image.data[index+2]!==first[2]||image.data[index+3]!==first[3])distinctFromFirstPixels+=1;
 }
 return {firstPixel:first,nonTransparentPixels,nonZeroPixels,distinctFromFirstPixels,nonblank:(nonTransparentPixels>0||nonZeroPixels>0)&&distinctFromFirstPixels>0};
};
const plate=(left,right)=>{
 const output=new PNG({width:left.width+right.width,height:Math.max(left.height,right.height),fill:true});
 PNG.bitblt(left,output,0,0,left.width,left.height,0,0);
 PNG.bitblt(right,output,0,0,right.width,right.height,left.width,0);
 return output;
};
async function compareJvmImages(captured,browserLinked){
 const jvmReceiptPath=resolve(jvmDir,'candidate.json');
 const jvmReceipt=JSON.parse(await readFile(jvmReceiptPath,'utf8'));
 const browserSource=captured.candidate.sourceJson;
 const jvmSource=String(jvmReceipt.sourceJson);
 const rows=[];
 for(const checkpoint of checkpoints){
  const referencePath=resolve(out,`${checkpoint}-reference.png`);
  const jvmPath=resolve(jvmDir,`${checkpoint}.png`);
  const referenceImage=PNG.sync.read(await readFile(referencePath));
  const jvmImage=PNG.sync.read(await readFile(jvmPath));
  const observedComparison=compareRgba({reference:referenceImage.data,candidate:jvmImage.data,width:600,height:450,candidateWidth:jvmImage.width,candidateHeight:jvmImage.height,tolerances:{minimumForegroundIoU:.995,maximumChannelError:2,background:[5,5,5,255],backgroundTolerance:0}});
  const comparisonPass=referenceImage.width===jvmImage.width&&referenceImage.height===jvmImage.height&&Buffer.compare(referenceImage.data,jvmImage.data)===0;
  const capture=jvmReceipt.captures?.find(value=>value.checkpoint===checkpoint);
  const expectedTimepoint=checkpoint==='linked-time'?1:0;
  const jvmSemanticCapture={observed:capture?{hover:capture.hover,selection:capture.selection,timepoint:capture.timepoint,roiVertices:capture.roiVertices,route:capture.route}:null,pass:!!capture&&String(capture.hover).includes('Some(3)')&&capture.selection===0&&capture.timepoint===expectedTimepoint&&JSON.stringify(capture.roiVertices)===JSON.stringify([0]),qualification:'unqualified-static-capture; physical event replay is not evidenced'};
  const sideBySidePath=resolve(out,`${checkpoint}-jvm-reference-side-by-side.png`);
  await writeFile(sideBySidePath,PNG.sync.write(plate(referenceImage,jvmImage)));
  rows.push({checkpoint,comparisonPass,comparisonKind:'exact-byte',observedComparison,jvmSemanticCapture,endpointPass:comparisonPass&&jvmSemanticCapture.pass,reference:{path:referencePath,sha256:sha(await readFile(referencePath)),width:referenceImage.width,height:referenceImage.height},jvm:{path:jvmPath,sha256:sha(await readFile(jvmPath)),width:jvmImage.width,height:jvmImage.height},sideBySide:{path:sideBySidePath,sha256:sha(await readFile(sideBySidePath))}});
 }
 const jvmLinkedPath=resolve(jvmDir,'linked-marker-display.png');
 const jvmLinked=PNG.sync.read(await readFile(jvmLinkedPath));
 const missingChecks=['repeated-reference-calibration','reference-linked-marker-anchor-proof','physical-event-replay','independent-per-element-masks'];
 const receipt={schema:'surface-flatmap-jvm-browser-comparison.v1',status:null,qualificationStatus:'incomplete',missingChecks,jvmDir,browserReferenceDir:out,source:{browserSha256:sha(browserSource),jvmSha256:sha(jvmSource),matches:browserSource===jvmSource},browserReceiptSha256:sha(await readFile(resolve(out,'flatmap-qualification.json'))),jvmReceipt:{path:jvmReceiptPath,sha256:sha(await readFile(jvmReceiptPath))},linkedMarkerDisplays:{browser:{...browserLinked,qualification:'artifact-availability-only; not linked-marker anchor parity'},jvm:{path:jvmLinkedPath,sha256:sha(await readFile(jvmLinkedPath)),width:jvmLinked.width,height:jvmLinked.height,...nonblank(jvmLinked),qualification:'artifact-availability-only; not linked-marker anchor parity'}},rows};
 receipt.status=receipt.source.matches&&receipt.linkedMarkerDisplays.browser.nonblank&&receipt.linkedMarkerDisplays.jvm.nonblank&&rows.every(row=>row.endpointPass)?'incomplete':'fail';
 await writeJson(resolve(out,'jvm-browser-comparison.json'),receipt);
 return receipt;
}
await mkdir(out,{recursive:true});
await writeJson(resolve(out,'guard-before.json'),audit());
const provenance={frozenInputsSha256:sha(await readFile(frozenPath)),candidateCommit:command('git',['rev-parse','HEAD'],candidateRoot),referenceCommit:command('git',['rev-parse','HEAD'],reference),moduleSha256:sha(await readFile(modulePath)),referenceBundleSha256:sha(await readFile(bundle)),runnerSha256:sha(await readFile(fileURLToPath(import.meta.url))),startedAt:new Date().toISOString()};
let server,browser,context;
try{
 const {createServer}=await import(pathToFileURL(resolve(reference,'node_modules/vite/dist/node/index.js')));
 server=await createServer({configFile:false,root:reference,cacheDir:resolve(out,'vite-cache'),optimizeDeps:{noDiscovery:true},server:{host:'127.0.0.1',port:0,fs:{allow:[here,candidateRoot,reference,out,dirname(resolve(bundle)),dirname(resolve(modulePath))]}}});await server.listen();
 const {chromium}=await import(pathToFileURL(resolve(reference,'node_modules/playwright/index.mjs')));
 browser=await chromium.launch({headless:true,args:['--use-angle=metal','--enable-gpu']});context=await browser.newContext({viewport:{width:1200,height:450},deviceScaleFactor:1});const page=await context.newPage();const errors=[];
 page.on('pageerror',error=>errors.push(String(error)));page.on('console',message=>{if(message.type()==='error')errors.push(message.text());});
 await page.goto(new URL(fsUrl(resolve(here,'index.html')),server.resolvedUrls.local[0]).href);await page.evaluate(()=>{window.exports={};document.body.style.cssText='margin:0';});await page.addScriptTag({url:new URL(fsUrl(modulePath),server.resolvedUrls.local[0]).href});
 const capturedJson=await page.evaluate(async ({referenceUrl,threeUrl})=>{
  const source={surfaceId:'flatmap-square2tri',vertices:[[-1,-1,0],[1,-1,0],[-1,1,0],[1,1,0]],faces:[[0,1,2],[1,3,2]]},style={background:'#050505',fill:'#d9d9d9',stroke:'#888888',hover:'#58c4ff',selected:'#ffd166',roi:'#ffd166',pointRadius:4},polygon=[[-1,-1],[.1,-1],[.1,.1],[-1,.1]],lasso=[[-1.1,-1.1],[.1,-1.1],[.1,.1],[-1.1,.1],[-1.1,-1.1]],checkpoints=['hover','pick','polygon','lasso','linked-marker','linked-time'];const ref=await import(referenceUrl),three=await import(threeUrl),toScreen=([x,y])=>({x:95+(x+1)*205,y:20+(1-y)*205});const candidateCanvas=document.createElement('canvas'),linkedCanvas=document.createElement('canvas');document.body.append(candidateCanvas,linkedCanvas);
  let candidate;try{candidate=window.exports.captureScalafimFlatMapQualification(three,candidateCanvas,linkedCanvas);}catch(error){return `capture-error:${String(error?.stack??error)}`;}
  const captures=[];
  for(const checkpoint of checkpoints){const root=document.createElement('div');document.body.append(root);const view=new ref.FlatMapView(root,{surfaceId:source.surfaceId,vertices:source.vertices.flat(),faces:source.faces.flat()},{width:600,height:450,padding:20,fit:'contain',background:style.background,fillStyle:style.fill,strokeStyle:style.stroke,hoverStyle:style.hover,selectionStyle:style.selected,pointRadius:style.pointRadius});try{view.setHover(3);view.setSelection(0);const points=(checkpoint==='lasso'?lasso:polygon).map(toScreen);const roi=view.createROIFromPolygon(points,{name:'ROI',color:style.roi});if(checkpoint==='linked-time')view.setTime(1);view.render();captures.push({checkpoint,image:view.canvas.toDataURL('image/png'),roiVertices:roi?.vertexIndices??[],selection:view.selectedVertexIndex,timepoint:view.currentTime??0});}finally{view.dispose();root.remove();}}
  window.__c11Images={candidate:Object.fromEntries(candidate.captures.map(c=>[c.checkpoint,c.image])),reference:Object.fromEntries(captures.map(c=>[c.checkpoint,c.image])),linked:Object.fromEntries(candidate.captures.filter(c=>c.linkedDisplay).map(c=>[c.checkpoint,c.linkedDisplay]))};
  candidateCanvas.remove();linkedCanvas.remove();window.__c11Summary=JSON.stringify({candidate:{sourceJson:candidate.sourceJson,width:candidate.width,height:candidate.height,padding:candidate.padding,fit:candidate.fit,style:{background:String(candidate.style.background),fill:String(candidate.style.fill),stroke:String(candidate.style.stroke),hover:String(candidate.style.hover),selected:String(candidate.style.selected),roi:String(candidate.style.roi),pointRadius:Number(candidate.style.pointRadius)},captures:Array.from(candidate.captures,c=>({checkpoint:String(c.checkpoint),linkedDisplay:!!c.linkedDisplay,hover:String(c.hover),selection:Number(c.selection),timepoint:Number(c.timepoint),roiVertices:Array.from(c.roiVertices,Number),roiOutline:Array.from(c.roiOutline,p=>({x:Number(p.x),y:Number(p.y)})),route:String(c.route)}))},reference:captures.map(({image,...rest})=>rest)});return 'ok';
 },{referenceUrl:fsUrl(bundle),threeUrl:fsUrl(resolve(reference,'node_modules/three/build/three.module.js'))});
 if(capturedJson!=='ok')throw new Error(`C11 page setup did not complete: ${capturedJson}`);
 const captured=JSON.parse(await page.evaluate(()=>window.__c11Summary));
 const candidateSource=JSON.parse(captured.candidate.sourceJson);const candidateMesh=candidateSource.surfaces?.[0];
 if(captured.candidate.width!==600||captured.candidate.height!==450||captured.candidate.padding!==20||captured.candidate.fit!=='contain'||JSON.stringify(captured.candidate.style)!==JSON.stringify(style)||JSON.stringify(candidateMesh?.vertices)!==JSON.stringify(source.vertices)||JSON.stringify(candidateMesh?.faces)!==JSON.stringify(source.faces))throw new Error('candidate did not expose frozen C11 source or parameters');
 const rows=[];for(const checkpoint of checkpoints){const left=captured.reference.find(value=>value.checkpoint===checkpoint),right=captured.candidate.captures.find(value=>value.checkpoint===checkpoint);if(!left||!right)throw new Error('missing endpoint '+checkpoint);const images=await page.evaluate(name=>[window.__c11Images.reference[name],window.__c11Images.candidate[name]],checkpoint);const refImage=decode(images[0]),candidateImage=decode(images[1]);const observedComparison=compareRgba({reference:refImage.data,candidate:candidateImage.data,width:600,height:450,candidateWidth:candidateImage.width,candidateHeight:candidateImage.height,tolerances:{minimumForegroundIoU:.995,maximumChannelError:2,background:[5,5,5,255],backgroundTolerance:0}});await writeFile(resolve(out,`${checkpoint}-reference.png`),PNG.sync.write(refImage));await writeFile(resolve(out,`${checkpoint}-candidate.png`),PNG.sync.write(candidateImage));const expectedTimepoint=checkpoint==='linked-time'?1:0,candidateSemantics=right.hover.includes('Some(3)')&&right.selection===0&&right.timepoint===expectedTimepoint&&JSON.stringify(right.roiVertices)===JSON.stringify([0]),referenceSemantics=left.selection===0&&left.timepoint===expectedTimepoint&&JSON.stringify(left.roiVertices)===JSON.stringify([0]),comparisonPass=refImage.width===candidateImage.width&&refImage.height===candidateImage.height&&Buffer.compare(refImage.data,candidateImage.data)===0,linkedDisplayAvailable=checkpoint==='linked-marker'?right.linkedDisplay:true;rows.push({checkpoint,comparisonPass,comparisonKind:'exact-byte',observedComparison,candidateSemantics,referenceSemantics,linkedDisplayAvailable,endpointPass:comparisonPass&&candidateSemantics&&referenceSemantics&&linkedDisplayAvailable});}
 const linkedUrl=await page.evaluate(()=>window.__c11Images.linked['linked-marker']);
 if(typeof linkedUrl!=='string')throw new Error('linked-marker did not return a browser linked-display image');
 const linkedImage=decode(linkedUrl),browserLinked={path:resolve(out,'linked-marker-browser-display.png'),width:linkedImage.width,height:linkedImage.height,...nonblank(linkedImage)};
 await writeFile(browserLinked.path,PNG.sync.write(linkedImage));browserLinked.sha256=sha(await readFile(browserLinked.path));
 const missingChecks=['repeated-reference-calibration','reference-linked-marker-anchor-proof','physical-event-replay','independent-per-element-masks'];
 const endpointEvidencePass=errors.length===0&&browserLinked.nonblank&&rows.every(row=>row.endpointPass);
 const status=endpointEvidencePass?'incomplete':'fail';
 await writeJson(resolve(out,'flatmap-qualification.json'),{status,qualificationStatus:'incomplete',missingChecks,errors,provenance,c11:{source,style,polygon,lasso,roiName:'ROI',roiNamePolicy:'supplementary adapter policy; typography is not frozen'},linkedMarkerBrowser:{...browserLinked,qualification:'artifact-availability-only; not linked-marker anchor parity'},rows,candidate:{...captured.candidate,captures:captured.candidate.captures.map(({image,...rest})=>rest)},reference:captured.reference.map(({image,...rest})=>rest)});
 if(jvmDir){const jvmComparison=await compareJvmImages(captured,browserLinked);if(jvmComparison.status!=='pass')process.exitCode=1;}
 if(status!=='pass')process.exitCode=1;
}finally{const cleanupFailures=await cleanupOwned([['context',async()=>{if(context)await context.close();}],['browser',async()=>{if(browser)await browser.close();}],['server',async()=>{if(server)await server.close();}],['guard-after',async()=>await writeJson(resolve(out,'guard-after.json'),audit())],['provenance',async()=>await writeJson(resolve(out,'provenance.json'),provenance)]]);if(cleanupFailures.length>0){await writeJson(resolve(out,'cleanup-failures.json'),cleanupFailures);process.exitCode=1;}}
