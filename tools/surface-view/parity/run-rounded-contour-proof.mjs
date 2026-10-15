#!/usr/bin/env node
// Bounded public OutlineLayer comparison. Full C05 qualification remains separate.
import {spawnSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {mkdir,readFile,writeFile,realpath} from 'node:fs/promises';
import {dirname,resolve} from 'node:path';
import {fileURLToPath,pathToFileURL} from 'node:url';
import {compareRgba} from './compare.mjs';
import {landmarkProjection} from './fixtures.mjs';
import {cleanupOwned} from './resource-contracts.mjs';

const here=dirname(fileURLToPath(import.meta.url));
const arg=name=>{const i=process.argv.indexOf(name);return i<0?null:process.argv[i+1]};
const reference=resolve(arg('--reference-root')??'/private/tmp/surfviewjs-frozen-fb60f370-20261015');
const modulePath=arg('--scalafim-module'),bundle=arg('--reference-bundle');
const out=resolve(arg('--out')??'/private/tmp/rounded-contour-proof');
if(!modulePath||!bundle)throw new Error('required --scalafim-module and --reference-bundle');
const sha=value=>createHash('sha256').update(value).digest('hex');
const json=(path,value)=>writeFile(path,JSON.stringify(value,null,2)+'\n');
const fsUrl=path=>'/@fs/'+resolve(path);
function audit(){
  const result=spawnSync(process.execPath,[resolve(process.env.HOME,'.local/share/agent-policy/browser-automation-guard.mjs'),'--audit'],{encoding:'utf8'});
  if(![0,1].includes(result.status))throw new Error(result.stderr||'browser audit failed');
  return {exitCode:result.status,stdout:result.stdout,stderr:result.stderr};
}
const coreRadius=4,haloRadius=8,edgeMargin=1.5;
const configurations=[
  {id:'orthographic-240',width:240,height:240,camera:{projection:'orthographic',eye:[0,0,3],target:[0,0,0],up:[0,1,0],left:-1,right:1,bottom:-1,top:1,near:.01,far:10,aspect:1}},
  {id:'perspective-480',width:480,height:320,camera:{projection:'perspective',eye:[0,0,3],target:[0,0,0],up:[0,1,0],fovDegrees:35,near:.01,far:10,aspect:1.5}}
];
function rgba(count){return {id:'blue',kind:'rgba',values:Array.from({length:count},()=>[0,0,1,1])}}
function plane(id,x0,z){
  return {id,hemisphere:'left',vertices:[[x0,-2,z],[2,-2,z],[2,2,z],[x0,2,z]],faces:[[0,1,2],[0,2,3]],layers:[rgba(4)]};
}
function specimen(configuration,kind){
  const depths=kind==='single'?[.1]:[.1,.2];
  const vertices=depths.flatMap(z=>[[-.15,0,z],[.15,0,z],[-.15,.7,z]]);
  const faces=depths.map((_,i)=>[3*i,3*i+1,3*i+2]);
  const labels=depths.flatMap(()=>[0,1,0]);
  const surfaces=[plane('background',-2,-1),{id:'contour',hemisphere:'left',vertices,faces,layers:[rgba(vertices.length),
    {id:'boundaries',kind:'outline',labels,color:0,width:8,halo:true,haloColor:0xffffff,haloWidth:8,offset:.3,mode:'rounded-strokes',haloOpacity:.6}]}];
  if(kind==='occluded')surfaces.push(plane('occluder',0,.9));
  return {...configuration,kind,id:configuration.id+'-'+kind,profile:{name:'controlled-unlit',ambientIntensity:Math.PI,directionalIntensity:0},surfaces};
}
const specimens=configurations.flatMap(c=>['single','overlap','occluded'].map(kind=>specimen(c,kind)));
function segmentDistance(x,y,a,b){
  const dx=b[0]-a[0],dy=b[1]-a[1],length=dx*dx+dy*dy;
  if(!(length>0))throw new Error('degenerate projected contour segment');
  const t=((x-a[0])*dx+(y-a[1])*dy)/length,clamped=Math.max(0,Math.min(1,t));
  return {distance:Math.hypot(x-a[0]-clamped*dx,y-a[1]-clamped*dy),t};
}
// The oracle uses only source endpoints, camera equations, fixed pixel radii,
// and source-over alpha. The edge exclusion is frozen before any capture.
function masksFor(spec){
  const source=spec.surfaces.find(s=>s.id==='contour');
  const edges=source.faces.flatMap(([a,b,c])=>[[a,b],[b,c]]).map(([a,b])=>[a,b].map(i=>{
    const point=[source.vertices[i][0],source.vertices[i][1],source.vertices[i][2]+.3];
    return landmarkProjection(point,spec.camera).map((v,j)=>v*[spec.width,spec.height][j]);
  }));
  const masks=Object.fromEntries(['core','halo-flank','endpoint-cap','overlap','occlusion'].map(id=>[id,[]]));
  for(let y=0;y<spec.height;y++)for(let x=0;x<spec.width;x++){
    const distances=edges.map(([a,b])=>segmentDistance(x+.5,y+.5,a,b));
    if(distances.some(d=>Math.abs(d.distance-coreRadius)<edgeMargin||Math.abs(d.distance-haloRadius)<edgeMargin))continue;
    const halos=distances.filter(d=>d.distance<haloRadius).length;
    if(!halos)continue;
    const covered=spec.kind==='occluded'&&x+.5>=spec.width/2;
    const core=distances.some(d=>d.distance<coreRadius);
    const alpha=1-Math.pow(.4,halos);
    const expected=covered?[0,0,255,255]:[Math.round(alpha*255),Math.round(alpha*255),core?Math.round(alpha*255):255,255];
    const sample={pixel:y*spec.width+x,expected};
    if(covered){if(x>=spec.width/2+3)masks.occlusion.push(sample);continue;}
    if(core)masks.core.push(sample);else masks['halo-flank'].push(sample);
    if(distances[0].t<0&&distances[0].distance<haloRadius)masks['endpoint-cap'].push(sample);
    if(halos>=2)masks.overlap.push(sample);
  }
  const required=['core','halo-flank','endpoint-cap',...(spec.kind==='single'?[]:['overlap']),...(spec.kind==='occluded'?['occlusion']:[])];
  for(const id of required)if(masks[id].length<8)throw new Error('degenerate oracle mask '+spec.id+'/'+id);
  return {edges,masks,required};
}
function checkPixels(image,oracle,mutation=false){
  return Object.fromEntries(oracle.required.map(id=>{
    let maximumError=0,failedPixels=0;
    for(const sample of oracle.masks[id]){
      const actual=mutation?(id==='occlusion'?[255,255,255,255]:[0,0,255,255]):image.data.subarray(sample.pixel*4,sample.pixel*4+4);
      const error=Math.max(...sample.expected.map((value,i)=>Math.abs(value-actual[i])));
      if(!Number.isFinite(error))throw new Error('invalid oracle pixel value');
      maximumError=Math.max(maximumError,error);if(error>2)failedPixels++;
    }
    return [id,{pixels:oracle.masks[id].length,maximumError,failedPixels,pass:failedPixels===0}];
  }));
}
function validateImage(image,spec){
  if(image.width!==spec.width||image.height!==spec.height||image.data.length!==spec.width*spec.height*4)
    throw new Error('image dimensions or RGBA length differ from specimen '+spec.id);
  return image;
}
const shapeControl={id:'shape-control',width:2,height:2};
for(const image of [{width:1,height:4,data:new Uint8Array(16)},{width:2,height:2,data:new Uint8Array(12)}]){
  let rejected=false;try{validateImage(image,shapeControl)}catch{rejected=true}
  if(!rejected)throw new Error('malformed image control was not rejected');
}
const oracles=specimens.map(masksFor);
if(!oracles.every(oracle=>Object.values(checkPixels(null,oracle,true)).every(check=>!check.pass)))throw new Error('missing-contour/occlusion mutation was not detected');
await mkdir(out,{recursive:true});
const provenance={startedAt:new Date().toISOString(),moduleSha256:sha(await readFile(modulePath)),referenceBundleSha256:sha(await readFile(bundle)),runnerSha256:sha(await readFile(fileURLToPath(import.meta.url))),browserProfile:'lean-browser-playwright',auditBefore:audit()};
provenance.sourceFiles={};
await mkdir(resolve(out,'runner-source'),{recursive:true});
for(const name of ['run-rounded-contour-proof.mjs','compare.mjs','fixtures.mjs','resource-contracts.mjs','reference-view.mjs','index.html']){
  const bytes=await readFile(resolve(here,name));
  provenance.sourceFiles[name]=sha(bytes);
  await writeFile(resolve(out,'runner-source',name),bytes);
}
provenance.threeSources={};
for(const name of ['three.module.js','three.core.js']){
  const bytes=await readFile(resolve(reference,'node_modules/three/build',name));
  provenance.threeSources[name]=sha(bytes);
  await writeFile(resolve(out,'runner-source',name),bytes);
}
if(arg('--build-receipt')){
  provenance.build=JSON.parse(await readFile(arg('--build-receipt'),'utf8'));
  if(provenance.build.moduleSha256!==provenance.moduleSha256)throw new Error('module/build receipt mismatch');
}
await json(resolve(out,'inputs.json'),specimens);
await json(resolve(out,'source-oracles.json'),oracles);
provenance.inputSha256=sha(await readFile(resolve(out,'inputs.json')));
provenance.oracleSha256=sha(await readFile(resolve(out,'source-oracles.json')));
await writeFile(resolve(out,'runner.mjs'),await readFile(fileURLToPath(import.meta.url)));
let server,browser,context;
const rows=[];
try{
  const {createServer}=await import(pathToFileURL(resolve(reference,'node_modules/vite/dist/node/index.js')));
  server=await createServer({configFile:false,root:reference,cacheDir:resolve(out,'vite-cache'),optimizeDeps:{noDiscovery:true},server:{host:'127.0.0.1',port:0,fs:{allow:[here,reference,await realpath(resolve(reference,'node_modules')),out,dirname(resolve(modulePath)),dirname(resolve(bundle))]}}});
  await server.listen();
  const {chromium}=await import(pathToFileURL(resolve(reference,'node_modules/playwright/index.mjs')));
  browser=await chromium.launch({headless:true,args:['--use-angle=metal','--enable-gpu']});
  provenance.browserVersion=browser.version();
  context=await browser.newContext({viewport:{width:1600,height:1200},deviceScaleFactor:1});
  const {PNG}=(await import(pathToFileURL(resolve(reference,'node_modules/playwright-core/lib/utilsBundle.js')))).default;
  const decode=url=>PNG.sync.read(Buffer.from(url.split(',')[1],'base64'));
  for(const [index,spec] of specimens.entries()){
    const {id,kind,...input}=spec;
    const directory=resolve(out,spec.id);await mkdir(directory,{recursive:true});
    const page=await context.newPage();const errors=[];page.on('pageerror',error=>errors.push(String(error)));
    try{
      await page.goto(new URL(fsUrl(resolve(here,'index.html')),server.resolvedUrls.local[0]).href);
      await page.evaluate(()=>{window.exports={};document.body.replaceChildren()});
      await page.addScriptTag({url:new URL(fsUrl(modulePath),server.resolvedUrls.local[0]).href});
      const referenceResult=await page.evaluate(async ({refUrl,threeUrl,adapterUrl,spec})=>{
        const ref=await import(refUrl),three=await import(threeUrl),{mountReferenceView}=await import(adapterUrl);
        const root=document.createElement('div');document.body.append(root);
        const mounted=mountReferenceView(ref,three,root,spec);
        try{return Array.from({length:3},()=>mounted.capture())}
        finally{mounted.dispose();root.remove()}
      },{refUrl:fsUrl(bundle),threeUrl:fsUrl(resolve(reference,'node_modules/three/build/three.module.js')),adapterUrl:fsUrl(resolve(here,'reference-view.mjs')),spec:input});
      const referenceImages=referenceResult.map(result=>validateImage(decode(result.image),spec));
      const ref=referenceImages[0];
      const repeatExact=referenceImages.length===3&&referenceImages.every(image=>image.data.equals(ref.data));
      await json(resolve(directory,'reference-calibration.json'),{repeatExact,repetitions:3,maximumInteriorError:2,edgeMarginPixels:edgeMargin,frozenBeforeCandidate:true});
      const captured=await page.evaluate(async ({threeUrl,spec})=>{
        const three=await import(threeUrl),native=document.createElement('canvas'),raster=document.createElement('canvas');
        document.body.append(native,raster);
        try{return {...window.exports.captureScalafimQualification(three,native,raster,JSON.stringify(spec),'conditioned')}}
        catch(error){throw new Error(String(error))}
        finally{native.remove();raster.remove()}
      },{threeUrl:fsUrl(resolve(reference,'node_modules/three/build/three.module.js')),spec:input});
      const row={id:spec.id,repeatExact,errors,reference:checkPixels(ref,oracles[index]),routes:{}};
      const images=[ref];await writeFile(resolve(directory,'reference.png'),PNG.sync.write(ref));
      for(const route of ['native','raster']){
        const result=captured[route];if(result.status!=='captured')throw new Error(route+' refused: '+result.reason);
        const image=validateImage(decode(result.image),spec);images.push(image);await writeFile(resolve(directory,route+'.png'),PNG.sync.write(image));
        row.routes[route]={oracle:checkPixels(image,oracles[index]),fullFrame:compareRgba({reference:ref.data,candidate:image.data,width:ref.width,height:ref.height,candidateWidth:image.width,candidateHeight:image.height,tolerances:{colorPolicy:{calibrated:repeatExact},maximumChannelError:2}})};
      }
      const plate=new PNG({width:spec.width*3,height:spec.height});
      images.forEach((image,i)=>PNG.bitblt(image,plate,0,0,spec.width,spec.height,i*spec.width,0));
      await writeFile(resolve(directory,'side-by-side.png'),PNG.sync.write(plate));
      row.primitivePass=repeatExact&&!errors.length&&[row.reference,...Object.values(row.routes).map(route=>route.oracle)].every(checks=>Object.values(checks).every(check=>check.pass));
      row.fullFramePass=Object.values(row.routes).every(route=>route.fullFrame.pass);
      rows.push(row);
      await json(resolve(directory,'candidate.json'),{...captured,native:{...captured.native,image:undefined},raster:{...captured.raster,image:undefined}});
      await json(resolve(directory,'reference.json'),referenceResult.map(({image,...rest})=>rest));
      await json(resolve(directory,'comparison.json'),row);
    }finally{await page.close()}
  }
  const primitivePass=rows.length===specimens.length&&rows.every(row=>row.primitivePass);
  await json(resolve(out,'rounded-contour-proof.json'),{status:primitivePass?'incomplete':'fail',primitivePass,fullFramePass:rows.every(row=>row.fullFramePass),scope:'bounded rounded contour geometry and blending; full C05 matrix remains incomplete',maskPolicy:{source:'source endpoints and camera only',edgeMarginPixels:edgeMargin,maximumInteriorError:2,alignment:'none',mutationControlsPass:true},provenance,rows});
  if(!primitivePass)process.exitCode=1;
}finally{
  const failures=await cleanupOwned([['context',async()=>{if(context)await context.close()}],['browser',async()=>{if(browser)await browser.close()}],['server',async()=>{if(server)await server.close()}],['guard-after',async()=>await json(resolve(out,'guard-after.json'),audit())],['provenance',async()=>await json(resolve(out,'provenance.json'),provenance)]]);
  if(failures.length){await json(resolve(out,'cleanup-failures.json'),failures);process.exitCode=1}
}
