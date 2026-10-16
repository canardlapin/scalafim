#!/usr/bin/env node
// Compare actual JVM host PNGs against the already-frozen contour source masks.
import {readFile,writeFile,mkdir,copyFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {resolve} from 'node:path';
import {pathToFileURL,fileURLToPath} from 'node:url';
import {compareRgba} from './compare.mjs';

const arg=name=>{const i=process.argv.indexOf(name);if(i<0||!process.argv[i+1])throw new Error('required '+name);return resolve(process.argv[i+1])};
const referenceRoot=arg('--reference-root'),primitiveProof=arg('--primitive-proof'),cortexProof=arg('--cortex-proof');
const primitiveJvm=arg('--primitive-jvm'),cortexJvm=arg('--cortex-jvm'),out=arg('--out');
const {PNG}=(await import(pathToFileURL(resolve(referenceRoot,'node_modules/playwright-core/lib/utilsBundle.js')))).default;
const json=async path=>JSON.parse(await readFile(path));
const save=(path,value)=>writeFile(path,JSON.stringify(value,null,2)+'\n');
const sha=value=>createHash('sha256').update(value).digest('hex');
const inputBytes=await readFile(resolve(primitiveProof,'inputs.json'));
const oracleBytes=await readFile(resolve(primitiveProof,'source-oracles.json'));
const proof=await json(resolve(primitiveProof,'rounded-contour-proof.json'));
if(sha(inputBytes)!==proof.provenance?.inputSha256||sha(oracleBytes)!==proof.provenance?.oracleSha256)
  throw new Error('frozen primitive input or oracle hash mismatch');
const specs=JSON.parse(inputBytes),oracles=JSON.parse(oracleBytes);
if(specs.length!==6||oracles.length!==6)throw new Error('expected six frozen primitive cases');
if(proof.primitivePass!==true||proof.rows?.length!==6||new Set(proof.rows.map(row=>row.id)).size!==6)
  throw new Error('primitive proof was not admitted');
const calibrated=new Map(specs.map((spec,index)=>{
  const row=proof.rows.find(row=>row.id===spec.id),oracle=oracles[index];
  if(row?.repeatExact!==true||!oracle.required?.length||oracle.required.some(name=>
      row.reference?.[name]?.pass!==true||row.reference[name].pixels!==oracle.masks[name]?.length))
    throw new Error('reference repeat or source-oracle admission failed '+spec.id);
  return [spec.id,true];
}));
const rows=[];
for(const [group,source,reference] of [['primitives',primitiveJvm,primitiveProof],['cortex',cortexJvm,cortexProof]]){
  const groupOut=resolve(out,group);await mkdir(groupOut,{recursive:true});
  for(const file of ['runtime.json','results.json'])await copyFile(resolve(source,file),resolve(groupOut,file));
  const captured=await json(resolve(source,'results.json'));
  const ids=captured.cases.map(row=>row.case);
  if(ids.length!==(group==='primitives'?6:3)||new Set(ids).size!==ids.length)throw new Error('missing or duplicate '+group+' captures');
  for(const row of captured.cases){
    const id=row.case,host=row.receipt;
    const jvm=PNG.sync.read(await readFile(resolve(source,id,'raster.png')));
    const ref=PNG.sync.read(await readFile(resolve(reference,id,'reference.png')));
    const browser=PNG.sync.read(await readFile(resolve(reference,id,'raster.png')));
    const index=specs.findIndex(spec=>spec.id===id);
    if(group==='primitives'&&index<0)throw new Error('unknown primitive '+id);
    const spec=index<0?null:specs[index],oracle=index<0?null:oracles[index];
    const {id:specId,kind,...payload}=spec??{};
    const expectedInputSha256=spec?sha(JSON.stringify(payload)+'\n'):(await json(resolve(reference,id,'receipt.json'))).inputSha256;
    if(!expectedInputSha256||host.inputSha256!==expectedInputSha256)throw new Error('JVM source input mismatch '+id);
    const width=spec?.width??ref.width,height=spec?.height??ref.height;
    for(const image of [jvm,ref,browser])if(image.width!==width||image.height!==height||image.data.length!==width*height*4)throw new Error('wrong image shape '+id);
    const masks=oracle?Object.fromEntries(oracle.required.map(name=>{
      let maximumError=0,failedPixels=0;
      if(oracle.masks[name].length<8)throw new Error('degenerate mask '+name);
      for(const sample of oracle.masks[name]){
        const error=Math.max(...sample.expected.map((value,channel)=>Math.abs(value-jvm.data[sample.pixel*4+channel])));
        if(!Number.isFinite(error))throw new Error('invalid oracle pixel');
        maximumError=Math.max(maximumError,error);if(error>2)failedPixels++;
      }
      return [name,{pixels:oracle.masks[name].length,maximumError,failedPixels,pass:failedPixels===0}];
    })):null;
    const hostPass=host.status==='captured'&&host.raster.warmImagesIdentical===true&&
      host.raster.snapshotPixelsEqualPresented===true&&host.raster.resizeBackExact===true&&
      host.disposal.canvasPresentedFrameReleased===true;
    const samples=host.raster.warmSnapshotSamplesNanos;
    if(samples.length!==5||samples.some(x=>!Number.isFinite(x)||x<0))throw new Error('invalid timing samples '+id);
    const fullFrame=compareRgba({reference:ref.data,candidate:jvm.data,width,height,candidateWidth:jvm.width,candidateHeight:jvm.height,
      tolerances:{maximumChannelError:2,colorPolicy:{calibrated:calibrated.get(id)===true}}});
    const result={id,group,hostPass,inputSha256:expectedInputSha256,native:host.native,masks,
      primitivePass:masks?Object.values(masks).every(mask=>mask.pass)&&hostPass:null,
      browserRasterPixelsEqual:jvm.data.equals(browser.data),fullFrame,
      warmSnapshotMedianMs:[...samples].sort((a,b)=>a-b)[2]/1e6};
    const directory=resolve(groupOut,id);await mkdir(directory,{recursive:true});
    for(const file of ['candidate.json','raster.png'])await copyFile(resolve(source,id,file),resolve(directory,file));
    const plate=new PNG({width:width*2,height});
    PNG.bitblt(ref,plate,0,0,width,height,0,0);PNG.bitblt(jvm,plate,0,0,width,height,width,0);
    await writeFile(resolve(directory,'side-by-side.png'),PNG.sync.write(plate));
    await save(resolve(directory,'comparison.json'),result);rows.push(result);
  }
}
const crossPlatformPass=rows.every(row=>row.hostPass&&row.primitivePass!==false&&row.browserRasterPixelsEqual);
await save(resolve(out,'comparisons.json'),{schema:'rounded-contour-jvm-comparison.v1',status:crossPlatformPass?'incomplete':'fail',crossPlatformPass,
  sourceOracle:resolve(primitiveProof,'source-oracles.json'),
  sourceOracleSha256:sha(await readFile(resolve(primitiveProof,'source-oracles.json'))),
  runnerSha256:sha(await readFile(fileURLToPath(import.meta.url))),rows});
if(!crossPlatformPass)process.exitCode=1;
