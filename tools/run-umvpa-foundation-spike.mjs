#!/usr/bin/env node
// Exact-source, isolated consumer qualification. Never modifies provider
// checkouts, publishes remotely, or relies on their uncommitted source files.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import crypto from 'node:crypto';
import {fileURLToPath} from 'node:url';
import {execFileSync, spawn} from 'node:child_process';

const repository = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const pins = {
  alder: 'c56a6b17989e77bdab8d57220fe3299fe9348e30',
  resample4s: '6bc4172a966c92f1b06811eac64ac2bada9fef9b',
  multivar: 'f74d631720d65147c51496dcbdd37c01912de1cb',
  gale: '099832ff15c8a4a8fcf3398c7b779fb4bbc12434',
  linop4s: 'fec77db060b130b3c609a43d07ff0bfb31088aae',
  locus4s: '58c9739be51345ad9adc4bc9c9e7335023254ec9',
  ravel: '9c5669399ab8e2a11402e71973dd5f1e2f2c13f4'
};
const args = process.argv.slice(2);
const valuedOptions = new Set(['--platform','--siblings','--workspace','--suite']);
for(let i=0;i<args.length;i++) {
  if(valuedOptions.has(args[i]))i++;
  else if(!['--prepare-only','--upstream'].includes(args[i]))throw Error('Unknown option '+args[i]);
}
const option = name => {
  const at = args.indexOf(name);
  if (at < 0) return undefined;
  if (!args[at + 1] || args[at + 1].startsWith('--')) throw Error('Missing value for ' + name);
  return args[at + 1];
};
const platform = option('--platform') || 'both';
if (!['jvm', 'js', 'both'].includes(platform)) throw Error('Use --platform jvm|js|both');
const suite = option('--suite');
if(suite&&!/^[A-Za-z_][A-Za-z0-9_.*]*$/.test(suite))throw Error('Use a qualified suite name or wildcard');
const siblingRoot = path.resolve(option('--siblings') || path.dirname(repository));
const existing = option('--workspace');
// Canonicalize fresh paths too: macOS /var and /private/var otherwise load
// the same sibling sbt project twice with overlapping output directories.
const workspace = fs.realpathSync(existing || fs.mkdtempSync(path.join(os.tmpdir(), 'umvpa-foundation-spike-')));
const marker = path.join(workspace, 'spike-receipt.json');
const providers = path.join(workspace, 'providers');
const spike = path.join(providers, 'consumer');
const git = (directory, ...a) => execFileSync('git', ['-C', directory, ...a], {encoding:'utf8'}).trim();

function digestFiles(directory, relativeNames) {
  const digest = crypto.createHash('sha256');
  for (const name of [...relativeNames].sort()) {
    digest.update(name); digest.update('\0');
    digest.update(fs.readFileSync(path.join(directory, name))); digest.update('\0');
  }
  return digest.digest('hex');
}
function sourceNames(directory) {
  const result=[];
  const walk=(at,prefix='')=>{
    for(const e of fs.readdirSync(at,{withFileTypes:true})) {
      if(['target','.git','.bsp','.bloop','.metals','.jvm','.js','.native'].includes(e.name))continue;
      const name=prefix+e.name;
      if(e.isDirectory())walk(path.join(at,e.name),name+'/');
      else if(e.isFile())result.push(name);
    }
  };
  walk(directory); return result;
}
function verifySnapshot(name,record) {
  const directory=path.join(providers,name);
  if(sourceNames(directory).sort().join('\n')!==[...record.files].sort().join('\n')||
     digestFiles(directory,record.files)!==record.sha256)throw Error('Provider snapshot changed: '+name);
}
let receipt;
if(existing) {
  if(!fs.existsSync(marker))throw Error('Refusing an unowned workspace: missing spike receipt');
  receipt=JSON.parse(fs.readFileSync(marker,'utf8'));
  if(receipt.repository!==repository||JSON.stringify(receipt.pins)!==JSON.stringify(pins))throw Error('Workspace/revision mismatch');
  for(const name of Object.keys(pins)) {
    const record=receipt.sources[name];
    verifySnapshot(name,record);
  }
} else {
  fs.mkdirSync(providers);
  receipt={schema:'scalafim-umvpa-spike-3',repository,pins,sources:{},runs:[],invocations:[],productionAdmission:false};
  for(const [name,revision] of Object.entries(pins)) {
    const checkout=path.join(siblingRoot,name);
    if(git(checkout,'rev-parse',revision+'^{commit}')!==revision)throw Error('Missing exact commit '+name);
    const destination=path.join(providers,name);
    fs.mkdirSync(destination);
    const archive=execFileSync('git',['-C',checkout,'archive','--format=tar',revision],{maxBuffer:256*1024*1024});
    execFileSync('tar',['-xf','-','-C',destination],{input:archive});
    const files=sourceNames(destination);
    receipt.sources[name]={tree:git(checkout,'rev-parse',revision+'^{tree}'),files,sha256:digestFiles(destination,files)};
  }
}
fs.mkdirSync(spike,{recursive:true});
const moduleRoot=path.join(repository,'modules/mvpa-foundation-spike');
for(const name of ['build.sbt','project/build.properties','project/plugins.sbt']) {
  const destination=path.join(spike,name);
  fs.mkdirSync(path.dirname(destination),{recursive:true});
  fs.copyFileSync(path.join(moduleRoot,name),destination);
}
if(fs.existsSync(path.join(moduleRoot,'shared')))fs.cpSync(path.join(moduleRoot,'shared'),path.join(spike,'shared'),{recursive:true});
// Persist exactly which live adapter bytes were exercised, including dirty
// files. This is consumer evidence, not a clean production-build attestation.
const adapterFiles=['response','locus-data'].flatMap(name=>{
  const root='modules/'+name+'/shared/src/main/scala/';
  return sourceNames(path.join(repository,root)).map(file=>root+file);
});
const adapterRoot=path.join(workspace,'adapters');
for(const name of adapterFiles) {
  const destination=path.join(adapterRoot,name);
  fs.mkdirSync(path.dirname(destination),{recursive:true});
  fs.copyFileSync(path.join(repository,name),destination);
}
if(sourceNames(adapterRoot).sort().join('\n')!==adapterFiles.sort().join('\n'))
  throw Error('Stale adapter source files: start a fresh spike workspace');
const consumerFiles=sourceNames(moduleRoot).filter(name=>name==='build.sbt'||name.startsWith('project/')||name.startsWith('shared/'));
const copiedConsumerFiles=sourceNames(spike).filter(name=>name==='build.sbt'||name.startsWith('project/')||name.startsWith('shared/'));
if(copiedConsumerFiles.sort().join('\n')!==consumerFiles.sort().join('\n'))
  throw Error('Stale consumer source files: start a fresh spike workspace');
const invocation={started:new Date().toISOString(),platform,suite:option('--suite')||null,upstream:args.includes('--upstream'),
  node:process.version,platformName:process.platform,arch:process.arch,
  repositoryHead:git(repository,'rev-parse','HEAD'),
  repositoryStatus:git(repository,'status','--porcelain','--','build.sbt','project','modules/response','modules/locus-data'),
  productionBuildSha256:digestFiles(repository,['build.sbt']),
  runnerSha256:digestFiles(repository,['tools/run-umvpa-foundation-spike.mjs']),
  adapters:{files:adapterFiles,sha256:digestFiles(adapterRoot,adapterFiles)},
  consumer:{files:consumerFiles,sha256:digestFiles(spike,consumerFiles)}};
receipt.invocations.push(invocation);
const invocationIndex=receipt.invocations.length-1;
const writeReceipt=()=>fs.writeFileSync(marker,JSON.stringify(receipt,null,2)+'\n');
writeReceipt();
process.stdout.write('Spike workspace: '+workspace+'\n');
if(args.includes('--prepare-only'))process.exit(0);

async function sbt(cwd,commands,label) {
  const log=path.join(workspace,String(receipt.runs.length).padStart(3,'0')+'-'+label+'.log');
  const output=fs.createWriteStream(log);
  const started=new Date().toISOString();
  const argv=['-batch','-J-Xmx3g','-Dsbt.task.cpus=2','-Dsbt.supershell=false',
    '-Dsbt.ivy.home='+path.join(workspace,'ivy'),
    '-Dalder.resample4s.build='+path.join(providers,'resample4s'),
    '-Dalder.gale.build='+path.join(providers,'gale'),
    '-Dalder.linop4s.build='+path.join(providers,'linop4s'),
    '-Dumvpa.providers='+providers,'-Dumvpa.adapters='+adapterRoot,...commands];
  process.stdout.write('Running '+label+'\n');
  const exitCode=await new Promise((resolve,reject)=>{
    const child=spawn('sbt',argv,{cwd,env:process.env,stdio:['ignore','pipe','pipe']});
    child.stdout.on('data',data=>{process.stdout.write(data);output.write(data);});
    child.stderr.on('data',data=>{process.stderr.write(data);output.write(data);});
    child.on('error',reject); child.on('close',resolve);
  });
  await new Promise(resolve=>output.end(resolve));
  receipt.runs.push({invocationIndex,label,cwd,argv,started,finished:new Date().toISOString(),exitCode,log});
  writeReceipt();
  if(exitCode!==0)throw Error(label+' failed; see '+log);
}
for(const p of (platform==='both'?['jvm','js']:[platform])) {
  const suffix=p==='jvm'?'JVM':'JS';
  // The Maven coordinate required by this exact Multivar revision is rebuilt
  // from the exact Gale tree into this run's PRIVATE Ivy home, not global Ivy.
  const galeVersion='0.1.0+99-099832ff-SNAPSHOT';
  const artifactRoot=path.join(workspace,'ivy/local/io.github.canardlapin',
    p==='jvm'?'gale-core_3':'gale-core_sjs1_3',galeVersion);
  const previous=receipt.publishedArtifacts?.[p];
  if(previous) {
    if(digestFiles(artifactRoot,previous.files)!==previous.sha256)throw Error('Private Gale artifact changed: '+p);
  } else {
    await sbt(path.join(providers,'gale'),[
      'set ThisBuild / version := "'+galeVersion+'"',
      'set ThisBuild / scalaVersion := "3.7.4"',
      'core'+suffix+'/publishLocal'
    ],'gale-'+p);
    const files=sourceNames(artifactRoot);
    receipt.publishedArtifacts??={};
    receipt.publishedArtifacts[p]={files,sha256:digestFiles(artifactRoot,files)};
    writeReceipt();
  }
  const filter=option('--suite');
  await sbt(spike,['spike'+suffix+(filter?'/testOnly '+filter:'/test')],'consumer-'+p);
  if(args.includes('--upstream')) {
    // Separate bounded JVMs: upstream laws supplement, never substitute for,
    // the external consumer tests above.
    for(const [provider,commands] of [
      ['gale',['core'+suffix+'/testOnly gale.linalg.LinearOperatorSuite']],
      ['multivar',['core'+suffix+'/testOnly multivar.core.SemanticAlgebraSuite']],
      ['resample4s',['core'+suffix+'/testOnly resample4s.core.AlgebraSuite resample4s.core.BackingAndPlanSuite',
        'designs'+suffix+'/testOnly resample4s.designs.GroupedOracleSuite']],
      ['alder',['data'+suffix+'/testOnly alder.data.Resample4sResamplerSuite alder.data.CrossFittedSuite']]
    ])await sbt(path.join(providers,provider),commands,'upstream-'+provider+'-'+p);
  }
}
for(const name of Object.keys(pins)) {
  const record=receipt.sources[name];
  verifySnapshot(name,record);
}
receipt.finalSourceCheck=true;
invocation.completed=true;
if(digestFiles(adapterRoot,adapterFiles)!==invocation.adapters.sha256||
   digestFiles(spike,consumerFiles)!==invocation.consumer.sha256)throw Error('Consumer input changed during execution');
writeReceipt();
process.stdout.write('Receipt: '+marker+'\n');
