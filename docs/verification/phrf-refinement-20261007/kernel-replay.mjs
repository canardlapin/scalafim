import {writeFileSync} from 'node:fs';
import {performance} from 'node:perf_hooks';
function $n(x) { if (x === null) throw new Error('null'); return x; }
class CheckedArray {
  constructor(data) { this.u=data; }
  get(i) { if ((i>>>0)>=(this.u.length>>>0)) throw new RangeError(); return this.u[i]; }
  set(i,v) { if ((i>>>0)>=(this.u.length>>>0)) throw new RangeError(); this.u[i]=v; }
}
const pairModule={pairIndex__I__I__I:(p,q)=>((q*(q+1)/2)|0)+p};
function $m_Lscalafim_fmri_fit_profile_TrialBandedPreparation$() {return pairModule;}
function $p_Lscalafim_fmri_fit_profile_TrialBandedObjective$__addBlock$1__I__AD__Lscalafim_fmri_fit_profile_TrialBandedPreparation__I__I__I__D__V($thiz, bandSize$1, aJets$2, prep$1, component, p, q, weight) {
  if ((weight !== 0.0)) {
    var block = Math.imul($m_Lscalafim_fmri_fit_profile_TrialBandedPreparation$().pairIndex__I__I__I(((p < q) ? p : q), ((p > q) ? p : q)), bandSize$1);
    var target = Math.imul(component, bandSize$1);
    var i = 0;
    while ((i < bandSize$1)) {
      var i$7 = ((target + i) | 0);
      $n(aJets$2).set(i$7, ($n(aJets$2).get(i$7) + (weight * $n($n(prep$1).Lscalafim_fmri_fit_profile_TrialBandedPreparation__f_gramBlocksData).get(((block + i) | 0)))));
      i = ((1 + i) | 0);
    }
  }
}
function $p_Lscalafim_fmri_fit_profile_TrialBandedObjective$__bandMatrixProduct$1__Lscalafim_fmri_fit_profile_TrialBandedPreparation__I__I__AD__I__I__AD__I__I__I__I__D($thiz, prep$2, n$1, k$1, aJets$3, bandSize$2, width$2, wcJets$2, component, solutionComponent, row, col) {
  var y = ((row - $n(prep$2).Lscalafim_fmri_fit_profile_TrialBandedPreparation__f_bandwidth) | 0);
  var from = ((y < 0) ? 0 : y);
  var x = ((n$1 - 1) | 0);
  var y$1 = ((row + $n(prep$2).Lscalafim_fmri_fit_profile_TrialBandedPreparation__f_bandwidth) | 0);
  var until = ((x < y$1) ? x : y$1);
  var solutionBase = Math.imul(Math.imul(solutionComponent, n$1), k$1);
  var sum = 0.0;
  var trial = from;
  while ((trial <= until)) {
    var y$2 = trial;
    var high = ((row > y$2) ? row : y$2);
    var x$1 = ((row - trial) | 0);
    var sign = (x$1 >> 31);
    var delta = (((x$1 ^ sign) - sign) | 0);
    sum = (sum + ($n(aJets$3).get(((((Math.imul(component, bandSize$2) + Math.imul(high, width$2)) | 0) + delta) | 0)) * $n(wcJets$2).get(((((solutionBase + Math.imul(trial, k$1)) | 0) + col) | 0))));
    trial = ((1 + trial) | 0);
  }
  return sum;
}

const n=300,bw=40,width=bw+1,k=9,rank=10,components=10,bandSize=n*width;
let seed=1;
function randomArray(length) {
 const a=new Float64Array(length);
 for(let i=0;i<length;i++){ seed=(Math.imul(seed,1664525)+1013904223)|0; a[i]=(seed>>>0)/4294967296-.5; }
 return a;
}
const gram=randomArray(55*bandSize),a=new Float64Array(components*bandSize),weights=randomArray(550);
const wc=randomArray(components*n*k),bands=randomArray(components*bandSize),out=new Float64Array(n*k);
const wa=new CheckedArray(a),wg=new CheckedArray(gram),ww=new CheckedArray(wc),wb=new CheckedArray(bands);
const prep={Lscalafim_fmri_fit_profile_TrialBandedPreparation__f_gramBlocksData:wg,
 Lscalafim_fmri_fit_profile_TrialBandedPreparation__f_bandwidth:bw};
function addBaseline(){
 a.fill(0); let pair=0;
 for(let q=0;q<rank;q++)for(let p=0;p<=q;p++,pair++)for(let c=0;c<components;c++)
  $p_Lscalafim_fmri_fit_profile_TrialBandedObjective$__addBlock$1__I__AD__Lscalafim_fmri_fit_profile_TrialBandedPreparation__I__I__I__D__V(null,bandSize,wa,prep,c,p,q,weights[c*55+pair]);
 return a[12345];
}
function addDirect(){
 a.fill(0);
 for(let pair=0;pair<55;pair++)for(let c=0;c<components;c++){
  const w=weights[c*55+pair], src=pair*bandSize,dst=c*bandSize;
  for(let i=0;i<bandSize;i++)a[dst+i]+=w*gram[src+i];
 }
 return a[12345];
}
function bandBaseline(){
 for(let row=0;row<n;row++)for(let col=0;col<k;col++)
  out[row*k+col]=$p_Lscalafim_fmri_fit_profile_TrialBandedObjective$__bandMatrixProduct$1__Lscalafim_fmri_fit_profile_TrialBandedPreparation__I__I__AD__I__I__AD__I__I__I__I__D(null,prep,n,k,wb,bandSize,width,ww,2,1,row,col);
 return out[1234];
}
function bandDirect(){
 for(let row=0;row<n;row++)for(let col=0;col<k;col++){
  let sum=0;
  for(let trial=Math.max(0,row-bw);trial<=Math.min(n-1,row+bw);trial++)
   sum+=bands[2*bandSize+Math.max(row,trial)*width+Math.abs(row-trial)]*wc[n*k+trial*k+col];
  out[row*k+col]=sum;
 }
 return out[1234];
}
function bandBatched(){
 out.fill(0);
 for(let row=0;row<n;row++){
  const dst=row*k; let src=n*k+Math.max(0,row-bw)*k;
  let index=2*bandSize+row*width+Math.min(row,bw);
  for(let trial=Math.max(0,row-bw);trial<=row;trial++,index--,src+=k){
   const value=bands[index];
   for(let col=0;col<k;col++)out[dst+col]+=value*wc[src+col];
  }
  index=2*bandSize+(row+1)*width+1;
  for(let trial=row+1;trial<=Math.min(n-1,row+bw);trial++,index+=width+1,src+=k){
   const value=bands[index];
   for(let col=0;col<k;col++)out[dst+col]+=value*wc[src+col];
  }
 }
 return out[1234];
}
const methods={addBaseline,addDirect,bandBaseline,bandDirect,bandBatched};
function check(left,right,data){
 left(); const expected=data.slice(); right(); let max=0;
 for(let i=0;i<data.length;i++)max=Math.max(max,Math.abs(expected[i]-data[i]));
 if(max>1e-12)throw new Error(`Numerical difference ${max}`);
 return max;
}
const errors={addDirect:check(addBaseline,addDirect,a),bandDirect:check(bandBaseline,bandDirect,out),bandBatched:check(bandBaseline,bandBatched,out)};
let checksum=0;
for(let warm=0;warm<20;warm++)for(const f of Object.values(methods))checksum+=f();
const milliseconds=Object.fromEntries(Object.keys(methods).map(name=>[name,[]]));
for(let repeat=0;repeat<7;repeat++){
 const names=Object.keys(methods); if(repeat%2)names.reverse();
 for(const name of names){
  const start=performance.now(); for(let i=0;i<20;i++)checksum+=methods[name]();
  milliseconds[name].push((performance.now()-start)/20);
 }
}
const median=Object.fromEntries(Object.entries(milliseconds).map(([name,values])=>[name,[...values].sort((a,b)=>a-b)[3]]));
const result={format:'phrf-kernel-replay/1',qualification:'not-admitted',node:process.version,
 geometry:{n,bandwidth:bw,columns:k,basisRank:rank,jetComponents:components},
 sourceMainSha256:"e4bb47d19d14c6b00f20544748e057f0bc59f63cbc4886c7b17735678bc21a26",
 note:'Synthetic finite arrays at B0 dimensions; copied emitted fastopt kernels with equivalent checked array helpers. Same summation order. Not a fullOpt or end-to-end experiment; concurrent Scala.js test sweep active.',
 errors,milliseconds,medianMilliseconds:median,
 speedup:{addDirect:median.addBaseline/median.addDirect,bandDirect:median.bandBaseline/median.bandDirect,bandBatched:median.bandBaseline/median.bandBatched},checksum};
writeFileSync(process.argv[2],JSON.stringify(result,null,2)+'\n');
console.log(JSON.stringify(result,null,2));
