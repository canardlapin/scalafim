import test from 'node:test';
import assert from 'node:assert/strict';
import {calibrateRetainedHeap, validWarmSamples, cleanupOwned} from './resource-contracts.mjs';
test('reference drift cannot enlarge candidate memory allowance', () => {
  const leaking = Array.from({length:8}, (_,i) => ({retainedHeapBytes:(100+i)*1024*1024}));
  assert.equal(calibrateRetainedHeap(leaking).valid, false);
  const stable = leaking.map((x,i) => ({retainedHeapBytes:100*1024*1024+(i%2)*1024}));
  const budget = calibrateRetainedHeap(stable);
  assert.equal(budget.valid,true);assert.equal(budget.maximumRetainedGrowthBytes,8*1024*1024);
  assert.equal(calibrateRetainedHeap(stable.slice(1)).valid,false);
});
test('warm samples require complete finite measurements', () => {
  assert.equal(validWarmSamples([0,0,0,0,0], x=>x===0),true);
  for (const invalid of [[],[0],[0,0,0,0,NaN],[0,0,0,0,-1]]) assert.equal(validWarmSamples(invalid),false);
});
test('one cleanup failure does not suppress other owned cleanup and audit', async () => {
  const attempted=[];
  const errors=await cleanupOwned(['context','browser','server','audit'].map(name=>[name,async()=>{attempted.push(name);if(name==='context')throw new Error('injected close failure');}]));
  assert.deepEqual(attempted,['context','browser','server','audit']);assert.equal(errors.length,1);assert.equal(errors[0].name,'context');
});
