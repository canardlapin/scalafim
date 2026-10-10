import { writeFileSync } from 'node:fs';
const expectedPid = Number(process.argv[2]);
const output = process.argv[3];
const targets = await (await fetch('http://127.0.0.1:9229/json/list')).json();
if (targets.length !== 1) throw new Error('Expected one local Node inspector target');
const ws = new WebSocket(targets[0].webSocketDebuggerUrl);
await new Promise((resolve, reject) => {
  ws.addEventListener('open', resolve, {once:true});
  ws.addEventListener('error', reject, {once:true});
});
let id = 0;
const pending = new Map();
ws.addEventListener('message', event => {
  const message = JSON.parse(event.data);
  if (!message.id) return;
  const entry = pending.get(message.id);
  if (!entry) return;
  pending.delete(message.id);
  if (message.error) entry.reject(message.error);
  else entry.resolve(message.result);
});
const send = (method, params={}) => new Promise((resolve,reject) => {
  const request = ++id;
  pending.set(request, {resolve,reject});
  ws.send(JSON.stringify({id:request,method,params}));
});
try {
  const identity = await send('Runtime.evaluate', {expression:'process.pid',returnByValue:true});
  if (identity.result.value !== expectedPid) throw new Error('Node PID mismatch');
  await send('Profiler.enable');
  await send('Profiler.setSamplingInterval', {interval:1000});
  await send('Profiler.start');
  console.log(`Sampling verified PHRF Node ${expectedPid} for 20 seconds`);
  await new Promise(resolve => setTimeout(resolve,20000));
  const {profile} = await send('Profiler.stop');
  await send('Profiler.disable');
  writeFileSync(output, JSON.stringify(profile));
  console.log(`Wrote ${profile.samples.length} samples to ${output}`);
} finally {
  ws.close();
}
