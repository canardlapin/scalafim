#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { mkdir, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const arg = name => { const i = process.argv.indexOf(name); return i < 0 ? null : process.argv[i + 1]; };
const modulePath = arg('--scalafim-module');
const out = resolve(arg('--out') ?? '/private/tmp/scalafim-three-depth-policy');
const reference = '/Users/bbuchsbaum/code/jscode/surfviewjs';
if (!modulePath) throw new Error('missing --scalafim-module');
const guard = resolve(process.env.HOME, '.local/share/agent-policy/browser-automation-guard.mjs');
const audit = () => {
  const result = spawnSync(process.execPath, [guard, '--audit'], { encoding: 'utf8' });
  if (![0, 1].includes(result.status)) throw new Error(`browser audit failed: ${result.stderr}`);
  return { exitCode: result.status, stdout: result.stdout, stderr: result.stderr };
};
const fsUrl = path => `/@fs/${resolve(path)}`;
await mkdir(out, { recursive: true });
await writeFile(resolve(out, 'guard-before.json'), `${JSON.stringify(audit(), null, 2)}\n`);
let server, browser, context;
try {
  const { createServer } = await import(pathToFileURL(resolve(reference, 'node_modules/vite/dist/node/index.js')));
  const { chromium } = await import(pathToFileURL(resolve(reference, 'node_modules/playwright/index.mjs')));
  server = await createServer({ configFile: false, root: reference, optimizeDeps: { noDiscovery: true },
    server: { host: '127.0.0.1', port: 0, fs: { allow: [here, reference, dirname(resolve(modulePath))] } } });
  await server.listen();
  browser = await chromium.launch({ headless: true, args: ['--use-angle=metal', '--enable-gpu'] });
  context = await browser.newContext({ viewport: { width: 360, height: 240 }, deviceScaleFactor: 1 });
  const page = await context.newPage();
  await page.goto(new URL(fsUrl(resolve(here, 'index.html')), server.resolvedUrls.local[0]).href);
  await page.evaluate(() => { window.exports = {}; document.body.style.cssText = 'margin:0'; });
  await page.addScriptTag({ url: new URL(fsUrl(modulePath), server.resolvedUrls.local[0]).href });
  const proof = await page.evaluate(async threeUrl => {
    const three = await import(threeUrl);
    const canvas = document.createElement('canvas'); document.body.append(canvas);
    return window.exports.runScalafimThreeDepthPolicyProbe(three, canvas);
  }, fsUrl(resolve(reference, 'node_modules/three/build/three.module.js')));
  const close = (a, b, tolerance = 1e-5) => Math.abs(a - b) <= tolerance;
  const checks = proof.captures.map(c => {
    const source = c.sourceProjection;
    const legacy = c.legacyReceipts?.[0]?.matrix;
    const composed = c.receipts?.[0]?.matrix;
    const preserved = c.policy === 'PreserveDepthProjection';
    const matrixPolicy = legacy && composed && [legacy, composed].every(matrix =>
      matrix.length === 16 && matrix.every(Number.isFinite) && (preserved
        ? source.every((value, i) => close(matrix[i], value))
        : [0, 1, 4, 5, 8, 9, 12, 13, 15].every(i => close(matrix[i], source[i])) &&
          [10, 14].some(i => !close(matrix[i], source[i]))));
    const scientific = c.legacyPick?.surface === 'depth-plane' && c.legacyPick.face === 0 &&
      c.legacyPick.vertex === 1 && c.legacyPick.world.every(Number.isFinite) &&
      close(c.legacyPick.world[0], 0) && close(c.legacyPick.world[1], 0) && close(c.legacyPick.world[2], 0);
    const translated = c.compositionPick?.surface === 'depth-plane' && c.compositionPick.face === 0 &&
      c.compositionPick.vertex === 2 && c.compositionPick.world.every(Number.isFinite) &&
      close(c.compositionPick.world[0], -.4) && close(c.compositionPick.world[1], 0) && close(c.compositionPick.world[2], 0);
    return c.legacyDraws > 0 && c.compositionDraws > 0 && matrixPolicy && scientific && translated;
  });
  const status = checks.every(Boolean) && proof.captures.length === 8 ? 'pass' : 'fail';
  await writeFile(resolve(out, 'depth-policy-proof.json'), `${JSON.stringify({ status, checks, proof }, null, 2)}\n`);
  if (status !== 'pass') process.exitCode = 1;
} finally {
  if (context) await context.close();
  if (browser) await browser.close();
  if (server) await server.close();
  await writeFile(resolve(out, 'guard-after.json'), `${JSON.stringify(audit(), null, 2)}\n`);
}
