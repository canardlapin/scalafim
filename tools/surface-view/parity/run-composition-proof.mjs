#!/usr/bin/env node
// Native Three.js composition proof. This is separate from parity qualification:
// it asserts composition invariants only and does not alter parity tolerances.
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const argument = name => { const index = process.argv.indexOf(name); return index < 0 ? null : process.argv[index + 1]; };
const repo = resolve(argument('--candidate-root') ?? resolve(here, '../../..'));
const modulePath = argument('--scalafim-module');
const out = resolve(argument('--out') ?? '/private/tmp/scalafim-three-composition-proof');
const reference = '/Users/bbuchsbaum/code/jscode/surfviewjs';
if (!modulePath) throw new Error('missing --scalafim-module');
const sha = value => createHash('sha256').update(value).digest('hex');
const command = (program, args, cwd) => {
  const result = spawnSync(program, args, { cwd, encoding: 'utf8' });
  if (result.status !== 0) throw new Error(`${program} failed (${result.status}): ${result.stderr || result.stdout}`);
  return result.stdout.trim();
};
const guard = resolve(process.env.HOME, '.local/share/agent-policy/browser-automation-guard.mjs');
const audit = () => {
  const result = spawnSync(process.execPath, [guard, '--audit'], { encoding: 'utf8' });
  if (![0, 1].includes(result.status)) throw new Error(`browser audit failed: ${result.stderr}`);
  return { exitCode: result.status, stdout: result.stdout, stderr: result.stderr };
};
const fsUrl = path => `/@fs/${resolve(path)}`;
const closeEnough = (actual, expected, tolerance = 1e-6) => Math.abs(actual - expected) <= tolerance;
const equalRegion = (left, right, x0, y0, x1, y1) => {
  for (let y = y0; y < y1; y++) for (let x = x0; x < x1; x++) {
    const i = (y * left.width + x) * 4;
    for (let c = 0; c < 4; c++) if (left.data[i + c] !== right.data[i + c]) return false;
  }
  return true;
};
const colors = (image, x0, y0, x1, y1) => {
  const result = { red: 0, blue: 0 };
  for (let y = y0; y < y1; y++) for (let x = x0; x < x1; x++) {
    const i = (y * image.width + x) * 4;
    const [r, g, b, a] = [image.data[i], image.data[i + 1], image.data[i + 2], image.data[i + 3]];
    if (a > 0 && r > 200 && g < 40 && b < 40) result.red++;
    if (a > 0 && r < 40 && g < 40 && b > 200) result.blue++;
  }
  return result;
};
const expectedPanels = [
  { view: 'red', instances: ['red-only'], colors: { red: true, blue: false } },
  { view: 'blue', instances: ['blue-only'], colors: { red: false, blue: true } },
  { view: 'split', instances: ['split-red', 'split-blue'], colors: { red: true, blue: true } },
  { view: 'camera', instances: ['camera-red'], colors: { red: true, blue: false } }
];

await mkdir(out, { recursive: true });
const auditBefore = audit();
await writeFile(resolve(out, 'guard-before.json'), `${JSON.stringify(auditBefore, null, 2)}\n`);
const provenance = {
  candidateRoot: repo,
  candidateCommit: command('git', ['rev-parse', 'HEAD'], repo),
  scalaModule: resolve(modulePath),
  scalaModuleSha256: sha(await readFile(modulePath)),
  runnerSha256: sha(await readFile(fileURLToPath(import.meta.url))),
  requestedAngleBackend: 'metal',
  auditBefore,
  startedAt: new Date().toISOString()
};
await writeFile(resolve(out, 'provenance.json'), `${JSON.stringify(provenance, null, 2)}\n`);
const { PNG } = (await import(pathToFileURL(resolve(reference, 'node_modules/playwright-core/lib/utilsBundle.js')))).default;
const decode = data => PNG.sync.read(Buffer.from(data.split(',')[1], 'base64'));
let server, browser, context;
try {
  const { createServer } = await import(pathToFileURL(resolve(reference, 'node_modules/vite/dist/node/index.js')));
  const { chromium } = await import(pathToFileURL(resolve(reference, 'node_modules/playwright/index.mjs')));
  server = await createServer({ configFile: false, root: reference, optimizeDeps: { noDiscovery: true },
    server: { host: '127.0.0.1', port: 0, fs: { allow: [here, repo, reference, dirname(resolve(modulePath))] } } });
  await server.listen();
  browser = await chromium.launch({ headless: true, args: ['--use-angle=metal', '--enable-gpu'] });
  context = await browser.newContext({ viewport: { width: 256, height: 256 }, deviceScaleFactor: 1 });
  const page = await context.newPage();
  await page.goto(new URL(fsUrl(resolve(here, 'index.html')), server.resolvedUrls.local[0]).href);
  await page.evaluate(() => { window.exports = {}; document.body.style.cssText = 'margin:0'; });
  await page.addScriptTag({ url: new URL(fsUrl(modulePath), server.resolvedUrls.local[0]).href });
  const captured = await page.evaluate(async threeUrl => {
    const three = await import(threeUrl);
    const canvas = document.createElement('canvas'); document.body.append(canvas);
    const failure = { remaining: null };
    class Mesh extends three.Mesh {
      constructor(...args) {
        if (failure.remaining === 0) throw new Error('injected composition occurrence failure');
        if (failure.remaining !== null) failure.remaining--;
        super(...args);
      }
    }
    const proof = window.exports.runScalafimThreeCompositionProbe({ ...three, Mesh, __compositionFailure: failure }, canvas);
    const gl = canvas.getContext('webgl2') ?? canvas.getContext('webgl');
    if (!gl) throw new Error('Three composition proof did not create a WebGL context');
    const debug = gl.getExtension('WEBGL_debug_renderer_info');
    return {
      proof,
      webgl: {
        version: String(gl.getParameter(gl.VERSION)),
        vendor: String(gl.getParameter(debug ? debug.UNMASKED_VENDOR_WEBGL : gl.VENDOR)),
        renderer: String(gl.getParameter(debug ? debug.UNMASKED_RENDERER_WEBGL : gl.RENDERER))
      }
    };
  }, fsUrl(resolve(reference, 'node_modules/three/build/three.module.js')));
  const proof = captured.proof;
  const before = decode(proof.before), after = decode(proof.after);
  const panelPixels = [[0, 0], [128, 0], [0, 128], [128, 128]].map(([x, y]) => colors(before, x, y, x + 128, y + 128));
  const panelOrigins = proof.panelOrigins ?? [];
  const panelChecks = expectedPanels.map((expected, index) => {
    const origin = panelOrigins[index];
    const observed = panelPixels[index];
    return origin?.view === expected.view && JSON.stringify(origin.pickViews) === JSON.stringify([expected.view]) &&
      JSON.stringify(origin.instances) === JSON.stringify(expected.instances) &&
      (observed.red > 0) === expected.colors.red && (observed.blue > 0) === expected.colors.blue;
  });
  const nearest = proof.nearestPicks ?? [];
  const nearestByVertex = new Map(nearest.map(receipt => [receipt.vertex, receipt]));
  const nearestChecks = [0, 2, 3].map(vertex => {
    const receipt = nearestByVertex.get(vertex);
    return receipt?.face === 1 && Array.isArray(receipt.barycentric) && Array.isArray(receipt.expected) &&
      receipt.barycentric.length === 3 && receipt.expected.length === 3 &&
      receipt.barycentric.every((value, index) => closeEnough(value, receipt.expected[index]));
  });
  const unchangedPanels = [[128, 0, 256, 128], [0, 128, 128, 256], [128, 128, 256, 256]]
    .map(region => equalRegion(before, after, ...region));
  const checks = {
    failureRecovery: proof.failureRejected === true && proof.failureRecoverySamePixels === true,
    initialSharedMeshUpload: proof.reuseGeometryUploads === 1,
    fourDistinctPanels: proof.viewCount === 4 && panelChecks.every(Boolean),
    cameraOnlyUploads: proof.cameraGeometryUploads === 0 && proof.cameraGeometryUpdates === 0 && proof.cameraColorUploads === 0,
    changedCameraPanel: !equalRegion(before, after, 0, 0, 128, 128),
    unaffectedPanels: unchangedPanels.every(Boolean),
    depthWinnerForwardOrder: proof.instanceCount === 2 && proof.repeatedPick?.found === true &&
      proof.repeatedPick?.origin?.instance === 'front-blue' && proof.repeatedPick?.surface === 'proof-blue',
    depthWinnerReversedOrder: proof.reversedPick?.found === true &&
      proof.reversedPick?.origin?.instance === 'front-blue' && proof.reversedPick?.surface === 'proof-blue',
    nearestFaceOneSourceBarycentrics: nearestChecks.every(Boolean) && nearestByVertex.size === 3
  };
  const status = Object.values(checks).every(Boolean) ? 'pass' : 'fail';
  await writeFile(resolve(out, 'composition-proof.json'), `${JSON.stringify({ status, checks, panelPixels, panelOrigins, unchangedPanels, nearest, webgl: captured.webgl, proof }, null, 2)}\n`);
  await writeFile(resolve(out, 'before.png'), Buffer.from(proof.before.split(',')[1], 'base64'));
  await writeFile(resolve(out, 'after.png'), Buffer.from(proof.after.split(',')[1], 'base64'));
  if (status !== 'pass') process.exitCode = 1;
} finally {
  if (context) await context.close();
  if (browser) await browser.close();
  if (server) await server.close();
  const auditAfter = audit();
  await writeFile(resolve(out, 'guard-after.json'), `${JSON.stringify(auditAfter, null, 2)}\n`);
  if (![0, 1].includes(auditAfter.exitCode)) process.exitCode = 1;
}
