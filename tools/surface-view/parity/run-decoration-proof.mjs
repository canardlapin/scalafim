#!/usr/bin/env node
// Native Three.js decoration proof. This is separate from parity qualification:
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
const out = resolve(argument('--out') ?? '/private/tmp/scalafim-three-decoration-proof');
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
await mkdir(out, { recursive: true });
const auditBefore = audit();
await writeFile(resolve(out, 'guard-before.json'), `${JSON.stringify(auditBefore, null, 2)}\n`);
const provenance = { candidateRoot: repo, candidateCommit: command('git', ['rev-parse', 'HEAD'], repo),
  scalaModule: resolve(modulePath), scalaModuleSha256: sha(await readFile(modulePath)),
  runnerSha256: sha(await readFile(fileURLToPath(import.meta.url))), requestedAngleBackend: 'metal',
  startedAt: new Date().toISOString() };
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
  provenance.browserVersion = browser.version();
  context = await browser.newContext({ viewport: { width: 500, height: 250 }, deviceScaleFactor: 1 });
  const page = await context.newPage();
  await page.goto(new URL(fsUrl(resolve(here, 'index.html')), server.resolvedUrls.local[0]).href);
  await page.evaluate(() => { window.exports = {}; document.body.style.cssText = 'margin:0'; });
  await page.addScriptTag({ url: new URL(fsUrl(modulePath), server.resolvedUrls.local[0]).href });
  const captured = await page.evaluate(async threeUrl => {
    const three = await import(threeUrl);
    const canvas = document.createElement('canvas'); document.body.append(canvas);
    const proof = window.exports.runScalafimThreeDecorationProbe(three, canvas);
    const gl = canvas.getContext('webgl2') ?? canvas.getContext('webgl');
    const debug = gl.getExtension('WEBGL_debug_renderer_info');
    return { proof, webgl: { version: String(gl.getParameter(gl.VERSION)),
      vendor: String(gl.getParameter(debug ? debug.UNMASKED_VENDOR_WEBGL : gl.VENDOR)),
      renderer: String(gl.getParameter(debug ? debug.UNMASKED_RENDERER_WEBGL : gl.RENDERER)) } };
  }, fsUrl(resolve(reference, 'node_modules/three/build/three.module.js')));
  const proof = captured.proof;
  const checks = { twoScientificMeshUploads: proof.initialGeometryUploads === 2,
    staleGeometryRejected: proof.staleRejected === true,
    cameraOnlyUploads: proof.cameraGeometryUploads === 0 && proof.cameraGeometryUpdates === 0 && proof.cameraColorUploads === 0 };
  const cases = [];
  const pixel = (img,x,y) => [...img.data.subarray((y * img.width + x) * 4, (y * img.width + x) * 4 + 4)];
  const red = ([r,g,b,a]) => r > 180 && g < 80 && b < 80 && a === 255;
  const green = ([r,g,b,a]) => r < 80 && g > 180 && b < 80 && a === 255;
  for (const capture of proof.captures) {
    const img = decode(capture.png);
    await writeFile(resolve(out, `${capture.name}.png`), Buffer.from(capture.png.split(',')[1], 'base64'));
    const panel = [0,250].map(x0 => colors(img, x0, 0, x0 + 250, 250));
    const hits = [];
    for (const sample of capture.picks) {
      const isGlyph = capture.name === 'overlay-text' ? green : red;
      const interior = [-1,0,1].every(dy => [-1,0,1].every(dx => isGlyph(pixel(img, sample.x + dx, sample.y + dy))));
      if (!interior) continue;
      const expectedView = `view-${sample.x >= 250 ? 1 : 0}`;
      const expectedKind = capture.name === 'label' ? 'scientific' : 'decoration';
      hits.push({ x: sample.x, y: sample.y, hit: sample.hit,
        pass: sample.hit.kind === expectedKind && sample.hit.view === expectedView });
    }
    if (['disk', 'sphere', 'crosshair', 'label', 'network-line', 'network-tube'].includes(capture.name)) {
      checks[`${capture.name}VisibleInBothViews`] = panel.every(p => p.red > 25);
      checks[`${capture.name}NativePicks`] = hits.length > 0 && hits.every(h => h.pass);
    }
    if (capture.name === 'disk') checks.diskOcclusion = [125,375].every(cx =>
      red(pixel(img, cx - 8, 125)) && JSON.stringify(pixel(img, cx + 8, 125)) === '[0,0,255,255]');
    if (capture.name === 'crosshair') checks.crosshairOverlay = [125,375].every(cx => red(pixel(img,cx+8,125)));
    if (capture.name === 'alpha-oracle') {
      checks.textureTransparentBackground = [125,375].every(cx => JSON.stringify(pixel(img,cx-50,125)) === '[0,0,255,255]');
      checks.textureHalfAlpha = [125,375].every(cx => {
        const c=pixel(img,cx-10,125);return Math.abs(c[0]-128)<=1 && c[1]===0 && Math.abs(c[2]-127)<=1;
      });
      checks.textureInterpolatedAlpha = [125,375].every(cx => {
        const c=pixel(img,cx-20,125);return Math.abs(c[0]-67)<=1 && c[1]===0 && Math.abs(c[2]-188)<=1;
      });
      checks.texturePartialOcclusion = [125,375].every(cx => JSON.stringify(pixel(img,cx+30,125)) === '[0,0,255,255]');
    }
    if (['label-near-first','label-far-first','label-over-opaque'].includes(capture.name)) {
      const expected = capture.name === 'label-over-opaque' ? [128,127,0] : [128,64,63];
      checks[capture.name] = [125,375].every(cx => pixel(img,cx-10,125).slice(0,3).every((c,i) => Math.abs(c-expected[i]) <= 1));
    }
    if (['overlay-near-first', 'overlay-far-first'].includes(capture.name)) {
      const last = capture.name === 'overlay-near-first' ? 'far-overlay' : 'near-overlay';
      const expected = last === 'far-overlay' ? [0,255,0,255] : [255,0,0,255];
      checks[`${capture.name}Pixels`] = [115,365].every(x => JSON.stringify(pixel(img,x,125)) === JSON.stringify(expected));
      checks[`${capture.name}Picks`] = capture.overlayPicks.length === 2 && capture.overlayPicks.every(p => p.id === last);
    }
    if (capture.name.startsWith('rounded-')) {
      const expected = capture.name === 'rounded-opaque-blocker' ? [255,255,255,255]
        : capture.name === 'rounded-near-first' ? [61,153,41,255] : [153,61,41,255];
      const expectedId = capture.name === 'rounded-opaque-blocker' ? 'anchor'
        : capture.name === 'rounded-near-first' ? 'far-rounded' : 'near-rounded';
      checks[`${capture.name}Pixels`] = [115,365].every(x => pixel(img,x,125).every((c,i) => Math.abs(c-expected[i]) <= 1));
      checks[`${capture.name}OpaqueScientificOcclusion`] = [135,385].every(x => JSON.stringify(pixel(img,x,125)) === '[0,0,255,255]');
      checks[`${capture.name}Picks`] = capture.orderedPicks.length === 4 && capture.orderedPicks.every(p =>
        p.id === ([115.5,365.5].includes(p.x) ? expectedId : 'cover'));
    }
    if (capture.name === 'gray') checks.displayGray = [125,375].every(cx => pixel(img,cx,125).slice(0,3).every(c => Math.abs(c-128) <= 1));
    if (capture.name === 'overlay-text') {
      checks.overlayAfterText = [125,375].every(cx => {
        for (let y=116;y<134;y++) for (let x=cx-9;x<cx+9;x++) if(!green(pixel(img,x,y))) return false;
        return true;
      });
      checks.overlayNativePicks = hits.length > 0 && hits.every(h=>h.pass);
    }
    cases.push({ name: capture.name, panel, hits });
  }
  const before=decode(proof.before),after=decode(proof.after),plain=decode(proof.plain);
  checks.cameraChangedOneView = !equalRegion(before, after, 0,0,250,250) && equalRegion(before, after,250,0,500,250);
  checks.plainRenderClearsGlyphs = colors(plain,0,0,500,250).red === 0;
  for (const name of ['before','after','plain']) await writeFile(resolve(out,`${name}.png`),Buffer.from(proof[name].split(',')[1],'base64'));
  const status = Object.values(checks).every(Boolean) ? 'pass' : 'fail';
  await writeFile(resolve(out,'decoration-proof.json'),`${JSON.stringify({status,checks,cases,webgl:captured.webgl},null,2)}\n`);
  console.log(JSON.stringify({status,checks}));
  if(status!=='pass') process.exitCode=1;
} finally {
  if(context) await context.close();
  if(browser) await browser.close();
  if(server) await server.close();
  await writeFile(resolve(out,'guard-after.json'),`${JSON.stringify(audit(),null,2)}\n`);
  await writeFile(resolve(out,'provenance.json'),`${JSON.stringify(provenance,null,2)}\n`);
}
