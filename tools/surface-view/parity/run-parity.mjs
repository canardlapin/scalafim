#!/usr/bin/env node
// Task-owned installed Playwright + Vite. Observed-default smoke captures do not
// confer qualification on a case family or an uncalibrated material profile.
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile, stat } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { compareRgba } from './compare.mjs';
import { validateRequiredMatrix } from './matrix.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const argument = name => { const i = process.argv.indexOf(name); return i < 0 ? null : process.argv[i + 1]; };
const repo = resolve(argument('--candidate-root') ?? resolve(here, '../../..'));
const reference = '/Users/bbuchsbaum/code/jscode/surfviewjs';
const scalarModule = argument('--scalafim-module');
const calibrationOnly = process.argv.includes('--reference-calibration-only');
const output = resolve(argument('--out') ?? '/private/tmp/scalafim-surfview-parity-artifacts');
const angle = argument('--angle');
if (angle && !['metal', 'swiftshader'].includes(angle)) throw new Error('--angle must be metal or swiftshader');
const { PNG } = (await import(pathToFileURL(resolve(reference, 'node_modules/playwright-core/lib/utilsBundle.js')))).default;
const guard = resolve(process.env.HOME, '.local/share/agent-policy/browser-automation-guard.mjs');
const sha = value => createHash('sha256').update(value).digest('hex');
const writeJson = (path, value) => writeFile(path, `${JSON.stringify(value, null, 2)}\n`);
const json = async path => JSON.parse(await readFile(path, 'utf8'));
const fsUrl = path => `/@fs/${resolve(path)}`;
function command(command, args, cwd) {
  const result = spawnSync(command, args, { cwd, encoding: 'utf8' });
  if (result.status !== 0) throw new Error(`${command} failed (${result.status}): ${result.stderr || result.stdout}`);
  return result.stdout.trim();
}
function audit() {
  const result = spawnSync(process.execPath, [guard, '--audit'], {encoding:'utf8'});
  if (![0,1].includes(result.status)) throw new Error(`browser audit unavailable: ${result.stderr}`);
  // Exit 1 inventories existing automation, including other sessions. It is
  // not permission to attach to or clean up any of those processes.
  return {exitCode:result.status,inventory:result.stdout};
}
function ownedBrowsers() {
  const rows=command('ps',['-ax','-o','pid=,ppid=,command=']).split('\n').flatMap(line=>{
    const m=line.match(/^\s*(\d+)\s+(\d+)\s+(.*)$/); return m?[{pid:+m[1],ppid:+m[2],command:m[3]}]:[];
  });
  const owned=new Set([process.pid]);
  let changed=true;
  while(changed) {changed=false; for(const r of rows) if(owned.has(r.ppid)&&!owned.has(r.pid)){owned.add(r.pid);changed=true;}}
  return rows.filter(r=>owned.has(r.pid)&&/--remote-debugging-pipe|--remote-debugging-port/.test(r.command)&&/chrome|chromium/i.test(r.command));
}
async function png(url, path) {
  if (!url.startsWith('data:image/png;base64,')) throw new Error('expected an actual PNG framebuffer');
  await writeFile(path, Buffer.from(url.split(',')[1], 'base64'));
}
if (!scalarModule && !calibrationOnly) throw new Error('missing --scalafim-module');
if (scalarModule) await stat(scalarModule);
const manifest = await json(resolve(here, 'manifest.json'));
const frozenInputs = await json(resolve(here, 'frozen-inputs.json'));
for (const [name, expected] of Object.entries(frozenInputs.sourceHashes)) {
  const inputPath = name.endsWith('.gii') ? resolve(manifest.requiredAssets.bilateralCortex.localRoot, name) : resolve(here, name);
  if (sha(await readFile(inputPath)) !== expected) throw new Error(`frozen gallery input changed: ${name}`);
}
const referenceCommit = command('git', ['rev-parse', 'HEAD'], reference);
if (referenceCommit !== manifest.reference.commit) throw new Error('reference commit differs from frozen manifest');
const provenance = {
  schema: 'scalafim.surface-view-parity-provenance.v1',
  purpose: calibrationOnly ? 'reference-only material calibration; no candidate qualification' : 'observed-default baseline smoke; incomplete and uncalibrated',
  candidateRoot: repo, candidateCommit: command('git', ['rev-parse', 'HEAD'], repo),
  candidateTrackedDiffSha256: sha(command('git', ['diff', '--binary', 'HEAD', '--', 'modules', 'build.sbt', 'examples/surface-view'], repo)),
  referenceCommit,
  referenceRelevantDiff: command('git', ['status', '--porcelain', '--', 'src', 'package.json', 'package-lock.json'], reference),
  referencePackageLockSha256: sha(await readFile(resolve(reference, 'package-lock.json'))),
  threeVersion: (await json(resolve(reference, 'node_modules/three/package.json'))).version,
  playwrightVersion: (await json(resolve(reference, 'node_modules/playwright/package.json'))).version,
  scalaModule: scalarModule ? resolve(scalarModule) : null,
  scalaModuleSha256: scalarModule ? sha(await readFile(scalarModule)) : null,
  runnerSha256: sha(await readFile(fileURLToPath(import.meta.url))),
  helperSha256: Object.fromEntries(await Promise.all(
    ['reference-view.mjs', 'calibrate-reference.mjs', 'fixtures.mjs', 'gallery.mjs', 'compare.mjs', 'matrix.mjs']
      .map(async name => [name, sha(await readFile(resolve(here, name)))]))),
  manifestSha256: sha(await readFile(resolve(here, 'manifest.json'))),
  frozenInputsSha256: sha(await readFile(resolve(here, 'frozen-inputs.json'))),
  requestedAngleBackend: angle ?? 'browser default',
  auditBefore: audit(), startedAt: new Date().toISOString()
};
if (provenance.referenceRelevantDiff) throw new Error('reference source/lockfile is dirty');
await mkdir(output, { recursive: true });
await writeJson(resolve(output, 'provenance.json'), provenance);
await writeFile(resolve(output, 'captured-manifest.json'), await readFile(resolve(here, 'manifest.json')));
let server, browser, context;
const rows = [];
try {
  const { createServer, build } = await import(pathToFileURL(resolve(reference, 'node_modules/vite/dist/node/index.js')));
  const referenceBuild = resolve(output, 'reference-source-build');
  await build({ configFile: false, root: reference, logLevel: 'warn',
    build: { outDir: referenceBuild, emptyOutDir: false, minify: false, sourcemap: false,
      lib: { entry: resolve(reference, 'src/index.ts'), formats: ['es'], fileName: () => 'reference.mjs' } }
  });
  provenance.referenceBundleSha256 = sha(await readFile(resolve(referenceBuild, 'reference.mjs')));
  server = await createServer({
    configFile: false, root: reference, cacheDir: resolve(output, 'vite-cache'),
    resolve: { alias: [{ find: /^three$/, replacement: resolve(reference, 'node_modules/three/build/three.module.js') }] },
    optimizeDeps: { noDiscovery: true },
    server: { host: '127.0.0.1', port: 0, fs: { allow: [here, repo, reference, output, ...(scalarModule ? [dirname(resolve(scalarModule))] : [])] } }
  });
  await server.listen();
  const url = server.resolvedUrls.local[0];
  const { chromium } = await import(pathToFileURL(resolve(reference, 'node_modules/playwright/index.mjs')));
  browser = await chromium.launch({ headless: true, args: angle ? [`--use-angle=${angle}`, '--enable-gpu'] : [] });
  provenance.ownedBrowserProcesses=ownedBrowsers();
  context = await browser.newContext({ viewport: { width: 1050, height: 550 }, deviceScaleFactor: 1 });
  for (const entry of calibrationOnly ? [] : manifest.requiredCases) {
    const caseDir = resolve(output, entry.id);
    await mkdir(caseDir, { recursive: true });
    const row = { case: entry.id, status: 'fail', artifacts: [] };
    rows.push(row);
    if (!['C01', 'C02', 'C03'].includes(entry.id)) {
      row.reason = 'full required family adapter is not implemented';
      await writeJson(resolve(caseDir, 'failure.json'), row);
      continue;
    }
    const page = await context.newPage();
    const errors = [];
    const timeout = setTimeout(() => page.close().catch(() => {}), 60000);
    page.on('pageerror', error => errors.push(String(error)));
    page.on('console', message => {
      if (message.type() === 'error') errors.push(message.text());
    });
    try {
      await page.goto(new URL(fsUrl(resolve(here, 'index.html')), url).href);
      await page.evaluate(() => { window.exports = {}; document.body.style.cssText = 'margin:0;display:flex;background:white'; });
      await page.addScriptTag({ url: new URL(fsUrl(scalarModule), url).href });
      const captured = await page.evaluate(async ({ caseId, refUrl, threeUrl }) => {
        const ref = await import(refUrl), three = await import(threeUrl);
        const root = document.createElement('div'); root.style.cssText = 'width:512px;height:512px;flex:none'; document.body.append(root);
        const viewer = new ref.NeuroSurfaceViewer(root, 512, 512, { backgroundColor: 0xffffff, useShaders: false, initialZoom: 4 }, 'left_dorsal');
        window.__surfaceParityViewer = viewer;
        if (viewer.initializationFailed) throw new Error('SurfViewJS WebGL initialization failed');
        const vertices = new Float32Array([0,0,0,1.4,.1,.2,-.2,1.1,.35,.15,-.35,1.3]);
        const faces = new Uint32Array([0,1,2,0,3,1,0,2,3,1,3,2]);
        const curvature = new Float32Array([-.8,.25,.7,-.15]);
        const geometry = new ref.SurfaceGeometry(vertices, faces, 'left', curvature, false);
        const surface = new ref.MultiLayerNeuroSurface(geometry, { curvature, showCurvature: caseId !== 'C02' });
        if (caseId !== 'C01') surface.addLayer(new ref.DataLayer('scalar', new Float32Array([-2.5,-2.5,2.5,2.5]), null,
          new ref.ColorMap([[0,0,1],[1,0,0]], { range: [-2.5,2.5], threshold: [-.35,.35] }),
          { range: [-2.5,2.5], threshold: [-.35,.35], opacity: .85 }));
        if (caseId === 'C03') surface.addLayer(new ref.DataLayer('scalar-2', new Float32Array([1,1,-1,-1]), null,
          new ref.ColorMap([[0,0,1],[1,0,0]], { range: [-1,1] }), { range: [-1,1], opacity: .35, blendMode: 'additive' }));
        viewer.addSurface(surface, 'left');
        // Public camera and controls fields; no renderer-internal camera patch.
        Object.assign(viewer.camera, { fov: 35, aspect: 1, near: .01, far: 1000 });
        viewer.camera.position.set(.6,.375,4.65); viewer.camera.up.set(0,1,0); viewer.camera.lookAt(.6,.375,.65);
        viewer.cameraControls.target.set(.6,.375,.65); viewer.cameraControls.update();
        viewer.camera.updateMatrixWorld(); viewer.camera.updateProjectionMatrix();
        // Flush layer work, and capture in the same JS turn as rendering, before
        // a non-preserved WebGL drawing buffer may be cleared by the browser.
        viewer.render();
        const referencePng = viewer.renderer.domElement.toDataURL('image/png');
        const canvas = document.createElement('canvas'); canvas.width = 512; canvas.height = 512; document.body.append(canvas);
        let semantic;
        try { semantic = window.exports.mountScalafimSurfaceParityBaseline(three, canvas, caseId); }
        catch (error) {
          // Scala.js exceptions are not native JS Errors and may contain deep
          // object graphs. Preserve their message without asking Playwright to
          // serialize a Scala runtime exception object.
          throw new Error(`ScalaFIM public adapter failed: ${String(error)}`);
        }
        const candidatePng = semantic.framebuffer; delete semantic.framebuffer;
        const gl = viewer.renderer.getContext(), debug = gl.getExtension('WEBGL_debug_renderer_info');
        return { referencePng, candidatePng, semantic: {
          caseId, materialProfile: 'observed-default-lit', scalafim: semantic,
          surfviewjs: { vertices: 4, faces: 4, matrixOrder: 'column-major',
            viewMatrix: Array.from(viewer.camera.matrixWorldInverse.elements), projectionMatrix: Array.from(viewer.camera.projectionMatrix.elements),
            modelMatrix: Array.from(surface.mesh.matrixWorld.elements),
            viewport: [viewer.renderer.domElement.width, viewer.renderer.domElement.height],
            colorSpace: viewer.renderer.outputColorSpace, toneMapping: viewer.renderer.toneMapping,
            material: surface.mesh.material.type, antialias: gl.getContextAttributes().antialias,
            renderer: String(gl.getParameter(debug ? debug.UNMASKED_RENDERER_WEBGL : gl.RENDERER)) },
          environment: { userAgent: navigator.userAgent, platform: navigator.platform, devicePixelRatio }
        } };
      }, { caseId: entry.id, refUrl: fsUrl(resolve(referenceBuild, 'reference.mjs')), threeUrl: fsUrl(resolve(reference, 'node_modules/three/build/three.module.js')) });
      const ref = captured.semantic.surfviewjs, candidate = captured.semantic.scalafim.observed;
      const difference = (column, row) => Math.max(...row.map((v, i) => Math.abs(v - column[(i % 4) * 4 + Math.floor(i / 4)])));
      captured.semantic.cameraAgreement = { maximumViewDifference: difference(ref.viewMatrix, candidate.viewMatrix), maximumProjectionDifference: difference(ref.projectionMatrix, candidate.projectionMatrix) };
      await png(captured.referencePng, resolve(caseDir, 'surfviewjs.png'));
      await png(captured.candidatePng, resolve(caseDir, 'scalafim-three.png'));
      const additional = [];
      for (const name of ['scalafim-javafx', 'scalafim-raster']) {
        try { additional.push({ name, url: `data:image/png;base64,${(await readFile(resolve(caseDir, `${name}.png`))).toString('base64')}` }); }
        catch { row.artifacts.push({ name, missing: true }); }
      }
      const images = [captured.referencePng,captured.candidatePng,...additional.map(x=>x.url)]
        .map(url=>PNG.sync.read(Buffer.from(url.split(',')[1],'base64')));
      const [left,right] = images;
      const plate = new PNG({width:images.reduce((s,im)=>s+im.width,0),height:Math.max(...images.map(im=>im.height))});
      let px=0;
      for(const im of images) {PNG.bitblt(im,plate,0,0,im.width,im.height,px,0);px+=im.width;}
      const heat = new PNG({width:left.width,height:left.height});
      const overlay = new PNG({width:left.width,height:left.height});
      for(let i=0;i<left.data.length;i+=4) {
        heat.data[i]=Math.max(...[0,1,2,3].map(k=>Math.abs(left.data[i+k]-right.data[i+k])));
        heat.data[i+3]=255;
        for(let k=0;k<4;k++)overlay.data[i+k]=Math.round((left.data[i+k]+right.data[i+k])/2);
      }
      const dataUrl = im=>'data:image/png;base64,'+PNG.sync.write(im).toString('base64');
      const pixels={width:left.width,height:left.height,candidateWidth:right.width,candidateHeight:right.height,
        buffers:images.map(im=>im.data),plate:dataUrl(plate),heat:dataUrl(heat),overlay:dataUrl(overlay)};
      const comparison = compareRgba({ reference: pixels.buffers[0], candidate: pixels.buffers[1],
        width:pixels.width,height:pixels.height,candidateWidth:pixels.candidateWidth,candidateHeight:pixels.candidateHeight,
        tolerances: { minimumForegroundIoU:.995,maximumChannelError:2,colorPolicy:{calibrated:false} } });
      const otherComparisons = additional.map((item,i)=>({ renderer:item.name, ...compareRgba({reference:pixels.buffers[0],candidate:pixels.buffers[i+2],width:pixels.width,height:pixels.height,tolerances:{colorPolicy:{calibrated:false}}}) }));
      row.reason='observed-default smoke only; required family coverage and controlled material calibration are incomplete';
      row.capture='complete'; row.comparison=comparison; row.otherComparisons=otherComparisons;
      row.pageErrors=errors; row.cameraAgreement=captured.semantic.cameraAgreement;
      await png(pixels.plate,resolve(caseDir,'side-by-side.png'));
      await png(pixels.heat,resolve(caseDir,'error-heatmap.png'));
      await png(pixels.overlay,resolve(caseDir,'alpha-overlay.png'));
      await writeJson(resolve(caseDir,'semantic.json'),captured.semantic);
      await writeJson(resolve(caseDir,'comparison.json'),row);
    } catch(error) {
      row.reason=String(error); row.pageErrors=errors;
      await writeJson(resolve(caseDir,'failure.json'),row);
    } finally {
      clearTimeout(timeout);
      try { if (!page.isClosed()) await page.evaluate(() => { window.__surfaceParityViewer?.dispose(); window.exports?.disposeScalafimSurfaceParityBaseline?.(); }); }
      catch(error) { row.cleanupError=String(error); }
      finally { if (!page.isClosed()) await page.close(); }
    }
  }
  if (calibrationOnly || process.argv.includes('--reference-calibration')) {
    const page = await context.newPage();
    const timeout = setTimeout(() => page.close().catch(() => {}), 60000);
    const calibration = { status: 'fail', candidateQualification: false };
    try {
      await page.goto(new URL(fsUrl(resolve(here, 'index.html')), url).href);
      const captured = await page.evaluate(async ({ refUrl, threeUrl, adapterUrl, calibrationUrl }) => {
        const ref = await import(refUrl), three = await import(threeUrl);
        const { mountReferenceView } = await import(adapterUrl);
        const { captureReferenceCalibration } = await import(calibrationUrl);
        return captureReferenceCalibration({ ref, three, mount: mountReferenceView });
      }, {
        refUrl: fsUrl(resolve(referenceBuild, 'reference.mjs')),
        threeUrl: fsUrl(resolve(reference, 'node_modules/three/build/three.module.js')),
        adapterUrl: fsUrl(resolve(here, 'reference-view.mjs')),
        calibrationUrl: fsUrl(resolve(here, 'calibrate-reference.mjs'))
      });
      const directory = resolve(output, 'reference-calibration');
      await mkdir(directory, { recursive: true });
      for (const specimen of captured.specimens) {
        const images = specimen.repeats.map(r => PNG.sync.read(Buffer.from(r.image.split(',')[1], 'base64')));
        for (let i = 0; i < specimen.repeats.length; i++) {
          await png(specimen.repeats[i].image, resolve(directory, `${specimen.id}-${i}.png`));
          delete specimen.repeats[i].image;
        }
        specimen.maximumRepeatChannelError = images[0].data.reduce((max, value, i) => Math.max(max, Math.abs(value - images[1].data[i])), 0);
        let maximumMappingError = 0;
        const region = captured.interior;
        for (let y = region.y; y < region.y + region.height; y++) {
          for (let x = region.x; x < region.x + region.width; x++) {
            let expected = specimen.expectedInteriorRgba;
            if (specimen.oracle === 'decode-vertices-then-linear-interpolation-then-encode') {
              // Analytic square x in [-1,1], fronto-parallel at distance4.
              // Independently invert the declared projection at pixel centers.
              const worldX = (2 * (x + .5) / 256 - 1) * 4 * Math.tan(35 * Math.PI / 360);
              const linear = (worldX + 1) / 2;
              const encoded = linear <= .0031308 ? 12.92 * linear : 1.055 * linear ** (1 / 2.4) - .055;
              const channel = Math.round(255 * encoded);
              expected = [channel, channel, channel, 255];
            }
            for (let channel = 0; channel < 4; channel++) {
              const actual = images[0].data[(y * images[0].width + x) * 4 + channel];
              maximumMappingError = Math.max(maximumMappingError, Math.abs(actual - expected[channel]));
            }
          }
        }
        specimen.maximumMappingChannelError = maximumMappingError;
        specimen.center = [...images[0].data.slice((128 * 256 + 128) * 4, (128 * 256 + 128) * 4 + 4)];
      }
      calibration.result = captured;
      calibration.status = captured.specimens.every(s => s.maximumRepeatChannelError === 0 && s.maximumMappingChannelError <= 1) ? 'pass' : 'fail';
    } catch (error) { calibration.error = String(error); }
    finally { clearTimeout(timeout); if (!page.isClosed()) await page.close(); }
    provenance.referenceCalibration = calibration.status;
    await writeJson(resolve(output, 'reference-calibration.json'), calibration);
  }
  if (process.argv.includes('--native-probes')) {
    provenance.nativeProbes = [];
    for (const name of ['Face', 'Nearest', 'Fragment']) {
      const page = await context.newPage();
      const timeout = setTimeout(() => page.close().catch(() => {}), 180000);
      const receipt = { name, status: 'fail' };
      provenance.nativeProbes.push(receipt);
      try {
        await page.goto(new URL(fsUrl(resolve(here, 'index.html')), url).href);
        await page.evaluate(() => { window.exports = {}; });
        await page.addScriptTag({ url: new URL(fsUrl(scalarModule), url).href });
        const result = await page.evaluate(async ({ name, threeUrl }) => {
          const three = await import(threeUrl);
          const canvas = document.createElement('canvas'); document.body.append(canvas);
          try { return window.exports[`runScalafimThree${name}Probe`](three, canvas); }
          catch (error) { throw new Error(String(error)); }
        }, { name, threeUrl: fsUrl(resolve(reference, 'node_modules/three/build/three.module.js')) });
        if (result.image) {
          await png(result.image, resolve(output, `native-${name.toLowerCase()}.png`));
          delete result.image;
        }
        const checks = name === 'Fragment'
          ? [...result.cases, ...result.edgeCases, ...result.clippingCases]
          : [result];
        const updatesPass = name !== 'Fragment' ||
          (JSON.stringify(result.updates.map(r => r.path)) === JSON.stringify(['timepoint', 'threshold', 'camera', 'morph']) &&
           result.updates.every(r => r.pickPassed === true && r.geometryUploads === 0 &&
             r.geometryUpdates === (r.path === 'morph' ? 1 : 0) &&
             r.layerUploads === (['timepoint', 'threshold'].includes(r.path) ? 1 : 0)));
        receipt.status = checks.length && checks.every(r => r.passed === true || r.status === 'pass') && updatesPass ? 'pass' : 'fail';
        receipt.result = result;
      } catch (error) { receipt.error = String(error); }
      finally { clearTimeout(timeout); if (!page.isClosed()) await page.close(); }
      await writeJson(resolve(output, `native-${name.toLowerCase()}.json`), receipt);
    }
  }
} finally {
  if(context) await context.close();
  if(browser) await browser.close();
  if(server) await server.close();
  provenance.auditAfter=audit(); provenance.remainingOwnedBrowserProcesses=ownedBrowsers();
  provenance.finishedAt=new Date().toISOString();
  await writeJson(resolve(output,'provenance.json'),provenance);
}
// A smoke image is not a receipt for any complete required family checkpoint.
// Emit the complete missing denominator rather than silently omitting rows.
const matrixValidation = validateRequiredMatrix({ manifest, receipts: [], frozen: {} });
const matrixRows = manifest.requiredMatrix.flatMap(row => row.checkpoints.map(checkpoint => ({
  rowId: row.id, checkpoint, productRoute: row.route, status: 'fail',
  reason: 'required checkpoint has no complete calibrated receipt'
})));
await writeFile(resolve(output, 'frozen-inputs.json'), await readFile(resolve(here, 'frozen-inputs.json')));
await writeJson(resolve(output,'required-matrix.json'), { validation: matrixValidation, rows: matrixRows });
await writeJson(resolve(output,'summary.json'),{status:'fail',qualification:false,rows,matrixValidation});
const escape = s => String(s).replaceAll('&','&amp;').replaceAll('<','&lt;').replaceAll('"','&quot;');
await writeFile(resolve(output,'index.html'),`<!doctype html><meta charset="utf-8"><title>Surface parity baseline</title><style>body{font:16px system-ui;margin:32px;color:#182229;background:#f4f5f6}article{background:white;padding:20px;margin:20px 0}img{max-width:100%;height:auto}code{overflow-wrap:anywhere}</style><h1>Surface display parity — observed baseline</h1><p>All cases remain unqualified. Plates: SurfViewJS, ScalaFIM Three.js, then JavaFX and portable raster when available. Each image retains its original 512px scale.</p><p>Candidate ${escape(provenance.candidateCommit)}; reference ${escape(referenceCommit)}.</p>${rows.map(r=>`<article><h2>${escape(r.case)} — ${escape(r.capture??'missing')}</h2><p>${escape(r.reason)}</p>${r.capture?`<a href="${r.case}/side-by-side.png"><img src="${r.case}/side-by-side.png"></a><p><a href="${r.case}/alpha-overlay.png">Overlay</a> · <a href="${r.case}/error-heatmap.png">Heatmap</a> · <a href="${r.case}/semantic.json">Semantics</a> · <a href="${r.case}/comparison.json">Measurements</a></p><pre>${escape(JSON.stringify(r.comparison.metrics,null,2))}</pre>`:''}</article>`).join('')}`);
console.log(JSON.stringify({output,captured:rows.filter(r=>r.capture).map(r=>r.case),failures:rows.map(r=>({case:r.case,reason:r.reason})),qualification:false},null,2));
process.exitCode=1;
