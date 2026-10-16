#!/usr/bin/env node
// Endpoint captures are evidence, not substitutes for the mandatory host replay.
// Default exit 0 means capture completed; --require-qualified is the fail-closed gate.
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile, realpath } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { loadQualificationContracts } from './qualification-contracts.mjs';
import { loadCortex, qualificationSpec } from './qualification-specs.mjs';
import { compareRgba } from './compare.mjs';
import { cleanupOwned } from './resource-contracts.mjs';
import { scientificPick, compareScientificPick, analyticVolumeSamples, compareVolumeSamples, annotationCenters, compareAnnotationCenters } from './scientific-oracle.mjs';
const here = dirname(fileURLToPath(import.meta.url));
const arg = name => { const i = process.argv.indexOf(name); return i < 0 ? null : process.argv[i + 1]; };
const repo = resolve(here, '../../..'), reference = resolve(arg('--reference-root') ?? '/Users/bbuchsbaum/code/jscode/surfviewjs');
const modulePath = resolve(arg('--module') ?? resolve(repo, 'examples/surface-view/js/target/scala-3.7.4/scalafim-examples-surface-view-fastopt/main.js'));
const out = resolve(arg('--out') ?? '/private/tmp/scalafim-qualification');
const depthPolicy = arg('--depth-policy') ?? 'conditioned';
const jvmResults = arg('--jvm-results');
const referenceClient = {followMorphAnnotations:process.argv.includes('--reference-follow-morph')};
const roundedContours = process.argv.includes('--rounded-contours');
// Explicit candidate display policy. The pinned reference already uses rounded
// strokes with 0.6 halo opacity. Preserve the original side-band captures too.
function prepare(variant, cortex) {
  const result = qualificationSpec(variant, cortex);
  if (roundedContours) for (const surface of result.spec.surfaces) for (const layer of surface.layers ?? []) {
    if (layer.kind === 'outline') Object.assign(layer, {mode: 'rounded-strokes', haloOpacity: 0.6});
  }
  return result;
}
if (!['conditioned', 'preserved'].includes(depthPolicy)) throw new Error('unknown depth policy');
const sha = bytes => createHash('sha256').update(bytes).digest('hex');
const json = (path, value) => writeFile(path, JSON.stringify(value, null, 2) + '\n');
const command = (cmd, args, cwd) => { const r = spawnSync(cmd, args, { cwd, encoding: 'utf8' }); if (r.status !== 0) throw new Error(r.stderr || r.stdout); return r.stdout.trim(); };
const audit = () => { const r = spawnSync(process.execPath, [resolve(process.env.HOME, '.local/share/agent-policy/browser-automation-guard.mjs'), '--audit'], { encoding: 'utf8' }); if (![0, 1].includes(r.status)) throw new Error('browser policy audit unavailable'); return { exitCode: r.status, output: r.stdout }; };
const fsUrl = path => '/@fs/' + resolve(path);
// Bind dirty and untracked source, too: HEAD alone is not a build identity.
async function sourceIdentity() {
  const paths = command('git', ['ls-files', '--cached', '--others', '--exclude-standard', '--',
    'build.sbt', 'project', 'modules/surface*', 'modules/image', 'examples/surface-view', 'tools/surface-view'], repo).split('\n').filter(Boolean).sort();
  const files = {};
  for (const path of paths) {
    try { files[path] = sha(await readFile(resolve(repo, path))); }
    catch (error) { if (error.code === 'ENOENT') files[path] = 'deleted'; else throw error; }
  }
  return { sha256: sha(JSON.stringify(files)), files };
}
const all = await loadQualificationContracts();
const manifest = JSON.parse(await readFile(resolve(here, 'manifest.json')));
for (const [name, expected] of Object.entries(all.sourceHashes)) {
  const path = name.endsWith('.gii') ? resolve(manifest.requiredAssets.bilateralCortex.localRoot, name) : resolve(here, name);
  if (sha(await readFile(path)) !== expected) throw new Error(`frozen input changed: ${name}`);
}
const referenceCommit = command('git', ['rev-parse', 'HEAD'], reference);
if (referenceCommit !== manifest.reference.commit || command('git', ['status', '--porcelain', '--', 'src', 'package.json', 'package-lock.json'], reference)) throw new Error('reference pin or source drift');
const candidates = all.variants.filter(v => (!arg('--row') || new RegExp(arg('--row')).test(v.parent.rowId)) && (!arg('--checkpoint') || v.parent.checkpoint === arg('--checkpoint')) && (!arg('--profile') || v.effectiveInput.bundle.profile === arg('--profile')));
const offset = Number(arg('--offset') ?? 0), limit = Number(arg('--limit') ?? candidates.length);
if (!Number.isSafeInteger(offset) || offset < 0 || !Number.isSafeInteger(limit) || limit < 1) throw new Error('offset/limit must be nonnegative/positive integers');
const selected = candidates.slice(offset, offset + limit);
if (!selected.length) throw new Error('empty selection');
const cortex = await loadCortex(manifest.requiredAssets.bilateralCortex.localRoot);
await mkdir(out, { recursive: true });
if (process.argv.includes('--prepare-only')) {
  const rows = [];
  for (const variant of selected) {
    const directory = resolve(out, variant.id.replaceAll('/', '--'));
    await mkdir(directory, { recursive: true });
    await json(resolve(directory, 'contract.json'), variant);
    try {
      const prepared = prepare(variant, cortex);
      await json(resolve(directory, 'input.json'), prepared.spec);
      await json(resolve(directory, 'oracle.json'), prepared.scientific);
      rows.push({ id: variant.id, inputSha256: sha(await readFile(resolve(directory, 'input.json'))) });
    } catch (error) { rows.push({ id: variant.id, failure: String(error) }); }
  }
  await json(resolve(out, 'prepared.json'), { frozenContractsSha256: all.frozenContractsSha256, rows });
  console.log(JSON.stringify({ prepared: rows.filter(x => !x.failure).length, unavailable: rows.filter(x => x.failure).length }));
  process.exit(0);
}
const provenance = { schema: 'surface-qualification-endpoints.v1', startedAt: new Date().toISOString(), candidateCommit: command('git', ['rev-parse', 'HEAD'], repo), referenceCommit,
  candidateSource: await sourceIdentity(), referenceClient, roundedContours,
  moduleSha256: sha(await readFile(modulePath)), depthPolicy, frozenContractsSha256: all.frozenContractsSha256, browserProfile: 'lean-browser-playwright', rendererRequest: 'ANGLE Metal', auditBefore: audit(),
  runnerHashes: Object.fromEntries(await Promise.all(['run-qualification.mjs', 'reference-view.mjs', 'qualification-contracts.mjs', 'qualification-specs.mjs', 'scientific-oracle.mjs', 'compare.mjs', 'resource-contracts.mjs'].map(async name => [name, sha(await readFile(resolve(here, name)))]))) };
if (arg('--build-receipt')) {
  provenance.build = JSON.parse(await readFile(arg('--build-receipt'), 'utf8'));
  if (provenance.build.moduleSha256 !== provenance.moduleSha256) throw new Error('built module checksum differs from retained build receipt');
}
if (jvmResults) {
  try {
    const bytes = await readFile(resolve(jvmResults, 'runtime.json'));
    provenance.jvmRuntime = { sha256: sha(bytes), ...JSON.parse(bytes) };
  } catch (error) { if (error.code !== 'ENOENT') throw error; provenance.jvmRuntime = { status: 'missing' }; }
}
await mkdir(resolve(out, 'runner-source'), { recursive: true });
for (const name of Object.keys(provenance.runnerHashes)) {
  const bytes = await readFile(resolve(here, name));
  if (sha(bytes) !== provenance.runnerHashes[name]) throw new Error('runner changed during source snapshot');
  await writeFile(resolve(out, 'runner-source', name), bytes);
}
await json(resolve(out, 'provenance.json'), provenance);
const { PNG } = (await import(pathToFileURL(resolve(reference, 'node_modules/playwright-core/lib/utilsBundle.js')))).default;
const decode = url => PNG.sync.read(Buffer.from(url.split(',')[1], 'base64'));
let server, browser, context;
const rows = [];
try {
  const { build, createServer } = await import(pathToFileURL(resolve(reference, 'node_modules/vite/dist/node/index.js')));
  const bundleDir = resolve(out, 'reference-build');
  await build({ configFile: false, root: reference, logLevel: 'warn', build: { outDir: bundleDir, emptyOutDir: false, minify: false,
    lib: { entry: resolve(reference, 'src/index.ts'), formats: ['es'], fileName: () => 'reference.mjs' } } });
  const bundle = resolve(bundleDir, 'reference.mjs'); provenance.referenceBundleSha256 = sha(await readFile(bundle));
  server = await createServer({ configFile: false, root: reference, cacheDir: resolve(out, 'vite-cache'), optimizeDeps: { noDiscovery: true },
    resolve: { alias: [{ find: /^three$/, replacement: resolve(reference, 'node_modules/three/build/three.module.js') }] },
    server: { host: '127.0.0.1', port: 0, fs: { allow: [repo, reference, await realpath(resolve(reference, 'node_modules')), out, dirname(modulePath)] } } });
  await server.listen();
  const { chromium } = await import(pathToFileURL(resolve(reference, 'node_modules/playwright/index.mjs')));
  browser = await chromium.launch({ headless: true, args: ['--use-angle=metal', '--enable-gpu', '--enable-precise-memory-info'] });
  context = await browser.newContext({ viewport: { width: 2000, height: 1500 }, deviceScaleFactor: 1 });
  for (const variant of selected) {
    const id = variant.id.replaceAll('/', '--'), directory = resolve(out, id); await mkdir(directory, { recursive: true });
    const row = { id: variant.id, parent: variant.parent, requirement: variant.requirement, status: 'fail', artifacts: {} }; rows.push(row);
    await json(resolve(directory, 'contract.json'), variant);
    let page;
    try {
      const prepared = prepare(variant, cortex);
      await json(resolve(directory, 'input.json'), prepared.spec); await json(resolve(directory, 'oracle.json'), prepared.scientific);
      row.inputSha256 = sha(await readFile(resolve(directory, 'input.json')));
      const jvmRoute = variant.effectiveInput.productRoute === 'JVM viewer';
      if (variant.effectiveInput.productRoute !== 'browser viewer' && !(jvmRoute && jvmResults)) throw new Error('required JVM/publication route needs its actual host; browser capture cannot substitute');
      page = await context.newPage(); const errors = [];
      page.on('pageerror', e => errors.push(String(e)));
      await page.goto(new URL(fsUrl(resolve(here, 'index.html')), server.resolvedUrls.local[0]).href);
      await page.evaluate(() => { window.exports = {}; document.body.style.cssText = 'margin:0;background:white'; });
      if (!jvmRoute) await page.addScriptTag({ url: new URL(fsUrl(modulePath), server.resolvedUrls.local[0]).href });
      const referenceResult = await page.evaluate(async ({ refUrl, threeUrl, adapterUrl, spec, referenceClient }) => {
        const ref = await import(refUrl), three = await import(threeUrl), { mountReferenceView } = await import(adapterUrl);
        const root = document.createElement('div'); document.body.append(root);
        const view = mountReferenceView(ref, three, root, spec, referenceClient); window.qualificationReference = view;
        const captures = []; const milliseconds = [];
        for (let i = 0; i < 3; i++) { const start = performance.now(); captures.push(view.capture()); milliseconds.push(performance.now() - start); }
        const picks = [];
        for (let row = 1; row <= 5; row++) for (let column = 1; column <= 5; column++) {
          const x = spec.width * column / 6, y = spec.height * row / 6;
          picks.push({ x, y, hit: view.pick(x, y) });
        }
        return { captures, milliseconds, picks, environment: { userAgent: navigator.userAgent, dpr: devicePixelRatio } };
      }, { refUrl: fsUrl(bundle), threeUrl: fsUrl(resolve(reference, 'node_modules/three/build/three.module.js')), adapterUrl: fsUrl(resolve(here, 'reference-view.mjs')), spec: prepared.spec, referenceClient });
      const ref = decode(referenceResult.captures[0].image);
      const repeated = referenceResult.captures.every(c => decode(c.image).data.equals(ref.data));
      const calibration = { referenceRepeatCount: 3, repeatPixelsIdentical: repeated, maximumUnlitInteriorChannelError: 2,
        minimumForegroundIoU: .995, maximumLandmarkPixels: 1, edgeEnvelopePixels: 0,
        warmNativeBudgetMs: 50, warmFallbackBudgetMs: 100, frozenBeforeCandidate: true, note: 'strict full-frame comparison; no candidate-derived exclusions' };
      await json(resolve(directory, 'reference-calibration.json'), calibration);
      await json(resolve(directory, 'reference-receipt.json'), { ...referenceResult, captures: referenceResult.captures.map(c => c.receipt) });
      // The original C07 policy was declared but not implemented by the old
      // adapter. Retain the actual reference behavior and fail that contract.
      const parameters = variant.effectiveInput.parameters;
      row.referenceMaterialContract = ['depthWrite', 'alphaDiscard'].filter(key => parameters[key] !== undefined).flatMap(key =>
        referenceResult.captures[0].receipt.materials.map(material => ({ surface: material.id, property: key,
          required: parameters[key], observed: material[key], pass: material[key] === parameters[key] })));
      const candidate = jvmRoute ? await (async () => {
        const path = resolve(jvmResults, id);
        const result = JSON.parse(await readFile(resolve(path, 'candidate.json')));
        if (result.inputSha256 !== row.inputSha256) throw new Error('JVM capture input checksum differs');
        if (result.status !== 'captured') throw new Error(result.failure ?? 'JVM capture failed');
        for (const name of ['native', 'raster']) if (result[name].status === 'captured') result[name].image = 'data:image/png;base64,' + (await readFile(resolve(path, name + '.png'))).toString('base64');
        return result;
      })() : await page.evaluate(async ({ threeUrl, spec, depthPolicy }) => {
        const three = await import(threeUrl), native = document.createElement('canvas'), raster = document.createElement('canvas');
        document.body.append(native, raster);
        const beforeHeap = performance.memory?.usedJSHeapSize ?? null;
        try { return { ...window.exports.captureScalafimQualification(three, native, raster, JSON.stringify(spec), depthPolicy), beforeHeap, afterHeap: performance.memory?.usedJSHeapSize ?? null }; }
        catch (error) { throw new Error(String(error)); }
        finally { native.remove(); raster.remove(); }
      }, { threeUrl: fsUrl(resolve(reference, 'node_modules/three/build/three.module.js')), spec: prepared.spec, depthPolicy });
      const referenceImage = PNG.sync.write(ref); await writeFile(resolve(directory, 'reference.png'), referenceImage); row.artifacts['reference.png'] = sha(referenceImage);
      row.comparisons = {};
      const images = [ref];
      for (const name of ['native', 'raster']) {
        const result = candidate[name];
        if (result.status !== 'captured') { row.comparisons[name] = { pass: false, failure: result.reason }; continue; }
        const image = decode(result.image), bytes = PNG.sync.write(image);
        await writeFile(resolve(directory, `${name}.png`), bytes); row.artifacts[`${name}.png`] = sha(bytes); images.push(image);
        row.comparisons[name] = compareRgba({ reference: ref.data, candidate: image.data, width: ref.width, height: ref.height, candidateWidth: image.width, candidateHeight: image.height,
          tolerances: { minimumForegroundIoU: calibration.minimumForegroundIoU, maximumChannelError: 2, colorPolicy: { calibrated: repeated && prepared.spec.profile.name === 'controlled-unlit' } } });
        delete result.image;
      }
      const diff = (col, row) => Math.max(...row.map((v, i) => Math.abs(v - col[(i % 4) * 4 + Math.floor(i / 4)])));
      row.camera = { viewMaxDifference: diff(referenceResult.captures[0].receipt.viewMatrix, candidate.viewMatrix), projectionMaxDifference: diff(referenceResult.captures[0].receipt.projectionMatrix, candidate.projectionMatrix) };
      if (prepared.spec.views && candidate.viewCameras) row.camera.views = referenceResult.captures[0].receipt.views.map(view => {
        const actual = candidate.viewCameras.find(v => v.id === view.id);
        return { id: view.id, viewMaxDifference: actual ? diff(view.viewMatrix, actual.viewMatrix) : null,
          projectionMaxDifference: actual ? diff(view.projectionMatrix, actual.projectionMatrix) : null };
      });
      row.runtimeErrors = errors; row.candidate = candidate;
      row.hostChecks = {
        runtimeBound: !jvmRoute || provenance.jvmRuntime?.schema === 'surface-jvm-runtime.v1',
        resizeBackExact: candidate.raster.resizeBackExact === true,
        disposedFrameReleased: candidate.disposal?.canvasPresentedFrameReleased === true,
        nativeResourcesReleased: jvmRoute || candidate.disposal?.backendResourceCount === 0,
        stableWarmImages: candidate.native.status !== 'captured' || candidate.native.warmImagesIdentical === true
      };
      if (variant.parent.rowId.endsWith('-volume')) {
        const expected = analyticVolumeSamples(prepared.spec);
        row.volumeSamples = { reference: compareVolumeSamples(expected, referenceResult.captures[0].receipt.volumeSamples),
          candidate: compareVolumeSamples(expected.map(value => ({ ...value, layer: `${value.surface}/${value.layer}` })), candidate.numericReadouts),
          identityPolicy: 'ScalaFIM layer IDs are explicitly namespaced by source surface' };
        await json(resolve(directory, 'volume-samples.json'), row.volumeSamples);
      }
      if (!jvmRoute && prepared.spec.surfaces.some(s => s.annotations?.length)) {
        const expected = annotationCenters(prepared.spec);
        row.annotationCenters = {reference:compareAnnotationCenters(expected,referenceResult.captures[0].receipt.markerCenters),
          candidate:compareAnnotationCenters(expected,candidate.markerCenters), toleranceWorldUnits:1e-4};
        await json(resolve(directory,'annotation-centers.json'),row.annotationCenters);
      }
      if (variant.parent.rowId.startsWith('C01') || variant.parent.rowId.startsWith('C04')) {
        const checks = candidate.picks.map((pick, index) => {
          const continuous = scientificPick(prepared.spec, pick.x, pick.y);
          const centre = scientificPick(prepared.spec, pick.x, pick.y, true);
          return { x: pick.x, y: pick.y,
            reference: compareScientificPick(continuous, referenceResult.picks[index].hit, 'euclidean'),
            native: candidate.native.status === 'captured' ? compareScientificPick(continuous, pick.native) : { unavailable: candidate.native.reason },
            raster: compareScientificPick(centre, pick.raster) };
        });
        row.scientificPicks = { policy: 'source ray oracle; native continuous, raster pixel-centre; reference Euclidean vertex, ScalaFIM largest barycentric vertex',
          pass: checks.every(c => c.reference.pass && c.raster.pass && (candidate.native.status !== 'captured' || c.native.pass)), checks };
        await json(resolve(directory, 'scientific-picks.json'), row.scientificPicks);
      }
      row.endpointStatus = errors.length === 0 && row.referenceMaterialContract.every(x => x.pass) &&
        Object.values(row.hostChecks).every(Boolean) &&
        (!row.volumeSamples || row.volumeSamples.reference.pass && row.volumeSamples.candidate.pass) &&
        (!row.annotationCenters || row.annotationCenters.reference.pass && row.annotationCenters.candidate.pass) &&
        (!row.scientificPicks || row.scientificPicks.pass) && Object.values(row.comparisons).every(x => x.pass) ? 'pass' : 'fail';
      row.remaining = ['independent per-element masks and scientific pick oracle', 'actual input-event replay', 'native appearance review', 'full family accounting'];
      const plate = new PNG({ width: images.reduce((n, x) => n + x.width, 0), height: Math.max(...images.map(x => x.height)) });
      let x = 0; for (const image of images) { PNG.bitblt(image, plate, 0, 0, image.width, image.height, x, 0); x += image.width; }
      const plateBytes = PNG.sync.write(plate); await writeFile(resolve(directory, 'side-by-side.png'), plateBytes); row.artifacts['side-by-side.png'] = sha(plateBytes);
    } catch (error) { row.failure = String(error); }
    finally {
      row.cleanupFailures = await cleanupOwned([
        ['reference', async () => { if (page) await page.evaluate(() => window.qualificationReference?.dispose()); }],
        ['page', async () => { if (page) await page.close(); }]
      ]);
      if (row.cleanupFailures.length) row.endpointStatus = 'fail';
    }
    await json(resolve(directory, 'receipt.json'), row);
    console.log(JSON.stringify({ id: row.id, endpoint: row.endpointStatus ?? 'unavailable', failure: row.failure, camera: row.camera,
      metrics: Object.fromEntries(Object.entries(row.comparisons ?? {}).map(([k, v]) => [k, v.metrics ?? v.failure])) }));
  }
} finally {
  provenance.cleanupFailures = await cleanupOwned([
    ['context', async () => { if (context) await context.close(); }],
    ['browser', async () => { if (browser) await browser.close(); }],
    ['server', async () => { if (server) await server.close(); }],
    ['audit', async () => { provenance.auditAfter = audit(); }]
  ]);
  provenance.finishedAt = new Date().toISOString(); await json(resolve(out, 'provenance.json'), provenance);
  if (provenance.cleanupFailures.length || rows.some(row => row.cleanupFailures?.length) || process.argv.includes('--require-qualified')) process.exitCode = 1;
  await json(resolve(out, 'results.json'), { status: 'fail', qualification: 'endpoint evidence only; mandatory missing checks remain failures', selected: selected.length, totalVariants: all.variants.length, rows });
  await writeFile(resolve(out, 'index.html'), '<!doctype html><meta charset="utf-8"><title>Surface qualification</title><style>body{font:16px system-ui;margin:2em}img{max-width:100%}pre{white-space:pre-wrap}</style><h1>Surface endpoint comparisons</h1><p>Left: SurfViewJS. Middle: native ScalaFIM, when admitted. Right: production Canvas raster host. Full qualification remains open.</p>' + rows.map(row => '<h2>' + row.id + '</h2>' + (row.artifacts['side-by-side.png'] ? '<img src="' + row.id.replaceAll('/', '--') + '/side-by-side.png">' : '') + '<pre>' + JSON.stringify({ endpoint: row.endpointStatus, failure: row.failure, camera: row.camera, remaining: row.remaining }, null, 2) + '</pre>').join(''));
}
