// Run after CorticalMapBrowserFixture and surfaceViewExamplesJS/fastLinkJS
// (NoModule linker). Uses an explicitly supplied project Playwright install.
// Browser ownership audits are run by the invoking session before and after.
import { createServer } from 'node:http';
import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { resolve, dirname } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { createHash } from 'node:crypto';
const [fixturePath, outputPath, dependencyRoot, selected = 'Scalar,Layered,CurvatureLayered,Nearest,Face', diagnosticStage = '', configuration = 'all', rendererBackend = 'default'] = process.argv.slice(2);
if (!fixturePath || !outputPath || !dependencyRoot) throw new Error('Expected fixture.json output-directory project-node_modules [modes]');
const root = resolve(dirname(fileURLToPath(import.meta.url)), '../../..');
const { chromium } = await import(pathToFileURL(resolve(dependencyRoot, 'playwright/index.mjs')));
const output = resolve(outputPath);
await mkdir(output, { recursive: true });
const linked = resolve(root, 'examples/surface-view/js/target/scala-3.7.4/scalafim-examples-surface-view-fastopt/main.js');
const routes = new Map([
  ['/', [resolve(root, 'examples/surface-view/browser/cortical-maps.html'), 'text/html']],
  ['/main.js', [linked, 'text/javascript']],
  ['/fixture.json', [resolve(fixturePath), 'application/json']],
  ['/legacy.html', [resolve(root, 'modules/surface-view-three/browser/index.html'), 'text/html']],
  ['/three/three.module.js', [resolve(dependencyRoot, 'three/build/three.module.js'), 'text/javascript']],
  ['/three/three.core.js', [resolve(dependencyRoot, 'three/build/three.core.js'), 'text/javascript']]
]);
const server = createServer(async (req, res) => {
  if (req.url === '/favicon.ico') { res.writeHead(204); res.end(); return; }
  const route = routes.get(req.url);
  if (!route) { res.writeHead(404); res.end(); return; }
  try {
    let body = await readFile(route[0]);
    if (req.url === '/legacy.html') body = Buffer.from(body.toString()
      .replace('../js/target/scala-3.7.4/scalafim-surface-view-three-fastopt/main.js', '/main.js')
      .replace('../../../../../jscode/surfviewjs/node_modules/three/build/three.module.js', '/three/three.module.js'));
    res.writeHead(200, { 'Content-Type': route[1] }); res.end(body);
  }
  catch (e) { res.writeHead(500); res.end(String(e)); }
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
let browser;
let page;
const errors = [];
const trace = [];
const receipt = { schema: 'scalafim.surface-cortical-webgl.v1', status: 'running',
  rendererBackend,
  startedAt: new Date().toISOString(), linkedSha256: createHash('sha256').update(await readFile(linked)).digest('hex'),
  cases: [], errors };
const save = () => writeFile(resolve(output, 'native.json'), JSON.stringify(receipt, null, 2) + '\n');
try {
  if (!['default', 'metal', 'no-antialias'].includes(rendererBackend)) throw new Error(`Unknown renderer backend: ${rendererBackend}`);
  browser = await chromium.launch({ headless: true, args: rendererBackend === 'metal' ? ['--use-gl=angle', '--use-angle=metal'] : [] });
  receipt.browser = browser.version();
  receipt.browserInstallRoot = process.env.PLAYWRIGHT_BROWSERS_PATH || 'Playwright default cache';
  const context = await browser.newContext({ viewport: { width: 1200, height: 1200 }, deviceScaleFactor: 1 });
  page = await context.newPage();
  page.on('pageerror', e => errors.push(String(e.stack || e)));
  page.on('console', msg => {
    trace.push(msg.text());
    if (msg.type() === 'error') errors.push(msg.text());
    if (['cortical_outlier=', 'cortical_neighbors=', 'cortical_gpu_id='].some(prefix => msg.text().startsWith(prefix))) console.log(msg.text());
    if (msg.text().startsWith('cortical_stage=')) {
      const row = JSON.parse(msg.text().slice('cortical_stage='.length));
      console.log(`STAGE ${row.action} ${row.width}x${row.height} checked=${row.checkedPixels} error=${row.maximumChannelError} IoU=${row.maskIoU}`);
    }
  });
  await page.goto(`http://127.0.0.1:${server.address().port}/`);
  await page.waitForFunction(() => document.body.dataset.ready || document.body.dataset.error, { timeout: 60000 });
  const loadError = await page.evaluate(() => document.body.dataset.error);
  if (loadError) throw new Error(loadError);
  receipt.corpusSha256 = await page.evaluate(() => window.corticalSourceHashes);
  receipt.threeRevision = await page.evaluate(() => window.threeRevision);
  if (rendererBackend === 'no-antialias') await page.evaluate(() => document.querySelector('canvas').getContext('webgl2', { antialias: false, alpha: false }));
  await save();
  for (const mode of selected === 'legacy' ? [] : selected.split(',')) for (const perspective of [false, true]) for (const lit of [false, true]) {
    if (configuration !== 'all' && configuration !== `${perspective ? 'perspective' : 'ortho'}-${lit ? 'lit' : 'unlit'}`) continue;
    receipt.activeCase = { mode, perspective, lit };
    console.log(`START ${mode} perspective=${perspective} lit=${lit}`);
    const result = await page.evaluate(([mode, perspective, lit, diagnosticStage]) => window.runCorticalCase(mode, perspective, lit, diagnosticStage), [mode, perspective, lit, diagnosticStage]);
    for (const image of result.images) {
      await writeFile(resolve(output, `${mode}-${perspective ? 'perspective' : 'ortho'}-${lit ? 'lit' : 'unlit'}-${image.action}.png`), Buffer.from(image.image.split(',')[1], 'base64'));
    }
    delete result.images;
    receipt.cases.push(result);
    receipt.webgl = await page.evaluate(() => {
      const gl = document.querySelector('canvas').getContext('webgl2');
      const debug = gl.getExtension('WEBGL_debug_renderer_info');
      return { version: gl.getParameter(gl.VERSION), samples: gl.getParameter(gl.SAMPLES),
        vendor: debug ? gl.getParameter(debug.UNMASKED_VENDOR_WEBGL) : gl.getParameter(gl.VENDOR),
        renderer: debug ? gl.getParameter(debug.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER) };
    });
    await save();
    console.log(`PASS ${mode} perspective=${perspective} lit=${lit} stages=${result.cases.length}`);
  }
  if (selected === 'legacy') {
    console.log('START existing browser harness');
    await page.goto(`http://127.0.0.1:${server.address().port}/legacy.html`);
    await page.waitForFunction(() => document.body.dataset.scalafimThree, undefined, { timeout: 600000 });
    const status = await page.evaluate(() => document.body.dataset.scalafimThree);
    const result = await page.locator('#receipt').textContent();
    if (status !== 'pass') throw new Error(`Existing browser harness failed: ${result}`);
    receipt.legacy = JSON.parse(result);
    receipt.legacyMatrix = await page.evaluate(() => window.scalafimThreeAdmissionMatrix);
    await page.locator('#surface').screenshot({ path: resolve(output, 'legacy.png') });
    console.log('PASS existing browser harness');
  }
  if (errors.length) throw new Error(`Browser errors: ${errors.join('\n')}`);
  receipt.status = 'completed';
  delete receipt.activeCase;
  receipt.completedAt = new Date().toISOString();
  await context.close();
} catch (error) {
  if (page) {
    const image = await page.evaluate(() => window.corticalFailureImage).catch(() => null);
    if (image) await writeFile(resolve(output, 'failure.png'), Buffer.from(image.split(',')[1], 'base64'));
    const geometry = await page.evaluate(() => window.corticalFailureGeometry).catch(() => null);
    if (geometry) await writeFile(resolve(output, 'failure-geometry.json'), JSON.stringify(geometry, null, 2) + '\n');
    receipt.failureWebgl = await page.evaluate(() => {
      const gl = document.querySelector('canvas')?.getContext('webgl2');
      if (!gl) return null;
      const debug = gl.getExtension('WEBGL_debug_renderer_info');
      return { version: gl.getParameter(gl.VERSION), samples: gl.getParameter(gl.SAMPLES),
        contextAttributes: gl.getContextAttributes(), depthBits: gl.getParameter(gl.DEPTH_BITS),
        depthTest: gl.isEnabled(gl.DEPTH_TEST), depthFunction: gl.getParameter(gl.DEPTH_FUNC),
        depthWrite: gl.getParameter(gl.DEPTH_WRITEMASK), viewport: Array.from(gl.getParameter(gl.VIEWPORT)),
        renderer: debug ? gl.getParameter(debug.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER) };
    }).catch(() => null);
  }
  receipt.status = 'failed'; receipt.failure = String(error.stack || error); throw error;
} finally {
  if (browser) await browser.close();
  await new Promise(resolve => server.close(resolve));
  receipt.resourcesClosed = true;
  await writeFile(resolve(output, 'run.log'), trace.join('\n') + '\n');
  await save();
}
