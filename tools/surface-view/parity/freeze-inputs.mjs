import { createHash } from 'node:crypto';
import { readFile, writeFile } from 'node:fs/promises';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { galleryContracts } from './gallery.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const manifest = JSON.parse(await readFile(resolve(here, 'manifest.json'), 'utf8'));
const assets = manifest.requiredAssets.bilateralCortex;
const sourceHashes = {};
for (const path of ['manifest.json', 'fixtures.mjs', 'gallery.mjs']) {
  sourceHashes[path] = hash(await readFile(resolve(here, path)));
}
for (const hemisphere of ['left', 'right']) {
  const expected = assets[hemisphere];
  const actual = hash(await readFile(resolve(assets.localRoot, expected.file)));
  if (actual !== expected.sha256) throw new Error(`${hemisphere} input hash differs from pinned manifest`);
  sourceHashes[expected.file] = actual;
}
const contracts = galleryContracts(manifest);
const output = {
  schema: 'scalafim.surface-gallery-inputs.v1',
  status: 'input-contract-only; renderer and visual qualification pending',
  referenceCommit: manifest.reference.commit, sourceHashes,
  contractsSha256: hash(JSON.stringify(contracts)), contracts
};
const file = resolve(process.argv[2] ?? resolve(here, 'frozen-inputs.json'));
await writeFile(file, `${JSON.stringify(output, null, 2)}\n`);
console.log(JSON.stringify({ file, checkpoints: contracts.length, sha256: hash(JSON.stringify(output)) }));
