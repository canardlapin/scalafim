import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import test from 'node:test';
import { galleryContracts } from './gallery.mjs';

const manifest = JSON.parse(await readFile(new URL('./manifest.json', import.meta.url), 'utf8'));
test('every required checkpoint has finite camera inputs, required elements and a failing pending calibration', () => {
  const contracts = galleryContracts(manifest);
  assert.equal(contracts.length, manifest.requiredMatrix.reduce((n, row) => n + row.checkpoints.length, 0));
  for (const row of contracts) {
    assert(row.camera.eye.every(Number.isFinite));
    assert(row.camera.aspect > 0);
    assert(row.requiredElements.length > 0);
    assert.match(row.metricPolicy.calibration, /pending/);
    assert.equal(row.profile.ambientIntensity, Math.PI);
  }
});
test('new checkpoints cannot inherit a default success-shaped contract', () => {
  const changed = structuredClone(manifest);
  changed.requiredMatrix[7].checkpoints.push('unimplemented-operation');
  assert.throws(() => galleryContracts(changed), /unknown checkpoint/);
  assert.throws(() => galleryContracts({ requiredMatrix: [] }), /empty gallery/);
  const duplicate = structuredClone(manifest);
  duplicate.requiredMatrix.push(duplicate.requiredMatrix[0]);
  assert.throws(() => galleryContracts(duplicate), /duplicate checkpoint/);
});
test('threshold boundaries are adjacent Float32 inputs and exports have equal aspect', () => {
  const rows = galleryContracts(manifest);
  const boundary = rows.find(row => row.rowId === 'C02-BR-scalar' && row.checkpoint === 'threshold-equality');
  assert.deepEqual(boundary.parameters.values.map(Math.fround), boundary.parameters.values);
  assert(boundary.parameters.values[1] < -.25);
  assert(boundary.parameters.values[3] > .25);
  const sizes = rows.filter(row => row.rowId === 'C12-PUB-restore' && row.checkpoint.startsWith('export'));
  assert.deepEqual(sizes.map(row => row.viewport.size), [[1200, 900], [2400, 1800]]);
});
