import test from 'node:test';
import assert from 'node:assert/strict';
import { cameraAction, decodeGifti, qualificationSpec, volumeFrame } from './qualification-specs.mjs';
import { loadQualificationContracts } from './qualification-contracts.mjs';
import { coreCortex, square2tri } from './fixtures.mjs';

test('camera actions preserve target/distance semantics without changing mesh', () => {
  const c = { eye: [3, 0, 0], target: [0, 0, 0], up: [0, 0, 1] };
  const moved = cameraAction(c, { azimuthDegrees: 90, eyeDistanceScale: .5, worldTranslation: [0, 12, 7] });
  assert.ok(Math.abs(moved.eye[0]) < 1e-12);
  assert.deepEqual(moved.eye.slice(1), [13.5, 7]);
  assert.deepEqual(c.eye, [3, 0, 0]);
});
test('external bytes require exact checked hash before decoding', () => assert.throws(() => decodeGifti(Buffer.from('x'), '0'.repeat(64)), /checksum/));
test('scalar boundary source values and NaN encoding are retained', async () => {
  const { variants } = await loadQualificationContracts();
  const v = variants.find(v => v.parent.rowId === 'C02-BR-scalar' && v.parent.checkpoint === 'threshold-equality');
  const { spec } = qualificationSpec(v);
  assert.deepEqual(spec.surfaces[0].layers[0].values, [-.25, -.2500000298023224, .25, .2500000298023224]);
  const n = qualificationSpec(variants.find(v => v.parent.rowId === 'C02-BR-scalar' && v.parent.checkpoint === 'NaN-mask')).spec;
  assert.equal(n.surfaces[0].layers[0].values[0], 'NaN');
});
test('key and publication checkpoints cannot silently degrade to surface captures', async () => {
  const { variants } = await loadQualificationContracts();
  for (const family of ['C10', 'C11', 'C12']) assert.throws(() => qualificationSpec(variants.find(v => v.parent.rowId.startsWith(family))), /requires/);
});
test('analytic volume uses x-fastest source order and the requested frame', () => {
  const input = { dims: [2, 3, 4, 3], frame: 2, values: 'i + 2*j + 3*k + 10*t' };
  const values = volumeFrame(input);
  assert.equal(values.length, 24);
  assert.deepEqual(values.slice(0, 6), [20, 21, 22, 23, 24, 25]);
  assert.equal(values[23], 34);
  assert.throws(() => volumeFrame({ ...input, frame: 3 }), /unsupported/);
});

test('four-view endpoints share sources and isolate independent camera changes', async () => {
  const cortex = Object.fromEntries(['left', 'right'].map(hemisphere => [hemisphere, coreCortex({ ...square2tri, hemisphere })]));
  const { variants } = await loadQualificationContracts();
  const make = checkpoint => qualificationSpec(variants.find(v => v.parent.rowId === 'C04-BR-layout' && v.parent.checkpoint === checkpoint), cortex).spec;
  const base = make('four-view'), moved = make('independent-camera'), linked = make('linked-camera');
  assert.equal(base.surfaces.length, 2);
  assert.deepEqual(base.views.map(v => v.surfaces), [['left'], ['right'], ['left'], ['right']]);
  assert.deepEqual(base.views.map(v => v.viewport), [[0, 0, .5, .5], [.5, 0, .5, .5], [0, .5, .5, .5], [.5, .5, .5, .5]]);
  assert.notDeepEqual(moved.views[0].camera, base.views[0].camera);
  for (let i = 1; i < 4; i++) assert.deepEqual(moved.views[i].camera, base.views[i].camera);
  for (let i = 0; i < 4; i++) assert.notDeepEqual(linked.views[i].camera, base.views[i].camera);
});
