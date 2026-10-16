import assert from 'node:assert/strict';
import test from 'node:test';
import { scientificPick, compareScientificPick, analyticVolumeSamples, compareVolumeSamples, annotationCenters, compareAnnotationCenters } from './scientific-oracle.mjs';
const spec = { width: 100, height: 100, camera: { projection: 'orthographic', eye: [0, 0, 4], target: [0, 0, 0], up: [0, 1, 0], left: -1, right: 3, bottom: -1, top: 3, near: .1, far: 10 },
  surfaces: [{ id: 'a', vertices: [[0, 0, 0], [2, 0, 0], [0, 2, 0]], faces: [[0, 1, 2]], layers: [] }] };
test('analytic volume oracle distinguishes voxel kernel, translation and source identity', () => {
  const input = { surfaces: [{ id: 'volume-plane', vertices: [[-.75, -.5, -.25], [10, 0, 0]], layers: [{
    id: 'volume', kind: 'volume', values: 'i + 2*j + 3*k + 10*t', matrixOrder: 'row-major',
    voxelToWorld: [1, 0, 0, -1, 0, 1, 0, -1, 0, 0, 1, -1, 0, 0, 0, 1], dims: [3, 4, 5, 2], frame: 1, interpolation: 'linear'
  }] }] };
  const linear = analyticVolumeSamples(input);
  assert.deepEqual(linear[0].values, [13.5, null]);
  input.surfaces[0].layers[0].interpolation = 'nearest';
  assert.deepEqual(analyticVolumeSamples(input)[0].values, [15, null]);
  assert.equal(compareVolumeSamples(linear, structuredClone(linear)).pass, true);
  assert.equal(compareVolumeSamples(linear, [{ ...linear[0], values: [0, null] }]).pass, false);
  assert.equal(compareVolumeSamples(linear, [{ ...linear[0], surface: 'wrong' }]).pass, false);
  assert.equal(compareVolumeSamples(linear, []).pass, false);
});
test('volume oracle solves a rotated sheared affine from columns', () => {
  const layer = { id: 'v', kind: 'volume', values: 'i + 2*j + 3*k + 10*t', matrixOrder: 'row-major',
    voxelToWorld: [0, -2, 1, 7, 2, 0, 0, -3, 0, 0, 3, 2, 0, 0, 0, 1], dims: [3, 4, 5, 1], frame: 0, interpolation: 'linear' };
  // Source index (.25,.5,1.75) maps to (7.75,-2.5,7.25).
  assert.deepEqual(analyticVolumeSamples({ surfaces: [{ id: 's', vertices: [[7.75, -2.5, 7.25]], layers: [layer] }] })[0].values, [6.5]);
});
test('explicit asymmetric camera gives an independently known intersection', () => {
  const hit = scientificPick(spec, 37.5, 62.5);
  assert.deepEqual(hit.world, [.5, .5, 0]);
  assert.deepEqual(hit.weights, [.5, .25, .25]);
  assert.equal(hit.vertex, 0);
  assert.equal(scientificPick(spec, 0, 0), null);
});
test('raster pixel centre is distinct from continuous native coordinates', () => {
  const hit = scientificPick(spec, 37.5, 62.1, true);
  assert.deepEqual(hit.world, [.5, .5, 0]);
  assert.notDeepEqual(scientificPick(spec, 37.5, 62.1).world, hit.world);
});
test('perspective, near/far rejection, clipping and original IDs', () => {
  const p = structuredClone(spec);
  p.camera = { ...p.camera, projection: 'perspective', aspect: 1, fovDegrees: 90 };
  const hit = scientificPick(p, 56.25, 43.75);
  assert.ok(Math.abs(hit.world[0] - .5) < 1e-12);
  p.surfaces[0].clip = { normal: [1, 0, 0], point: [1, 0, 0] };
  assert.equal(scientificPick(p, 56.25, 43.75), null);
  p.surfaces[0].clip.flip = true;
  assert.equal(scientificPick(p, 56.25, 43.75).surface, 'a');
  p.camera.far = 3;
  assert.equal(scientificPick(p, 56.25, 43.75), null);
});
test('nearest hit is independent of surface submission order; wrong scientific IDs fail', () => {
  const near = { ...spec.surfaces[0], id: 'near', vertices: [[0, 0, 1], [2, 0, 1], [0, 2, 1]] };
  for (const surfaces of [[near, ...spec.surfaces], [...spec.surfaces, near]]) {
    const hit = scientificPick({ ...spec, surfaces }, 37.5, 62.5);
    assert.equal(hit.surface, 'near');
    assert.equal(compareScientificPick(hit, { surface: 'a', face: 0, vertex: 0 }).pass, false);
    assert.equal(compareScientificPick(hit, { surface: 'near', face: 0, vertex: 1 }).pass, false);
    assert.equal(compareScientificPick(hit, { surface: 'near', face: 0, vertex: 0 }).pass, true);
  }
});

test('annotation oracle rejects stale morph anchors, missing markers and wrong IDs', () => {
  const input={surfaces:[{id:'left',vertices:[[1,2,3]],morph:{weight:.5,vertices:[[5,4,7]]},annotations:[{id:'m',vertex:0}]}]};
  const expected=annotationCenters(input);
  assert.deepEqual(expected[0].world,[3,3,5]);
  assert.equal(compareAnnotationCenters(expected,expected).pass,true);
  assert.equal(compareAnnotationCenters(expected,[{...expected[0],world:[1,2,3]}]).pass,false);
  assert.equal(compareAnnotationCenters(expected,[]).pass,false);
  assert.equal(compareAnnotationCenters(expected,[{...expected[0],id:'wrong'}]).pass,false);
});
