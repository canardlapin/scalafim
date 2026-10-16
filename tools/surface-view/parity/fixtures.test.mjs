import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import test from 'node:test';
import { cameraFor, coreCortex, crossLabelMeshEdges, fixtureSpecs, galleryControls, landmarkProjection, linearVolume, nearestAnchors, polygonVertexSelection, scalarLut, square2tri, tetra } from './fixtures.mjs';
function nextFloat32(value, direction) {
    const view = new DataView(new ArrayBuffer(4));
    view.setFloat32(0, value, false);
    let bits = view.getUint32(0, false);
    bits += direction > 0 ? (value >= 0 ? 1 : -1) : (value >= 0 ? -1 : 1);
    view.setUint32(0, bits, false);
    return view.getFloat32(0, false);
}
test('every manifest fixture has a contract and retains the asymmetric smoke tetra', async () => { const m = JSON.parse(await readFile(new URL('./manifest.json', import.meta.url), 'utf8')); for (const id of new Set(m.requiredMatrix.map(r => r.fixture)))
    assert(fixtureSpecs[id], id); assert.deepEqual(tetra.vertices[1], [1.4, .1, .2]); });
test('camera recipes are mm-space with separate orthographic inputs', () => { const left = cameraFor('leftLateral'); assert.deepEqual(left.eye, [-350, -20, 20]); assert.equal(left.near, .1); assert.equal(left.far, 2000); const corticalLandmark = landmarkProjection([-30, -10, 20], left); assert(corticalLandmark.every(v => v > 0 && v < 1)); const analytic = cameraFor('analytic-dorsal', 'square'); const p = landmarkProjection([1.6, .375, .65], analytic); assert(Math.abs(p[0] - .8968) < 1e-3); assert(Math.abs(p[1] - .5) < 1e-12); const ortho = cameraFor('dorsal', 'wide', undefined, 'orthographic'); assert.deepEqual(landmarkProjection(ortho.target, ortho), [.5, .5]); });
test('volume and scalar contract covers Float32 neighbors, disabled zero-width threshold, NaN, and LUT interiors', () => { assert.equal(linearVolume(1, 2, 3, 4), 54); const e = Math.fround(-.25), below = nextFloat32(e, -1); assert.deepEqual(scalarLut(e, { threshold: [e, .25] }), [0, 0, 0, 0]); assert.notDeepEqual(scalarLut(below, { threshold: [e, .25] }), [0, 0, 0, 0]); assert.deepEqual(scalarLut(.2, { colors: [[1, 1, 1, 255], [2, 2, 2, 255], [3, 3, 3, 255], [4, 4, 4, 255]] }), [2, 2, 2, 255]); assert.deepEqual(scalarLut(NaN), [0, 0, 0, 0]); assert.notDeepEqual(scalarLut(.2, { threshold: [.2, .2] }), [0, 0, 0, 0]); });
test('mesh-edge outline and inclusive polygon selection are input oracles', () => { assert.deepEqual(crossLabelMeshEdges(square2tri.faces, [1, 1, 2, 2]), [[1, 2], [0, 2], [1, 3]]); const selected = polygonVertexSelection(square2tri.vertices, galleryControls.polygonRoi); assert(selected.length > 0); assert(selected.includes(0)); });
test('cortex fields, frame-major time, labels and expansion remain deterministic synthetic inputs', () => { const c = coreCortex({ ...square2tri, hemisphere: 'left' }); assert.equal(c.fields.time.length, 3); assert(c.fields.time.every(frame => frame.length === 4 && frame.every(v => v >= -3 && v <= 3))); assert.equal(c.medialWallLabel, 0); assert.equal(c.curvature.length, 4); assert.deepEqual(c.geometryVariant.faces, square2tri.faces); assert(c.geometryVariant.vertices[0].every((v, i) => Math.abs(v - ([4.8, 2.8, -4][i])) < 1e-12)); assert.equal(c.geometryVariant.scientific, false); assert.deepEqual(c.networkEdges.map(x => x[2]), [.8, -.6, .4, -.2]); assert.throws(() => coreCortex({ vertices: [], faces: [], hemisphere: 'left' })); assert.throws(() => nearestAnchors([], [])); });
