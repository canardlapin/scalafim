import assert from 'node:assert/strict';
import test from 'node:test';
import { compareRgba } from './compare.mjs';

const width = 512;
const height = 512;
const pixels = width * height;
const white = [255, 255, 255, 255];

function image() {
  const value = new Uint8Array(pixels * 4);
  for (let index = 0; index < value.length; index += 4) value.set(white, index);
  return value;
}
function mask() { return new Uint8Array(pixels); }
function fill(imageData, x, y, w, h, color, elementMask) {
  for (let row = y; row < y + h; row += 1) for (let col = x; col < x + w; col += 1) {
    const pixel = row * width + col;
    imageData.set(color, pixel * 4);
    if (elementMask) elementMask[pixel] = 1;
  }
}
function request(reference, candidate, expectedElements) {
  return compareRgba({ reference, candidate, width, height, expectedElements, tolerances: { minimumForegroundIoU: .995, maximumChannelError: 2, background: white, colorPolicy: { calibrated: true } } });
}

test('deleting a 2x2 required marker fails despite global IoU above .995', () => {
  const reference = image(); const candidate = image(); const marker = mask(); const body = mask();
  fill(reference, 100, 100, 300, 300, [20, 40, 200, 255], body);
  fill(candidate, 100, 100, 300, 300, [20, 40, 200, 255], body);
  fill(reference, 250, 250, 2, 2, [255, 0, 0, 255], marker);
  const result = request(reference, candidate, [{ id: 'marker', mask: marker, minCoverage: 1, expectedColor: [255, 0, 0, 255] }]);
  assert(result.metrics.foregroundIoU > .995);
  assert.equal(result.pass, false);
  assert(result.failures.some(value => value.code === 'element-coverage-below-minimum' && value.id === 'marker'));
});

test('removing a one-pixel thin contour fails its independent required mask', () => {
  const reference = image(); const candidate = image(); const contour = mask();
  fill(reference, 100, 100, 200, 1, [0, 0, 0, 255], contour);
  const result = request(reference, candidate, [{ id: 'contour', mask: contour, minCoverage: 1, expectedColor: [0, 0, 0, 255] }]);
  assert.equal(result.pass, false);
  assert(result.failures.some(value => value.code === 'element-coverage-below-minimum' && value.id === 'contour'));
});

test('a three-pixel shift fails the fixed object mask', () => {
  const reference = image(); const candidate = image(); const object = mask();
  fill(reference, 100, 100, 40, 40, [0, 100, 255, 255], object);
  fill(candidate, 103, 100, 40, 40, [0, 100, 255, 255]);
  const result = request(reference, candidate, [{ id: 'geometry', mask: object, minCoverage: .995, expectedColor: [0, 100, 255, 255] }]);
  assert.equal(result.pass, false);
  assert(result.failures.some(value => value.code === 'element-coverage-below-minimum' && value.id === 'geometry'));
});

test('blank frames cannot pass a required object check', () => {
  const object = mask(); fill(image(), 8, 8, 1, 1, [0, 0, 0, 255], object);
  const result = request(image(), image(), [{ id: 'required-object', mask: object, minCoverage: 1 }]);
  assert.equal(result.pass, false);
  assert(result.failures.some(value => value.code === 'reference-foreground-empty'));
  assert(result.failures.some(value => value.code === 'required-reference-element-missing' && value.id === 'required-object'));
});

test('dimension mismatch and uncalibrated color are formal failures', () => {
  const result = compareRgba({ reference: image(), candidate: image(), width, height, candidateWidth: 511, candidateHeight: height, tolerances: { colorPolicy: { calibrated: false } } });
  assert.equal(result.pass, false);
  assert.equal(result.failures[0].code, 'dimension-mismatch');
  const noPolicy = compareRgba({ reference: image(), candidate: image(), width, height });
  assert(noPolicy.failures.some(value => value.code === 'uncalibrated-color-policy'));
});

test('identical valid images and independently declared marker pass', () => {
  const reference = image(), marker = mask();
  fill(reference, 250, 250, 2, 2, [255, 0, 0, 255], marker);
  const result = request(reference, reference.slice(), [{id:'marker',mask:marker,expectedColor:[255,0,0,255]}]);
  assert.equal(result.pass,true);
  assert.equal(result.metrics.foregroundIoU,1);
});
