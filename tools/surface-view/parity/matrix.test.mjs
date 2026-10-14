import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import test from 'node:test';
import { validateRequiredMatrix } from './matrix.mjs';

const row = { id: 'small', fixture: 'smallfixture', route: 'browser viewer', checkpoints: ['cold', 'resize'] };
const manifest = { requiredMatrix: [row] };
const frozen = {
  sourceCommit: 'a'.repeat(40),
  fixtureHashes: { smallfixture: 'b'.repeat(64) },
  artifactHashes: {
    small: { cold: { source: 'c'.repeat(64), candidate: 'd'.repeat(64) }, resize: { source: 'e'.repeat(64), candidate: 'f'.repeat(64) } }
  },
  calibrations: { small: { cold: '1'.repeat(64), resize: '2'.repeat(64) } },
  requiredElements: { small: { cold: ['large-object', 'marker'], resize: ['large-object', 'marker'] } },
  approvedActualRenderers: { 'browser viewer': ['Three.WebGLRenderer'] }
};

function receipt(checkpoint, overrides = {}) {
  return {
    rowId: 'small', checkpoint, productRoute: 'browser viewer', actualRenderer: 'Three.WebGLRenderer',
    fixtureHash: 'b'.repeat(64), sourceCommit: 'a'.repeat(40),
    artifactHashes: frozen.artifactHashes.small[checkpoint],
    calibration: { id: frozen.calibrations.small[checkpoint], calibrated: true },
    semantics: true, visual: true, interaction: true, resource: true,
    elementEvidence: { 'large-object': true, marker: true }, verdict: 'pass', ...overrides
  };
}
function validate(receipts) { return validateRequiredMatrix({ manifest, receipts, frozen }); }
function has(result, code) { return result.errors.some(value => value.code === code); }

test('a complete calibrated small fixture passes', () => {
  const result = validate([receipt('cold'), receipt('resize')]);
  assert.equal(result.ok, true);
  assert.equal(result.expected, 2);
});

test('a missing checkpoint receipt fails the complete required row', () => {
  const result = validate([receipt('cold')]);
  assert.equal(result.ok, false);
  assert(has(result, 'missing-receipt'));
});

test('duplicate, stale, renderer-substituted and caveated receipts fail', () => {
  let result = validate([receipt('cold'), receipt('cold'), receipt('resize')]);
  assert(has(result, 'duplicate-receipt'));
  result = validate([receipt('cold', { sourceCommit: '0'.repeat(40) }), receipt('resize')]);
  assert(has(result, 'stale-source-commit'));
  result = validate([receipt('cold', { actualRenderer: 'portable-raster' }), receipt('resize')]);
  assert(has(result, 'renderer-substituted'));
  result = validate([receipt('cold', { verdict: 'passWithCaveats' }), receipt('resize')]);
  assert(has(result, 'non-clean-verdict'));
});

test('an unexpected row or checkpoint receipt fails', () => {
  const result = validate([receipt('cold'), receipt('resize'), receipt('other', { rowId: 'unknown' })]);
  assert(has(result, 'unexpected-receipt'));
});

test('missing per-element evidence and calibration fail clean-pass admission', () => {
  let result = validate([receipt('cold', { elementEvidence: { 'large-object': true } }), receipt('resize')]);
  assert(has(result, 'missing-element-evidence'));
  result = validate([receipt('cold', { calibration: { id: '1'.repeat(64), calibrated: false } }), receipt('resize')]);
  assert(has(result, 'uncalibrated-receipt'));
});

test('removing both frozen and receipt identities cannot compare as undefined', () => {
  const incomplete = { ...frozen, sourceCommit: undefined, fixtureHashes: {}, calibrations: {}, requiredElements: {} };
  const stripped = receipt('cold', { sourceCommit: undefined, fixtureHash: undefined, calibration: { id: undefined, calibrated: true }, elementEvidence: {} });
  const result = validateRequiredMatrix({ manifest, receipts: [stripped, receipt('resize')], frozen: incomplete });
  assert(has(result, 'invalid-frozen-source-commit'));
  assert(has(result, 'invalid-frozen-fixture-hash'));
  assert(has(result, 'invalid-frozen-calibration'));
  assert(has(result, 'fixture-hash-mismatch'));
  assert(has(result, 'stale-source-commit'));
});

test('empty artifacts and malformed required contracts fail before admission', () => {
  let altered = receipt('cold', { artifactHashes: {} });
  let result = validate([altered, receipt('resize')]);
  assert(has(result, 'artifact-hashes-mismatch'));
  const malformed = { requiredMatrix: [{ id: ' ', fixture: 'smallfixture', route: '', checkpoints: [] }] };
  result = validateRequiredMatrix({ manifest: malformed, receipts: [], frozen: {} });
  assert(has(result, 'invalid-required-row'));
  const emptyCheckpoints = { requiredMatrix: [{ id: 'valid', fixture: 'smallfixture', route: 'browser viewer', checkpoints: [] }] };
  result = validateRequiredMatrix({ manifest: emptyCheckpoints, receipts: [], frozen: {} });
  assert(has(result, 'empty-required-checkpoints'));
  result = validateRequiredMatrix({ manifest: { requiredMatrix: [] }, receipts: [], frozen: {} });
  assert(has(result, 'empty-required-matrix'));
});

test('the current prospective manifest with zero receipts remains failed', async () => {
  const current = JSON.parse(await readFile(new URL('./manifest.json', import.meta.url), 'utf8'));
  const result = validateRequiredMatrix({ manifest: current, receipts: [], frozen: {} });
  assert.equal(result.ok, false);
  assert(result.expected > 0);
  assert(has(result, 'missing-receipt'));
});
