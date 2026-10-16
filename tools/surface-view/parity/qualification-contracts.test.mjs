import assert from 'node:assert/strict';
import test from 'node:test';
import { createHash } from 'node:crypto';
import { enumerateQualificationContracts, loadQualificationContracts } from './qualification-contracts.mjs';
const row = { id: 'C01-x', fixture: 'f', route: 'r', viewport: '10x10', checkpoints: ['a'] };
const c = { rowId: 'C01-x', checkpoint: 'a', fixture: 'f', productRoute: 'r', viewport: { size: [10, 10] }, parameters: { views: ['leftLateral'], projections: ['perspective', 'orthographic'], profiles: ['observedDefault', 'controlledUnlit'] }, requiredElements: ['surface-mask'] };
const frozen = contracts => ({ contractsSha256: createHash('sha256').update(JSON.stringify(contracts)).digest('hex'), sourceHashes: { source: 'x' }, contracts });
const run = (contracts = [c], rows = [row]) => enumerateQualificationContracts({ manifest: { requiredMatrix: rows }, frozen: frozen(contracts) });
test('enumerates required bundles and supplementary lit captures deterministically', () => {
  assert.deepEqual(run(), run()); assert.equal(run().variants.length, 6);
  assert.equal(run().variants.filter(x => x.requirement === 'required').length, 4);
});
test('keeps contradictory viewport as an additional named variant', () => {
  const x = run([c], [{ ...row, viewport: '20x30' }]); assert.equal(x.variants.length, 12);
  assert.deepEqual([...new Set(x.variants.map(x => x.effectiveInput.viewport.source))].sort(), ['frozen', 'manifest']);
});
test('missing, extra and duplicate checkpoints cannot reduce required coverage', () => {
  assert.throws(() => run([]), /missing frozen checkpoint/);
  assert.throws(() => run([c, c]), /duplicate frozen checkpoint/);
  assert.throws(() => run([{ ...c, checkpoint: 'unknown' }]), /unknown/);
  assert.throws(() => run([{ ...c, requiredElements: [] }]), /missing required elements/);
});
test('frozen hash binds original JSON.stringify serialization', () => {
  const changed = frozen([c]); changed.contractsSha256 = '0'.repeat(64);
  assert.throws(() => enumerateQualificationContracts({ manifest: { requiredMatrix: [row] }, frozen: changed }), /hash mismatch/);
});
test('unknown and empty bundles cannot silently vanish', () => {
  for (const field of ['views', 'projections', 'profiles']) for (const values of [[], ['unknown']])
    assert.throws(() => run([{ ...c, parameters: { ...c.parameters, [field]: values } }]));
});
test('actual checkpoint parameters declare presence and absence', () => {
  for (const [family, parameters, element] of [['C06', { showCrosshair: false }, 'crosshair'], ['C09', { showNodes: false }, 'node-glyphs'], ['C11', { linkedMarker: false }, 'linked-marker']]) {
    const contract = { ...c, rowId: `${family}-x`, parameters, requiredElements: [element] };
    assert.equal(run([contract], [{ ...row, id: contract.rowId }]).variants[0].expectedElements[element].expected, 'absent');
  }
});
test('invalid legend original and valid positive erratum are both mandatory', () => {
  const contract = { ...c, rowId: 'C10-x', checkpoint: 'window-change', parameters: { mapping: { range: [-1, 2] }, ticks: [-2, 0, 3] } };
  const result = run([contract], [{ ...row, id: contract.rowId, checkpoints: [contract.checkpoint] }]);
  assert.equal(result.variants.length, 2); assert.equal(result.variants[0].diagnostics[0].code, 'legend-ticks-outside-window');
  assert.deepEqual(result.variants[0].effectiveInput.parameters.ticks, [-2, 0, 3]);
  assert.deepEqual(result.variants[1].effectiveInput.parameters.ticks, [-1, 0, 2]);
  assert.equal(result.variants[1].originalInputHash, result.variants[0].effectiveInputHash);
});
test('all 151 original checkpoint bundles are accounted for', async () => {
  const out = await loadQualificationContracts();
  assert.equal(new Set(out.variants.map(v => `${v.parent.rowId}/${v.parent.checkpoint}`)).size, 151);
  assert.equal(out.sourceHashes['manifest.json'].length, 64);
});
