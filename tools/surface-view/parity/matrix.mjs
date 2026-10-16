/**
 * Validate the prospective surface-view admission matrix.  This is deliberately
 * a pure receipt checker: callers freeze source, fixture, artifact and
 * calibration identities independently, hash artifacts after capture and before
 * validation, then supply the allowed concrete renderer for each product route.
 *
 * @typedef {{code: string, rowId?: string, checkpoint?: string, detail?: object}} MatrixError
 * @typedef {{ok: boolean, errors: MatrixError[], expected: number, received: number}} MatrixValidation
 * @typedef {{sourceCommit: string, fixtureHashes: Record<string, string>, artifactHashes: Record<string, Record<string, Record<string, string>>>, calibrations: Record<string, Record<string, string>>, requiredElements: Record<string, Record<string, string[]>>, approvedActualRenderers: Record<string, string[]>}} FrozenMatrixIdentity
 */

function error(code, detail = {}) {
  return { code, ...detail };
}

function object(value) {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function nonBlank(value) {
  return typeof value === 'string' && value.trim().length > 0;
}

function hash(value, length) {
  return typeof value === 'string' && new RegExp(`^[a-f0-9]{${length}}$`, 'i').test(value);
}

function validArtifacts(value) {
  return object(value) && Object.keys(value).length > 0 &&
    Object.entries(value).every(([key, entry]) => nonBlank(key) && hash(entry, 64));
}

function sameHashes(actual, expected) {
  if (!validArtifacts(actual) || !validArtifacts(expected)) return false;
  const actualKeys = Object.keys(actual).sort();
  const expectedKeys = Object.keys(expected).sort();
  return actualKeys.length === expectedKeys.length && actualKeys.every((key, index) =>
    key === expectedKeys[index] && actual[key] === expected[key]);
}

function receiptKey(rowId, checkpoint) {
  return `${rowId}\u0000${checkpoint}`;
}

function expectedRows(manifest, errors) {
  if (!object(manifest) || !Array.isArray(manifest.requiredMatrix)) {
    errors.push(error('invalid-required-matrix'));
    return new Map();
  }
  if (manifest.requiredMatrix.length === 0) {
    errors.push(error('empty-required-matrix'));
    return new Map();
  }
  const expected = new Map();
  for (const row of manifest.requiredMatrix) {
    if (!object(row) || !nonBlank(row.id) || !nonBlank(row.fixture) || !nonBlank(row.route) || !Array.isArray(row.checkpoints)) {
      errors.push(error('invalid-required-row'));
      continue;
    }
    if (row.checkpoints.length === 0) {
      errors.push(error('empty-required-checkpoints', { rowId: row.id }));
      continue;
    }
    for (const checkpoint of row.checkpoints) {
      if (!nonBlank(checkpoint)) {
        errors.push(error('invalid-required-checkpoint', { rowId: row.id }));
        continue;
      }
      const key = receiptKey(row.id, checkpoint);
      if (expected.has(key)) errors.push(error('duplicate-required-checkpoint', { rowId: row.id, checkpoint }));
      else expected.set(key, row);
    }
  }
  return expected;
}

function validateFrozen(row, checkpoint, frozen, errors) {
  if (!hash(frozen?.sourceCommit, 40)) errors.push(error('invalid-frozen-source-commit'));
  if (!hash(frozen?.fixtureHashes?.[row.fixture], 64)) {
    errors.push(error('invalid-frozen-fixture-hash', { rowId: row.id, checkpoint }));
  }
  const artifacts = frozen?.artifactHashes?.[row.id]?.[checkpoint];
  if (!validArtifacts(artifacts)) errors.push(error('invalid-frozen-artifact-hashes', { rowId: row.id, checkpoint }));
  if (!hash(frozen?.calibrations?.[row.id]?.[checkpoint], 64)) {
    errors.push(error('invalid-frozen-calibration', { rowId: row.id, checkpoint }));
  }
  const elements = frozen?.requiredElements?.[row.id]?.[checkpoint];
  if (!Array.isArray(elements) || elements.length === 0 || elements.some(value => !nonBlank(value)) || new Set(elements).size !== elements.length) {
    errors.push(error('invalid-frozen-required-elements', { rowId: row.id, checkpoint }));
  }
  const approved = frozen?.approvedActualRenderers?.[row.route];
  if (!Array.isArray(approved) || approved.length === 0 || approved.some(value => !nonBlank(value))) {
    errors.push(error('invalid-frozen-renderers', { rowId: row.id, checkpoint }));
  }
}

/**
 * @param {{manifest: {requiredMatrix: object[]}, receipts: object[], frozen: FrozenMatrixIdentity}} input
 * @returns {MatrixValidation}
 */
export function validateRequiredMatrix(input) {
  const errors = [];
  const expected = expectedRows(input?.manifest, errors);
  const receipts = Array.isArray(input?.receipts) ? input.receipts : null;
  const frozen = input?.frozen;
  if (!receipts) errors.push(error('invalid-receipts'));
  if (!object(frozen)) errors.push(error('invalid-frozen-identities'));
  for (const [key, row] of expected) {
    const [, checkpoint] = key.split('\u0000');
    validateFrozen(row, checkpoint, frozen, errors);
  }
  const seen = new Set();

  for (const receipt of receipts ?? []) {
    if (!object(receipt) || typeof receipt.rowId !== 'string' || typeof receipt.checkpoint !== 'string') {
      errors.push(error('invalid-receipt'));
      continue;
    }
    const key = receiptKey(receipt.rowId, receipt.checkpoint);
    const row = expected.get(key);
    if (!row) {
      errors.push(error('unexpected-receipt', { rowId: receipt.rowId, checkpoint: receipt.checkpoint }));
      continue;
    }
    if (seen.has(key)) {
      errors.push(error('duplicate-receipt', { rowId: receipt.rowId, checkpoint: receipt.checkpoint }));
      continue;
    }
    seen.add(key);
    if (receipt.productRoute !== row.route) errors.push(error('product-route-mismatch', { rowId: row.id, checkpoint: receipt.checkpoint }));

    const approved = frozen?.approvedActualRenderers?.[row.route];
    if (!Array.isArray(approved) || !approved.includes(receipt.actualRenderer)) {
      errors.push(error('renderer-substituted', { rowId: row.id, checkpoint: receipt.checkpoint }));
    }
    if (!hash(receipt.fixtureHash, 64) || receipt.fixtureHash !== frozen?.fixtureHashes?.[row.fixture]) {
      errors.push(error('fixture-hash-mismatch', { rowId: row.id, checkpoint: receipt.checkpoint }));
    }
    if (!hash(receipt.sourceCommit, 40) || receipt.sourceCommit !== frozen?.sourceCommit) {
      errors.push(error('stale-source-commit', { rowId: row.id, checkpoint: receipt.checkpoint }));
    }
    if (!sameHashes(receipt.artifactHashes, frozen?.artifactHashes?.[row.id]?.[receipt.checkpoint])) {
      errors.push(error('artifact-hashes-mismatch', { rowId: row.id, checkpoint: receipt.checkpoint }));
    }
    if (!hash(receipt.calibration?.id, 64) || receipt.calibration?.id !== frozen?.calibrations?.[row.id]?.[receipt.checkpoint] || receipt.calibration?.calibrated !== true) {
      errors.push(error('uncalibrated-receipt', { rowId: row.id, checkpoint: receipt.checkpoint }));
    }
    for (const check of ['semantics', 'visual', 'interaction', 'resource']) {
      if (receipt[check] !== true) {
        errors.push(error('required-check-not-clean', { rowId: row.id, checkpoint: receipt.checkpoint, detail: { check } }));
      }
    }
    const requiredElements = frozen?.requiredElements?.[row.id]?.[receipt.checkpoint];
    const evidenceIds = object(receipt.elementEvidence) ? Object.keys(receipt.elementEvidence).sort() : [];
    const expectedElements = Array.isArray(requiredElements) ? [...requiredElements].sort() : [];
    if (evidenceIds.length !== expectedElements.length || evidenceIds.some((id, index) => id !== expectedElements[index]) ||
        evidenceIds.some(id => receipt.elementEvidence[id] !== true)) {
      errors.push(error('missing-element-evidence', { rowId: row.id, checkpoint: receipt.checkpoint }));
    }
    if (receipt.verdict !== 'pass') {
      errors.push(error('non-clean-verdict', { rowId: row.id, checkpoint: receipt.checkpoint, detail: { verdict: receipt.verdict } }));
    }
  }
  for (const [key, row] of expected) {
    if (!seen.has(key)) {
      const [rowId, checkpoint] = key.split('\u0000');
      errors.push(error('missing-receipt', { rowId, checkpoint, detail: { productRoute: row.route } }));
    }
  }
  return { ok: errors.length === 0, errors, expected: expected.size, received: receipts?.length ?? 0 };
}
