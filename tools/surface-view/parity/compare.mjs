/**
 * Dependency-free, renderer-neutral RGBA comparison.
 *
 * `expectedElements` are frozen, independently supplied masks in reference
 * pixel coordinates. They are never inferred from the candidate. A formal
 * pass requires a calibrated colour policy; callers may still retain the
 * metrics and failures for an observed-default baseline.
 */

function failure(code, detail = {}) {
  return { code, ...detail };
}

function asBytes(value) {
  return value instanceof Uint8Array || Buffer.isBuffer(value) ? value : null;
}

function pixelIsForeground(bytes, offset, background, backgroundTolerance) {
  if (bytes[offset + 3] === 0) return false;
  return Math.abs(bytes[offset] - background[0]) > backgroundTolerance ||
    Math.abs(bytes[offset + 1] - background[1]) > backgroundTolerance ||
    Math.abs(bytes[offset + 2] - background[2]) > backgroundTolerance ||
    Math.abs(bytes[offset + 3] - background[3]) > backgroundTolerance;
}

function channelDifference(left, right, offset) {
  return Math.max(
    Math.abs(left[offset] - right[offset]),
    Math.abs(left[offset + 1] - right[offset + 1]),
    Math.abs(left[offset + 2] - right[offset + 2]),
    Math.abs(left[offset + 3] - right[offset + 3])
  );
}

function normalizeMask(mask, pixels) {
  if (!mask || typeof mask.length !== 'number' || mask.length !== pixels) return null;
  return mask;
}

/**
 * @param {object} input
 * @param {Uint8Array} input.reference - RGBA reference pixels.
 * @param {Uint8Array} input.candidate - RGBA candidate pixels.
 * @param {number} input.width
 * @param {number} input.height
 * @param {number} [input.candidateWidth=input.width]
 * @param {number} [input.candidateHeight=input.height]
 * @param {Array<{id: string, mask: Uint8Array|boolean[], minCoverage?: number, expectedColor?: number[], maxColorError?: number}>} [input.expectedElements]
 * @param {{minimumForegroundIoU?: number, maximumChannelError?: number, background?: number[], backgroundTolerance?: number, colorPolicy?: {calibrated?: boolean}}} [input.tolerances]
 */
export function compareRgba(input) {
  const reference = asBytes(input?.reference);
  const candidate = asBytes(input?.candidate);
  const width = input?.width;
  const height = input?.height;
  const candidateWidth = input?.candidateWidth ?? width;
  const candidateHeight = input?.candidateHeight ?? height;
  const tolerances = input?.tolerances ?? {};
  const failures = [];
  if (!Number.isInteger(width) || !Number.isInteger(height) || width <= 0 || height <= 0) {
    return { pass: false, failures: [failure('invalid-reference-dimensions')], metrics: null, elements: [] };
  }
  if (candidateWidth !== width || candidateHeight !== height) {
    return { pass: false, failures: [failure('dimension-mismatch', { reference: [width, height], candidate: [candidateWidth, candidateHeight] })], metrics: null, elements: [] };
  }
  const pixels = width * height;
  if (!reference || reference.length !== pixels * 4 || !candidate || candidate.length !== pixels * 4) {
    return { pass: false, failures: [failure('invalid-rgba-length', { expected: pixels * 4, reference: reference?.length, candidate: candidate?.length })], metrics: null, elements: [] };
  }
  const background = tolerances.background ?? [255, 255, 255, 255];
  if (!Array.isArray(background) || background.length !== 4) {
    return { pass: false, failures: [failure('invalid-background')], metrics: null, elements: [] };
  }
  const backgroundTolerance = tolerances.backgroundTolerance ?? 0;
  const minimumForegroundIoU = tolerances.minimumForegroundIoU ?? 0.995;
  const maximumChannelError = tolerances.maximumChannelError ?? 2;
  let referenceForeground = 0;
  let candidateForeground = 0;
  let intersection = 0;
  let maximumObservedError = 0;
  let totalMaximumError = 0;
  for (let pixel = 0; pixel < pixels; pixel += 1) {
    const offset = pixel * 4;
    const refForeground = pixelIsForeground(reference, offset, background, backgroundTolerance);
    const candidateIsForeground = pixelIsForeground(candidate, offset, background, backgroundTolerance);
    if (refForeground) referenceForeground += 1;
    if (candidateIsForeground) candidateForeground += 1;
    if (refForeground && candidateIsForeground) intersection += 1;
    const difference = channelDifference(reference, candidate, offset);
    maximumObservedError = Math.max(maximumObservedError, difference);
    totalMaximumError += difference;
  }
  const union = referenceForeground + candidateForeground - intersection;
  const foregroundIoU = union === 0 ? 0 : intersection / union;
  const metrics = {
    width,
    height,
    referenceForeground,
    candidateForeground,
    foregroundIntersection: intersection,
    foregroundIoU,
    maximumChannelError: maximumObservedError,
    meanMaximumChannelError: totalMaximumError / pixels
  };
  if (referenceForeground === 0) failures.push(failure('reference-foreground-empty'));
  if (candidateForeground === 0) failures.push(failure('candidate-foreground-empty'));
  if (foregroundIoU < minimumForegroundIoU) failures.push(failure('foreground-iou-below-minimum', { actual: foregroundIoU, minimum: minimumForegroundIoU }));
  if (maximumObservedError > maximumChannelError) failures.push(failure('global-channel-error-exceeded', { actual: maximumObservedError, maximum: maximumChannelError }));
  if (tolerances.colorPolicy?.calibrated !== true) failures.push(failure('uncalibrated-color-policy'));

  const elements = [];
  for (const element of input.expectedElements ?? []) {
    const mask = normalizeMask(element.mask, pixels);
    if (!mask) {
      failures.push(failure('invalid-element-mask', { id: element.id }));
      continue;
    }
    let expectedPixels = 0;
    let referencePresent = 0;
    let candidatePresent = 0;
    let elementMaximumError = 0;
    let elementTotalError = 0;
    const colorLimit = element.maxColorError ?? maximumChannelError;
    for (let pixel = 0; pixel < pixels; pixel += 1) {
      if (!mask[pixel]) continue;
      expectedPixels += 1;
      const offset = pixel * 4;
      const expected = element.expectedColor;
      const difference = expected
        ? Math.max(Math.abs(candidate[offset] - expected[0]), Math.abs(candidate[offset + 1] - expected[1]), Math.abs(candidate[offset + 2] - expected[2]), Math.abs(candidate[offset + 3] - expected[3]))
        : channelDifference(reference, candidate, offset);
      const referenceDifference = expected
        ? Math.max(Math.abs(reference[offset] - expected[0]), Math.abs(reference[offset + 1] - expected[1]), Math.abs(reference[offset + 2] - expected[2]), Math.abs(reference[offset + 3] - expected[3]))
        : 0;
      // For a declared colour, coverage means the required element itself is
      // present, rather than merely that some unrelated foreground survived in
      // its mask (for example a blue underlay where a red marker was removed).
      if (pixelIsForeground(reference, offset, background, backgroundTolerance) && referenceDifference <= colorLimit) referencePresent += 1;
      if (pixelIsForeground(candidate, offset, background, backgroundTolerance) && difference <= colorLimit) candidatePresent += 1;
      elementMaximumError = Math.max(elementMaximumError, difference);
      elementTotalError += difference;
    }
    if (expectedPixels === 0) {
      failures.push(failure('element-mask-empty', { id: element.id }));
      continue;
    }
    const coverage = candidatePresent / expectedPixels;
    const minimumCoverage = element.minCoverage ?? 1;
    const summary = { id: element.id, expectedPixels, referencePresent, candidatePresent, coverage, maximumChannelError: elementMaximumError, meanMaximumChannelError: elementTotalError / expectedPixels };
    elements.push(summary);
    if (referencePresent === 0) failures.push(failure('required-reference-element-missing', { id: element.id }));
    if (coverage < minimumCoverage) failures.push(failure('element-coverage-below-minimum', { id: element.id, actual: coverage, minimum: minimumCoverage }));
    if (elementMaximumError > colorLimit) failures.push(failure('element-color-error-exceeded', { id: element.id, actual: elementMaximumError, maximum: colorLimit }));
  }
  return { pass: failures.length === 0, failures, metrics, elements };
}
