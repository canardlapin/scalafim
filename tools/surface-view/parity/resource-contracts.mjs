/** Fixed budgets are independent of either renderer's measured result. */
export function calibrateRetainedHeap(cycles) {
  const values = cycles.slice(3).map(x => x.retainedHeapBytes);
  const referenceGrowthLimitBytes = 2 * 1024 * 1024;
  const maximumAllowanceBytes = 8 * 1024 * 1024;
  const finite = values.length === 5 && values.every(x => Number.isFinite(x) && x >= 0);
  const span = finite ? Math.max(...values) - Math.min(...values) : null;
  const growth = finite ? values.at(-1) - values[0] : null;
  return { valid: finite && span <= referenceGrowthLimitBytes && growth <= referenceGrowthLimitBytes,
    referenceGrowthLimitBytes, referenceRangeBytes: span, referenceGrowthBytes: growth,
    maximumRetainedGrowthBytes: maximumAllowanceBytes,
    policy: 'Reference last-five retained heap range must be at most 2 MiB; candidate ten-cycle median growth budget fixed at 8 MiB, never enlarged by observed reference drift.' };
}
export function validWarmSamples(values, predicate = () => true) {
  return Array.isArray(values) && values.length === 5 && values.every(x => Number.isFinite(x) && x >= 0 && predicate(x));
}
/** Attempt every owned cleanup and retain all failures, including an earlier failure. */
export async function cleanupOwned(actions) {
  const failures = [];
  for (const [name, action] of actions) {
    try { await action(); } catch (error) { failures.push({ name, error: String(error) }); }
  }
  return failures;
}
