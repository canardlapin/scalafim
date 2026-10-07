# Gale paired residual comparison candidate

Candidate `a57146ff55404039c3261a9af4a82ed307f6e625` extends the
published Gale `54e73f8e` revision. Its documentation correction is preserved
in commit `b56a9dd0b8ad479621a3594880f90c9add8c2824` and the corrected bundle;
the numeric source patch is unchanged.

The corrected candidate was published as [Gale PR 16](https://github.com/canardlapin/gale/pull/16),
then externally merged at d03eb99b. This task observed that merge and adopted the
tested b56a9dd0 revision. `merge-observation.json` records the observation;
`publication.json` and `candidate.json` retain the earlier draft/preparation states.

Full core gates passed 786 JVM and 774 Scala.js tests, including 23 new tests
on each platform. Formatting gates passed and compiler warnings were zero.
The independent exact-dyadic oracle subtracts two complete residual sums;
captured high-precision witnesses and negative controls are retained.

The original full-core records did not capture the Node version; their evidence
README's Node 24.1 claim was stale and is explicitly corrected. A separate
measured Node 24.21.0 run passed all 23 comparison tests. Its raw success log and
binary hash are retained. The supplemental recorder failed after the tests;
repaired metadata reports that failure and does not invent a recorded process
exit code or whole-process duration. The original archive remains unchanged.

The generic API outward encloses the difference between actual stored models.
For profiled minima it also bounds the previous coefficients' suboptimality.
Only a strictly negative final upper bound certifies decrease. Genuine uphill
witnesses, poor previous coefficients, subnormals, unavailable Gram bounds,
nonfinite inputs and overflow cannot produce a false certificate.

`paired-residual-source-only.patch` changes only the new numeric API, its two
test sources and README. Its SHA256 is
`a18e2746e43e0414436d69c23534df74c3220c42d8ab5dc350fd032adc49736c`.
It applies to clean published Gale 54e73f8e; native Linux rehearsal can use it
through an explicit local build override. The full patch and Git bundle preserve
the upstream candidate and its evidence.

The candidate does not decide optimizer policy, callback identity, priors or
evaluation quotas. Those require a separate compact-condition adapter and
JVM, Scala.js and native Linux qualification before production adoption.

`candidate.json` records the source patch and commit identities.
`upstream-evidence/receipt.json` seals the raw source, fixture and gate archive.
