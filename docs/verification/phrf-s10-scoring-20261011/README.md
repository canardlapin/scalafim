# PHRF S10 production scoring contribution rehearsal — 2026-10-11

This packet implements the bounded scoring-contribution slice of S10. Production
arms now seal deterministic, truth-free contributions; the owner can reconstruct
the corpus against separately held, bound generator truth. A synthetic 450-unit
retry/crash/resume rehearsal produces `OwnerVerdict.Valid` and the same corpus
digest on reconstruction. This is not a real pilot or full S10 launch qualification.

## Implementation and scoring policy

- `PilotContribution` uses canonical JSON with exact IEEE-754 bit strings and a
  versioned six-field envelope. Unit identity, input fingerprint, dimensions,
  finite values and canonical bytes are checked. A missing payload refuses
  assembly; a committed unavailable result retains every voxel as Failed.
- `PilotArmRunner` contributes condition, native trial, PHRF-ML and GLMsingle
  outcomes through the existing transactional commit path. Legacy `ScoreFeed`
  callbacks remain observational. Failed or interrupted attempts cannot become
  authoritative through those callbacks.
- `ScoringInputFingerprint` extends the common preparation identity with stimulus
  labels and the noise pool. Estimated GLMsingle results additionally bind the
  actual NPZ hash reported by the child to the held dataset hash.
- `PilotScoreTruth.fromBound` copies held truth. `PilotCorpusAssembler` requires
  all seven cells and every expected method/index exactly once. Condition truth
  is `cond_coef * truth_kernel` on the 321-point `[0,32]` grid; the noise variance
  is the generator-v1 value 1. Trial truth stays in input event order.
- Accepted Gaussian readouts retain the existing conditional shape SDs and exact
  signed response peaks. Relative peak error averages the three signed peak
  errors divided by each absolute true peak; true continuous peaks account for
  the generator's 0.01-second grid normalization. Tau error is fitted minus true
  tau. Hand-calculated tests distinguish signed peak errors and the two coverage
  targets.

Coverage has no default: callers must choose `GaussianCoverageTarget.Tau` or
`BothShapeCoordinates`. The choice is part of the mapping schema. The synthetic
rehearsal uses Tau; no actual study policy is selected here. Exact byte and
mapping rules are in [the sealed-format specification](../../plans/phrf-pilot-sealed-format.md).

## Validation

Commands run with the worktree-isolated `python3 tools/build/sbt-warm`:

| Gate | Result | Receipt |
| --- | --- | --- |
| Full `phrfComparisonJVM/test`, before the final input/NPZ binding additions | 533 passed, 10 skipped, 0 failed | [full-gates-1.log](full-gates-1.log) |
| Final JVM `testOnly *PilotContributionSuite *ScoringInputFingerprintSuite *PilotCorpusAssemblerSuite *PilotArmsSuite` | 30 passed, 0 failed | [final-binding-gates.log](final-binding-gates.log) |
| Final full `phrfComparisonJS/test` | 252 passed, 0 failed | [final-binding-gates.log](final-binding-gates.log) |

The final gates compile without source warnings. The ten JVM skips are the
existing heavy rho study, fresh Python generator, three real GLMsingle checks and
five Python/JVM sealing interoperability checks. They are not claimed as passed.

The new contracts cover canonical codec bytes (including a literal independent
trial golden), malformed/nonfinite/dimension failures, truth snapshot isolation,
fingerprint sensitivity without truth leakage, exact inventory, wrong unit/input/
truth/file identities, failure denominators and deterministic corpus ordering.
The sealed rehearsal covers a discarded first-attempt estimate, a marker-written
crash, resume, owner reconstruction and contribution tampering. A separate tiny
CPU-ceiling rehearsal prevents repeated work after exhaustion, resumes with a
synthetic output-bound authorization, and refuses to score its two datasets as a
pilot.

Earlier engineering receipts are retained: [engineering-1.log](engineering-1.log)
and [engineering-2.log](engineering-2.log) caught compiler warnings;
[engineering-3.log](engineering-3.log) caught two incorrect test expectations
(450 units in the valid verdict, and a minimum of two rehearsal datasets).
The first full-gate attempt passed JVM tests but its JS compile caught a test
accessing an ingest-private constructor from the wrong package. The test was
moved to the ingest package; the final gate above passes both platforms.

## Scope and remaining work

All new execution evidence here is synthetic. Ephemeral test keys, a fixed test
root, fake engines and a fake CPU clock exercise the contracts; they are not
owner custody material, a beacon selection or evidence about actual pilot costs.
No actual sealed pilot, confirmation stream or real GLMsingle study was opened.

The broader S10 runner mote remains open for its real-engine rehearsal, frozen
study policy/inventory, owner launch inputs and independent review. The bounded
contribution implementation is tracked separately as
`bd-01M4NV2GJQ218WS6SBXRMPCAH7`, under
`bd-01M3TNWW63XC71C3EF2PQME3P8`.
