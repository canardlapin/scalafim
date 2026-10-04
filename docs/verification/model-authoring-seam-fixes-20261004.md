# Model-authoring seam fixes, 2026-10-04

This receipt records the fixes for the three boundary findings in the
independent model-authoring seam review of 2026-10-04 (verdict CHANGES-REQUIRED,
mote `bd-01M43BJCDW0VMGMEMNCSEG1ZT5`). The review documents that motivated the
change are copied from `97445bde` into
[`model-authoring-seam-review-20261004/`](model-authoring-seam-review-20261004/):
`review.md`, `review.json`, and `boundary_analysis.py`. The source snapshot,
logs, and review probe suites stay on branch
`wip/model-authoring-seam-review-20261004`. The probe suites asserted the
defects, so they were rewritten as regression tests that assert the corrected
behaviour. Those tests are listed below.

Base: `main` at `780661c5`. The two affected production files on that base are
byte-identical to the files the review hashed in `review.json`, so the finding
locations did not move.

## Findings

### R1: trapezoid interval count saturated (confirmed, fixed)

`ResponseBasis.windowWeights` in `modules/hrf/.../Basis.scala` converted
`ceil(width / maxStep)` to `Int` without a bound.

- **Reproduced.** In `BasisGeometrySuite`, the test "an unrepresentable
  trapezoid grid is refused before any kernel evaluation" uses a 1e-20 s step
  over a 1 s window. On the unfixed code the kernel was evaluated: the test
  kernel throws on its first call, which stops the saturated
  `Int.MaxValue`-interval loop.
- **Fix.** The interval count is decided in `Double` before any conversion. If
  it is non-finite or exceeds the new documented work limit
  `FunctionalDiscretization.MaxTrapezoidIntervals = 1_000_000`, the request
  returns `BasisError.InvalidDiscretization`, and no kernel evaluation takes
  place. Nothing is clamped.
- **Accepted grids.** These now satisfy `effectiveStep <= maxStep` exactly. The
  fix adds one interval when `width / ceil(width / maxStep)` rounds above
  `maxStep` in the last ulp. One example is `maxStep = 0.09999999999999999`
  over `[0, 1)`: ceil gives 10 intervals, and `1 / 10` rounds to `0.1`. The
  sample count `intervals + 1` is at most `1_000_001`.
- **Refactor.** The quadrature loop moved into a private `trapezoidWeights`
  helper unchanged.

### R2: norm underflow/overflow in basis orthogonalization (confirmed, fixed)

`ConvolvedBasisOrthogonalization` formed column and residual norms from
unscaled sums of squares.

- **Reproduced.** On the unfixed code, collinear columns of amplitude 1e-200
  were accepted as an exact-zero group with rank 0 and the identity transform.
  Non-collinear 1e-200 columns also returned rank 0 instead of 2.
- **Fix: norms.** Column and residual norms use Gale's public scaled Euclidean
  norm (`DVec.norm2`, LAPACK-style `dnrm2`). Exact zero is decided from the
  entries, never from a norm.
- **Fix: vanishing test.** The test is the ratio `residual / source <= 1e-10`,
  so it cannot underflow.
- **Zero groups.** A genuinely zero group keeps rank 0 and the identity
  transform. This matches the documented policy for cells whose events fall
  outside the scan.
- **Tiny and huge columns.** Tiny or huge collinear groups are refused with the
  existing typed "vanishes … collinear" error. Tiny and huge non-collinear
  groups (1e-200, 1e-160, 1e160, 1e200) orthogonalize at full rank.

### R3: the reference SVD cutoff discarded an accepted tiny reference (confirmed at runtime against Gale, fixed)

This is the review's construction: x1 = 1e-18 e1, x2 = e2, x3 = e1 + e2 + e3,
built through public `Hrf.multi` and impulse convolution, then run through the
pinned Gale (`da38f8c4`) SVD.

- **Confirmed on the JVM.** The unfixed code returned rank 3 with a normalized
  cosine of `0.7071067811865475` between columns 1 and 3. That is exactly the
  value the review's NumPy toy predicted.
- **Fix.** Each serial projection now solves against unit-normalized reference
  columns with the existing Gale SVD, then transports the coefficients back by
  dividing by the reference norms. The rank cutoff therefore acts on the
  references' directions, not their amplitudes.
- **Rank-loss refusal.** If an accepted reference is still truncated, the group
  is refused with a typed `InvalidSchema` error ("lose rank"). The fix adds no
  private linear algebra; Gale needed no new capability.

## Stored SPM fixtures

`uv run --with numpy==2.4.3 --with scipy==1.17.1 python
tools/fixtures/generate_spm_informed_basis.py` regenerated both fixture files
byte-identically: `git status` showed no change. The fixes do not touch the SPM
kernel or `TemporalDerivativeConvention`. All SPM fixture suites pass on JVM and
JS within the gates below. No stored number changed.

## Mutation checks (JVM)

Each fix was reverted in place, and the targeted suite was run against the
mutant. The fixed files were then restored and verified by SHA-256:

- `Basis.scala`: `91c8e5beeddf04fccb89be0024604618b3c4a6d4cfb583d13272648fd650e731`
- `ConvolvedBasisOrthogonalization.scala`: `aa8e777fc2d89aa43e0fc9bf8ebd1242b7edc4d6d4321115e5c24826fb1af332`

| Mutant | Change | Result |
| --- | --- | --- |
| M1a | interval-limit refusal disabled (`if false`) | killed: "unrepresentable trapezoid grid…" fails (kernel evaluated) |
| M1b | last-ulp step guard removed | killed: "accepted trapezoid grids…" fails (effective step 0.1 > 0.09999999999999999) |
| M2 | column and residual norms back to unscaled sums of squares | killed: "tiny nonzero collinear…" and "huge collinear…" fail (accepted) |
| M3 | reference normalization removed (scales = 1) | killed: "a tiny earlier reference…" fails, and the new rank-loss guard reports "lose rank (1 of 2 retained)" |

Two parts of the fixes have no killing test:

- **Entry-based zero test.** It is equivalent to `nrm2 == 0` once norms are
  scaled, so no mutant can separate the two. It is kept for clarity.
- **Rank-loss refusal.** With normalized references it has no known trigger.
  M3 exercises it only with normalization removed.

## Gates

Each batch ran through `tools/build/sbt-warm`, and only with at least 30% free
memory.

| Gate | Result |
| --- | --- |
| `hrfJVM/test` | 277 passed |
| `designJVM/test` | 445 passed |
| `modelJVM/test` | 53 passed |
| `fitJVM/test` | 562 passed |
| `hrfJS/test` | 277 passed |
| `designJS/test` | 444 passed |
| `modelJS/test` | 53 passed |
| `fitJS/test` | 509 passed |
| `scalafimCompileAll` (`-release:17`) | exit 0, 666 s; no compiler warnings (only sbt GC-pressure notices) |
| `examplesCompile` | exit 0, no warnings |

The JVM and JS counts differ for design and fit because of existing JVM-only
suites. All new tests are in the shared test trees and run on both platforms.

## Tests added

- `modules/hrf/shared/src/test/scala/scalafim/fmri/hrf/BasisGeometrySuite.scala`:
  - "an unrepresentable trapezoid grid is refused before any kernel evaluation"
  - "accepted trapezoid grids respect the declared maximum step and a representable sample count"
- `modules/design/shared/src/test/scala/scalafim/fmri/design/ConvolvedBasisOrthogonalizationSuite.scala`:
  - "tiny nonzero collinear columns are refused rather than recorded as an exact-zero group"
  - "tiny and huge non-collinear columns orthogonalize at full rank"
  - "huge collinear columns are refused rather than accepted through an overflowed norm"
  - "a genuinely zero basis group keeps rank 0 and the identity transform"
  - "a tiny earlier reference is not truncated away: the result is orthogonal at full rank"

## Owner decisions

- **Work limit.** `MaxTrapezoidIntervals = 1_000_000` is a policy constant: two
  kernel evaluations per interval. It allows a 1 ms step over a 1000 s window.
  If larger grids are needed, the owner should raise it or make it a parameter.
- **Review status.** This receipt does not re-qualify the seam. A fresh
  independent review of these fixes is still needed before the review's
  CHANGES-REQUIRED verdict can be lifted.
