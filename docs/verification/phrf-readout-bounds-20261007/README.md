# PHRF residual admission, finite observation bounds and envelope gradient

This continues local `e1d67dfb` on `work/phrf-checkpoint-20261007`, with Gale
still pinned to `0a981febc163e539b019aa9379935e21d8987a20`. The changes advance
PHRF-11/14/15/33. **Original-equation, scientific and complete-workload
qualification remain open.** No hosted CI, merge or publication is claimed.

## Public residual admission

The existing empirical readout now has an explicit, opt-in residual ceiling:

```scala
val evidence = ProfileTrialEvidenceRequest.PreparedBasisResidualAtMost(
  ProfileTrialResidualLimit(1e-8))
outputs.run(reader, request, ProfileTrialReadoutMode.CorrectedReference,
  sink, evidence = evidence)
```

The threshold applies to the Euclidean normal residual of the prepared
equations in their native coordinates, before output normalization. It is
neither an amplitude-error tolerance nor a bound for the original HRF. The
value above is a diagnostic choice, not a new scientific acceptance threshold.
Existing ungated callers retain their historical semantics. Original-equation
certification continues to refuse.

A selected voxel exceeding the ceiling returns `ReadoutRefused` in its block.
Other voxels can still be delivered. Its decoded shape/status, numerical work,
measured residual and requested ceiling remain inspectable; no amplitudes or
Float32 output are emitted, and there is no exact fallback. A direct frozen
readout returns the typed error with its work receipt. Public progress counts
refused normal actions and response encoding separately from emitted results.
The gated response map refuses an unconditional adjoint; the existing ungated,
fixed-shape forward/transpose contract remains available. Native ML continues
to admit only its existing exact empirical request.

## Search implementation

For a fixed penalty and a conditional optimum `(a, gamma)`, stationarity removes
coefficient derivatives from the profile gradient:

```
dE = a' d(X'X) a + 2 a' d(X'F) gamma - 2 a' d(X'y).
```

The search oracle builds one value reference, solves the response once,
recovers coefficients from the already solved release, and contracts each
packed basis-pair quadratic once for all shape coordinates. Existing worker
scratch holds the temporary trial coefficients. There are no derivative
reference solves, new provider kernels or retained voxel coefficient fields.
Full jets still determine terminal curvature. Nine starts, evaluation budgets,
priors, criterion and terminal admission tolerances are unchanged. The optional
initialization provenance is now `bounded-multistart/v3` because floating-point
evaluation order changes. Numerical equality is tested with explicit roundoff
tolerances; bitwise-identical search trajectories are not claimed.

## Finite observation audit

The new fixed-shape audit uses T=600, N=30, four interior Cascade34 shapes,
signed heterogeneous trial amplitudes, six nuisance columns and AR(1)=0.3.
It repeats the same physical event schedule at horizons 48, 96 and 192 seconds.
Changing the horizon does not move the events. Exact and corrected readouts
both use the eight-corner bank. Query-only execution is exercised directly.

At every evaluated shape the oracle materialises the full-window, whitened,
augmented observation array `A` and response `y`, treating their rounded entries
as exact binary numbers. Gale validates an SVD lower bound `s > 0`; outward
interval operations enclose `r = A'(y-A a)`. This gives the finite-model bound

```
||a - a_finite||_2 <= ||r||_2 / s^2.
```

The signed-query bound additionally multiplies by the query norm and includes
the query contraction's arithmetic enclosure. An independent augmented QR
solution checks the observed coefficient/query errors against these bounds.
This is useful evidence about the declared finite array, not a certificate for
the arithmetic that constructed it. Family evaluation, convolution, whitening,
continuous-time integration, normalization and uniform reference neighborhoods
still require their own enclosures for the original-equation contract.

Dense per-shape SVD validation is charged diagnostic work outside the corrected
readout budget. It is not inserted into the production fast path. Every
scientific scenario carries blocking scope/certificate caveats and remains
`Fail`. These exploratory shapes and seeds cannot serve as a new confirmation
cohort or establish adaptive coverage, other families or search optimality.

## Measured results

The finite-array bounds enclose all observed QR discrepancies but are far too
loose for tight amplitude admission. Maxima below are across the four fixed
shapes. Coefficient discrepancy is the maximum absolute entry of the joint
trial/nuisance solution; the coefficient bound is for its Euclidean norm.

| Support | Readout | Original QR discrepancy | Signed-query error | Finite coefficient bound | Signed-query bound | Residual gate passes |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| 48 s | ExactShape | 0.009963 | 0.001213 | 16.741 | 91.693 | 4/4 |
| 48 s | CorrectedReference | 0.442923 | 0.007017 | 17.606 | 96.431 | 0/4 |
| 96 s | ExactShape | 0.001043 | 0.000200 | 2.288 | 12.534 | 4/4 |
| 96 s | CorrectedReference | 0.477734 | 0.006645 | 19.865 | 108.802 | 0/4 |

Exact prepared-equation residuals are below 1.50e-14. The 96-second support
reduces the largest exact/full-window discrepancy about 9.56-fold at these
points, with the same 1e-3 basis tolerance. It does not fix distant-reference
correction. Selected rank/bandwidth rises from 10/6 to 12/10.

The compiler admits 96 seconds when its maximum rank is 24 instead of 32;
maximum rank sizes compiler workspaces, independently of the selected rank.
All requested variants and refusals remain in the artifact:

| Support | Maximum rank | Subspace | Outcome |
| --- | ---: | ---: | --- |
| 48 s | 32 | 96 | Admitted; selected rank 10 |
| 96 s | 32 | 96 | Certification work 102,729,900 exceeds 100,000,000 |
| 192 s | 32 | 96 | Storage 20,510,961 exceeds 16,000,000 cells |
| 96 s | 24 | 96 | Admitted; selected rank 12 |
| 192 s | 15 | 96 | Storage 20,314,662 exceeds 16,000,000 cells |
| 192 s | 15 | 48 | Training work 32,679,640 exceeds 20,000,000 |

No compiler work/storage limit was raised. There are 16 numerical records and
four preparation refusals; all 20 scientific scenarios remain `Fail`. The
separate N=1200, 192-second diagnostic also stops at the kernel compiler's
training-work cap, before trial preparation or voxel execution.

### Fixed-call gradient probe

Each platform uses the same T=600, N=300 fixture, four fixed shapes, a warmup
batch per mode and five alternating paired batches of 64 calls. Checksums,
energies and gradients agree within the declared roundoff tolerances.

| Platform | Median full-jet batch | Median envelope batch | Ratio of medians |
| --- | ---: | ---: | ---: |
| JVM | 0.803429 s | 0.115298 s | 6.97x |
| Scala.js | 3.396816 s | 0.624504 s | 5.44x |

The new gradient needs three banded solves and 13 right-hand sides per call;
full jets need 21/103. The previous first-order implementation needed 9/43,
but it is not the timed comparator above. These ratios describe only the
fixed-call probe, not an overall decoder speedup. The B0 reference arrays shrink
from 766,760 bytes for the previous first-order reference to 339,776 bytes for
the new value reference; a terminal full reference remains 1,620,728 bytes.

### Bounded resource attempts

Both requests use the repaired 901-request budget, eight workers, 256-voxel
blocks, blocked preparation, noise ratio 2, corrected readout and the explicit
1e-8 empirical residual gate. Cancellation is requested at 120 seconds and
checked between voxels; workers are drained before recording the result.

| Request | Preparation | Execution | Attempted voxels | Decoder outcomes | Delivered / emitted |
| --- | ---: | ---: | ---: | --- | ---: |
| B0: N=300, V=100,000 | 1.549 s | 126.125 s | 384 | 52 Accepted, 266 Boundary, 66 CurvatureNotPositive | 0 / 0 |
| Stress: N=1200, V=10,000 | 21.938 s | 136.570 s | 18 | 18 Boundary | 0 / 0 |

All 52 B0 decoder acceptances fail the readout residual gate. Their 156 inverse
applications and 31,200 response rows are charged despite discarded partial
blocks. B0 and stress use 126,863 and 6,620 first-order requests respectively.
These attempts still miss the complete-workload requirement. Other JVM activity
was observed, and these are single runs in an uncontrolled sbt process; they do
not establish a speedup or regression against the earlier 404/32-voxel attempts.
No complete workload time is extrapolated.

Whole-process sampled heap maxima are 4,463,070,040 and 4,560,926,208 bytes.
They include sbt, compiler state, fixture, garbage and engine, and are not engine
live memory. The source-based N=1200 scenario estimates shared arrays plus eight
search references at 235,670,648 bytes (224.75 MiB), down from 256.85 MiB for the
prior first-order references. Eight full terminal references still bring that
scenario to 336,638,264 bytes (321.04 MiB). Worker/construction scratch, blocks,
metadata and object overhead are excluded. No live-memory admission follows.

## Validation and reproduction

Both platforms pass the affected implementation gates:

- Full fit suites: **706 JVM / 648 Scala.js** tests.
- All **80 recovery reference cases** remain within the existing scientific
  tolerances on both platforms (seven refinement tests, including native ML
  and additional-shape derivative checks).
- Targeted workflows: 18 Scala.js tests; JVM gates cover the same six suites
  across the initial 10-test batch, the seven refinement tests and the final
  two-test bound suite. Repeated bound tests are not counted as new coverage.
- Comparison runner: **466 JVM tests pass, 10 explicitly skipped**;
  **17 targeted Scala.js PhrfTrialSuite tests pass**. The ten JVM skips cover
  five Python interop checks, one opt-in rho-bias cohort, one live generator
  check and three external GLMsingle checks; they remain unverified here.

The final horizon diagnostic completes with 16 records and four declared
refusals. The verifier checks the finite-bound inequalities, query-only storage,
work counts, cancellation receipts, paired checksums and source/artifact hashes.

The compiler is warning-clean; sbt's existing multiple-main discovery warning
is retained separately. Passing MUnit regression tests means the implementation
preserves its tested contracts; it does not turn scientific `Fail` scenarios
into qualification passes. Development logs retain superseded compile errors
and refused exploratory variants, with final outcomes recorded in
`validation.json`.

Run bounded batches from the frozen source overlay, shutting down the warm
server between larger Scala.js phases:

```sh
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm fitJVM/test
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm fitJS/test
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm --shutdown
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/testOnly *TrialReadoutBoundAuditSuite *TrialStressIntegrationSuite *TrialQualificationAuditSuite *DecodedTrialCheckpointSuite *TrialEnvelopeGradientProbeSuite *TrialRefinementSuite'
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm phrfComparisonJVM/test
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm --shutdown
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJS/testOnly *TrialReadoutBoundAuditSuite *TrialStressIntegrationSuite *TrialQualificationAuditSuite *DecodedTrialCheckpointSuite *TrialEnvelopeGradientProbeSuite *TrialRefinementSuite'
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm --shutdown
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'phrfComparisonJS/testOnly *PhrfTrialSuite'
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm --shutdown
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialReadoutBoundAuditMain /private/tmp/phrf-readout-horizons.json'
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.DecodedTrialCheckpointMain /private/tmp/phrf-envelope-b0.json dense-blocked 100000 300 8 penalized corrected envelope-v3 2 repaired 32 2 120 1e-8'
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.DecodedTrialCheckpointMain /private/tmp/phrf-envelope-stress.json dense-blocked 10000 1200 8 penalized corrected envelope-v3 2 repaired 32 2 120 1e-8'
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.DecodedTrialCheckpointMain /private/tmp/phrf-long-horizon-stress.json dense-blocked 1 1200 1 penalized exact horizon192 2 repaired 32 0.1 1 1e-8 192 15 48'
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm --shutdown
python3 -S docs/verification/phrf-readout-bounds-20261007/verify.py
```

`manifest.json` binds the final source overlay, artifacts and raw compressed
logs. The B0/stress resource JSONs precede the final CLI extension for explicit
maximum-rank/subspace arguments and its maximum-rank metadata field; those
runs used the unchanged defaults 32/96. The final horizon audit uses the final
source. Earlier development logs are historical intermediate states; no claim
is made that every intermediate invocation used the final byte-identical
source. `gradient-probes.json` retains every paired measurement and checksum.

## Remaining gates

The residual ceiling makes refusal explicit but does not repair distant-reference
Taylor errors. Longer support can reduce sampled tail discrepancies while
increasing rank, bandwidth and memory. The original-equation contract needs
the complete observation enclosure and an admitted reference neighborhood,
with certification work and memory charged. Broader independent search,
Gaussian/LWU, weak/null/misspecified regimes and the required coverage cohorts
remain open. Complete certified B0/stress timing and measured engine live
memory follow those prerequisites. FiniteState remains deferred.
