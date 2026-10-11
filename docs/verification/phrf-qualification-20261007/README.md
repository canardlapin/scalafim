# PHRF integration and qualification attempt — 2026-10-07

The blocked 1,200-trial preparation and repaired decoder work together. This
attempt also finds two substantive numerical admission gaps: corrected readout
from the eight-corner bank can have order-one amplitude errors, and the
48-second Cascade34 horizon can omit scientifically material tails.
**The epic is not admitted.** PHRF-11/14/15/33 remain unresolved.

Two allocation fixes accompany the evidence. Production dataset acquisition
construction now materializes its active voxel indices once, removing a
quadratic reverse-map construction. The diagnostic reader similarly resolves
selected time and voxel vectors once per block. Neither change alters
scientific policies, solver budgets, normalization or numerical kernels.

## Integration

The JVM runner now exposes `repaired`, the same nine-start, 901-request
policy tested by `TrialRefinementSuite`. The policy has one shared definition
in the diagnostic fixture. Optional noise and execution-deadline arguments are
explicit and recorded; historical defaults stay unchanged.

At N=1,200, T=600, four voxels and one worker, the original noise ratio of 2
produces four `Boundary` refusals, without budget exits. A noise ratio of 0.1
produces four accepted public exact-shape outputs with maximum prepared-basis
normal residual `4.57e-12`. Execution takes about 71.93 and 116.16 seconds,
respectively, in the initial single diagnostic runs. These timings precede
the allocation fixes and are not controlled speedup evidence.

The portable stress integration scenario exercises the accepted lower-noise
case on JVM and Scala.js. Its claim is limited to prepared-basis integration;
it does not request or imply original-equation certification.

## Certification and scientific audit

See [the certificate composition and capability audit](certification-audit.md).
Pinned Gale already supplies validated spectral enclosures. The missing
production capability is a complete, geometry-bound HRF/observation error
envelope and conditioning bound within the declared readout work budget.
The public `CertifiedOriginalEquations` request remains a refusal.

The exploratory audit uses four interior Cascade34 shapes, 60 trials, 600
rows, two voxels per cell, signed amplitudes and six nuisance columns. Each
shape has a matched noiseless control, heterogeneous trial amplitudes with
original-family generation, and an original-family AR noise ratio of 0.5.
Both readout modes consume the same decoded shape. This is 24 distinct
data/voxel cases and 48 mode-specific records, including every refusal.

Direct-family designs are evaluated through the complete acquisition window
on the existing microtime grid. Comparing them with a direct 48-second design
separates tail error from basis error. The independent augmented QR solves
compare coefficients at the returned shape, without injecting truth into the
decoder. Gale validates positive singular-value lower bounds for the finite
original-observation augmented arrays; these do not certify the upstream
family, convolution or whitening arithmetic.

<!-- audit-results -->
| Readout | Attempted | Emitted | Max prepared QR coefficient error | Max original trial error | Max original nuisance error | Max signed-query error |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| ExactShape | 24 | 21 | 3.88391e-13 | 0.00888737 | 0.00963684 | 0.00113393 |
| CorrectedReference | 24 | 21 | 2.22325 | 2.22328 | 0.170238 | 0.0656858 |

Both modes have two boundary refusals and one curvature refusal. All eight
matched noiseless exact-readout controls emit, with maximum chart-scaled shape
error `3.31e-10`. The original-family generator exposes approximation and
shrinkage effects beyond these matched controls.

| Shape | Basis error within 48 seconds | Tail design error | Whitened design error | Validated finite-array sigma lower |
| --- | ---: | ---: | ---: | ---: |
| shape-0 | 5.58112e-06 | 0.0348476 | 0.0349657 | 0.108636 |
| shape-1 | 3.761e-05 | 0.000840818 | 0.000912316 | 0.124767 |
| shape-2 | 9.03726e-05 | 0.00824823 | 0.00839966 | 0.203718 |
| shape-3 | 0.000159822 | 1.12458e-09 | 0.000191209 | 0.199359 |
<!-- end-audit-results -->

Heterogeneous-amplitude truth errors include the effect of the fixed shrinkage
penalty; they are not all solver errors. Signed-query error here is a
post-solve alternating-sign query over the amplitudes, not a separate public
query-only execution. The eight matched noiseless controls test recovery of
four different shapes. The remaining cells do not have independently searched
global references and cannot establish search optimality.

Every scientific audit record returns a `ScenarioResult` with blocking caveats
for original-equation certification and incomplete scientific scope. All are
`Fail`, and no CI policy accepts them. MUnit checks that the numerical oracles
agree where they should and that the scientific failures remain visible.
Passing those tests is not passing PHRF-14. These are consumed exploratory
seeds, not a preregistered confirmation cohort. Gaussian/LWU, weak/null and
misspecified regimes, and >=500-replicate coverage courts remain outstanding.

## Resource attempts

The declared attempts request dense-overlap B0 at V=100,000, N=300 and stress
at V=10,000, N=1,200, eight workers and blocks of 256. They use blocked basis
compilation and trial lowering, the existing eight-node bank, corrected
readout, fixed noise ratio 2, and explicitly named decoder policies.
All output and refusal work is charged. Original-family certification is
unavailable, so even a completed timing would be a narrower diagnostic.

Each run requests cancellation after 120 execution seconds. Cancellation is
checked between voxels, and workers are drained before reporting. It is not a
hard preemption limit: an in-progress voxel can overshoot. Attempted counts and
statuses include work in a partially completed block, while the sink only
counts delivered blocks. Accepted results in a discarded partial block are
not reported as emitted output.

<!-- resource-results -->
| Final attempt | Requested voxels | Preparation seconds | Execution seconds | Attempted | Decoder accepted | Delivered voxels | Emitted outputs |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| B0 baseline | 100,000 | 1.745 | 120.067 | 56,992 | 0 | 55,296 | 0 |
| B0 repaired | 100,000 | 0.888 | 122.274 | 404 | 56 | 0 | 0 |
| N=1,200 repaired | 10,000 | 15.241 | 147.754 | 32 | 0 | 0 | 0 |

All three attempts end in cancellation before completing the declared voxel
count. Decoder acceptance in a discarded partial block is not emitted output.
Before the acquisition fix, B0 preparation took 161.510 seconds (baseline)
and 151.790 seconds (repaired). The final repeats take 1.745 and 0.888 seconds.
The baseline reader summed across workers falls from 289.779 to 1.253 seconds.
These are exploratory before/after observations, not controlled benchmark
medians; the eliminated quadratic construction is established by the source diff.
Final B0 reports a 5 GiB maximum JVM heap and four JVM-visible processors, with
eight explicitly requested executor workers.
<!-- end-resource-results -->

The process heap is sampled every 10 ms during execution. It includes sbt,
the fixture, uncollected garbage and the engine. It is neither a measured
engine live bound nor an exact peak. The 256 MiB engine gate remains
unestablished; the earlier scoped preparation/bank estimates do not fill it.
No complete-workload timing is extrapolated from these partial runs.

`memory-scenario.json` also evaluates the existing reference-array accounting
at the measured N=1,200, bandwidth=127 geometry. Shared preparation/bank arrays
occupy 204,786,040 bytes. Eight simultaneous first-order references add
64,540,480 bytes, reaching **256.85 MiB**; eight full-jet references reach
**321.04 MiB**. These totals already omit worker/build scratch, encoded
responses, input/output blocks, result vectors and object headers. This is a
source-based possible allocation scenario, not a measurement of actual
simultaneous liveness. It shows why a safe eight-worker memory admission cannot
rely on the 195.3 MiB shared estimate alone.

The host is an M2 Pro, macOS 15.1.1, JDK 21 and Node 24, differing from the
frozen reference host. These resource results cannot establish a pass on that
host. The deferred finite-state backend is not activated from an incomplete
uncertified workload or from the changed multistart search budget.

## Validation and source identity

`environment.json` records the runtime and host. `manifest.json` binds the
final source files and evidence. Base is local commit `650acfa3`; the provider
pin is unchanged. Initial integration runs and the pre-fix B0 runs are marked
as such, rather than presented as final-source timing measurements. The
development compile failure and its successful correction are retained in
the logs. No hosted CI or publication is claimed.

<!-- validation-results -->
Full dataset suites: **76 JVM / 62 Scala.js**. Targeted workflow suites:
**8 JVM / 8 Scala.js**. All pass; the final workflow tasks take 65 seconds
on JVM and 272 seconds on Scala.js FastOpt, including their test-task work.
The 48 scientific scenario verdicts remain **Fail**, as required by their
blocking caveats. Final source compiles without Scala compiler warnings.
No repository-wide test run or new hosted validation is claimed.
<!-- end-validation-results -->

## Reproduction

Run bounded batches from the worktree with the pinned Gale provider:

```sh
python3 tools/build/sbt-warm datasetJVM/test datasetJS/test
python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/testOnly *TrialQualificationAuditSuite *TrialStressIntegrationSuite *DecodedTrialCheckpointSuite' \
  'firstLevelLawsJS/testOnly *TrialQualificationAuditSuite *TrialStressIntegrationSuite *DecodedTrialCheckpointSuite'
python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialQualificationAuditMain /private/tmp/family-audit.json'
python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.DecodedTrialCheckpointMain /private/tmp/b0.json dense-blocked 100000 300 8 penalized corrected local-check 2 repaired 32 2.0 120'
python3 -S docs/verification/phrf-qualification-20261007/verify.py
```

## Remaining decisions

Certification needs a complete observation-error envelope, an admitted
reference neighborhood and a charged residual/error bound. The measured
counterexamples must become admission/refusal cases; merely increasing a
solver budget cannot fix tail truncation or distant-reference Taylor error.
Larger horizons and denser banks have explicit bandwidth and memory costs.

Scientific admission then needs the original multi-family and coverage courts.
The full certified workload, measured engine memory and provider/backend
decision follow those prerequisites. No ticket is closed solely because the
new regression tests pass.
