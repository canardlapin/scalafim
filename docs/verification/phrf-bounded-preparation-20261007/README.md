# PHRF bounded trial preparation — 2026-10-07

The N=1,200 preparation blocker identified in the [public checkpoint](../phrf-checkpoint-20261007/README.md)
is removed through an explicit blocked lowering policy. PHRF-33 remains **open and not admitted**:
this is preparation and same-model numerical evidence, not original-family certification,
a complete B0/stress throughput measurement, or a measured engine peak-memory result.

## Implementation

`TrialBasisDesign` names the basis, membership, caller trial order, reversible
canonical event-row maps, and access to basis-major trial blocks. Existing
`ExpandedTrialDesign.lower` remains the dense API, with its existing convolution
cap. `TrialDesignLowering.Blocked(size)` instead retains the validated event
schedule and materializes at most `size` trial columns per basis component.
It uses the same `EventTerm` convolution, sampling frame, precision, duration
semantics and run isolation as dense lowering. It does not create an all-trial
event design matrix or retain an all-trial convolved matrix.

Preparation whitens complete time columns one block at a time, then packs each
trial's observed support. In particular, it does not truncate an MA tail at the
unwhitened HRF horizon. Packed cross-basis Gram construction preserves the old
time summation order. Nuisance geometry, penalties, decoder budgets, numerical
solvers, terminal checks and original-equation admission rules are unchanged.
No Gale provider change or new generic numerical solver is involved.

The public opt-in is:

```scala
policy.copy(trialPreparation = TrialPreparationPolicy(
  lowering = TrialDesignLowering.Blocked(32),
  maxRetainedValues = 16000000L
))
```

Dense lowering remains the default. Preparation now checks a default limit of
16 million retained numeric entries before allocating packed storage or the
Gram arrays. Int entries are conservatively charged as Double entries. Invalid
limits fail construction; an exceeded limit returns `StorageLimit` with the
required and permitted counts. This is a scoped preparation limit, **not an
engine-memory budget**. Basis and source metadata, convolution and whitening
scratch, temporary packed copies, node banks, workers, output buffers and VM
object overhead are outside that limit. Counts are checked before narrowing to
array indices, including hostile multiplication sizes.

The retained-source count is zero for a blocked schedule source; the preparation
receipt records the number of blocks and largest materialized block. Nondefault
preparation policies are bound into public provenance. The dense default's
existing provenance goldens remain unchanged. Preparation/readout `source` now
has the general `TrialBasisDesign` type; concrete dense values retain `.term`.

## Evidence

Work starts from local commit `80b43d69`, with hosted Gale
`0a981febc163e539b019aa9379935e21d8987a20`. `manifest.json` binds the final
source and artifact hashes. Earlier evidence packets describe their own source
revisions; their complete source-hash checks require those revisions.

<!-- results -->
Final portable stress receipts agree on all structural counts:

| Measurement | JVM | Scala.js FastOpt |
| --- | ---: | ---: |
| Trial blocks | 38 | 38 |
| Largest convolved block (Double values) | 192,000 | 192,000 |
| Retained dense source values | 0 | 0 |
| Packed bandwidth | 127 | 127 |
| Preparation-owned numeric entries | 9,116,727 | 9,116,727 |
| Scoped preparation + eight-node-bank bytes | 204,786,040 | 204,786,040 |
| Public preparation seconds | 2.538 | 13.667 |

The rejected dense shape has 7,200,000 convolved values (57,600,000 data bytes).
The largest block has 192,000 values (1,536,000 data bytes), 37.5x smaller than
the rejected dense shape. These are array counts, not VM heap measurements.
The 204,786,040-byte shared estimate includes preparation and reference arrays;
it must not be added to the preparation estimate again. It is about 195.3 MiB
before excluded worker/reference-build scratch, source/basis metadata and object
overhead. Timing is from single diagnostic runs on the host in `environment.json`;
there is no controlled speedup or throughput claim.

The four-voxel JVM diagnostic completes public execution: **4
attempted, 0 emitted**, statuses `{"BudgetExceeded": 2, "CurvatureNotPositive": 2}`.
Preparation took 4.317s and execution including reads/sink
0.827s. It uses the explicitly named
baseline decoder budget, not the repaired 901-request multistart policy. Refusals
remain in the denominator. Original-family admission still refuses. This removes
the setup failure and makes the stress workflow runnable; it does not qualify
its decoding or performance.

Final gates pass: full design **481 JVM / 480 JS**,
full fit **705 JVM / 647 JS**, and packed/dense/public
workflow laws **18 per platform**. The earlier recovery sweep passes
**16 tests per platform**, including all 80 reference cells and boundary refusals.
The final `scalafimCompileAll` gate compiles both platforms without Scala compiler
warnings. Both corrected development failures and final logs are archived; the
workflow log includes the existing multiple-main discovery warning. No hosted
ScalaFIM CI or original-family/full-workload admission is claimed.
<!-- end-results -->

The portable stress test uses T=600, N=1,200, C=3, six nuisance columns,
Cascade34's default chart/horizon, 0.1-second sampling, the 9x9x7 training grid,
tolerance 1e-3, max rank 32, default held-out parameters, blocked basis compilation
with 96 training columns, lambda=1, AR(1)=0.3 and onset seed 20260910. It verifies
that dense lowering still refuses, then completes the public eight-node-bank
preparation with 32-trial blocks. The test itself requests no voxel data.

Dense-versus-blocked tests cover interleaved runs, mixed TRs, event durations,
partial blocks, caller identity, IID, run-specific AR and MA whitening, packed
geometry and off-node derivatives. Public exact/corrected readouts preserve
trial and nuisance outputs; the legacy decoded trial readout is also checked
against independently assembled dense equations. The existing public diagnostic
fixture preserves its generated inputs, refusal/status counts and Float32
output checksum when switched to blocked lowering. Its independent QR checks
remain prepared-basis checks, not original-family certificates.

## Reproduce

Use the checked-in Gale pin in `build.sbt`, with no local provider override.
Tests are portable and run under the normal FastOpt Scala.js test stage:

```sh
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'fitJVM/testOnly *TrialBandedPreparationSuite *TrialPreparationStressSuite' \
  'fitJS/testOnly *TrialBandedPreparationSuite *TrialPreparationStressSuite'
python3 -S docs/verification/phrf-bounded-preparation-20261007/verify.py
```

The existing diagnostic runner accepts an optional final `trial-block-size`.
Its `dense-blocked` geometry name still means random dense event timing plus
blocked **basis compilation**; the new trailing argument controls **trial
lowering** independently:

```sh
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.DecodedTrialCheckpointMain /private/tmp/phrf-stress.json dense-blocked 4 1200 1 penalized exact local-check 2 baseline 32'
```

The runner also writes structured preparation counts and retained source values.
Its fixture contracts the basis blockwise; it still uses a T*N contracted design
while constructing synthetic responses, outside the measured public execution.
The optional independent QR oracle remains a deliberately dense diagnostic.
Every run remains `qualification=not-admitted`, including successful execution.

## Remaining epic gates

Original-equation certification (PHRF-11) still needs rigorous spectral and
family/basis/whitening error bounds. The repaired multistart decoder's budget
and recovery evidence still need broader scientific admission. Complete frozen
B0/stress execution, fixed-shape denominators, phase timings and measured live
memory remain outstanding. In particular, a small lowered block and a scoped
shared-array estimate do not establish the 256 MiB engine gate. Finite-state
activation remains deferred; this change does not close the epic.
