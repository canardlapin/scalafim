# PHRF public decoded checkpoint — 2026-10-07

PHRF-33 is **not admitted and remains open**. The complete public workload now
has an executable diagnostic, and it reveals failures that the historical
component benchmark did not exercise. No finite-state activation follows from
these small samples: they establish preparation and decoding failures, not a
measured full-workload throughput miss.

## What changed

`DecodedTrialCheckpoint` constructs the plan and a bounded synthetic dataset
reader, calls `ProfileHrfFit.prepare` and the public trial-output executor,
and converts accepted amplitudes at the decoder's returned coordinates to
Float32 in a sink. Every delivered refusal stays in the attempted denominator.
A single trial-sized sink buffer is retained. Counters include continuous
factors, failed/budget attempts, terminal verification and output work.
Generating coordinates appear only in fixture construction, never selection.

The JVM `DecodedTrialCheckpointMain` persists a JSON receipt even when setup
refuses. Every receipt is unconditionally `qualification=not-admitted`; exit 0
means the diagnostic completed and wrote its result, not that B0 passed.
It supports dense/regular schedules, the public exact/corrected readout modes,
penalized/native-ML criteria, explicit bank size and explicit decoder budget.
The portable suite checks off-node exact readout against independent augmented
QR, original-equation refusal before reads, exhausted-budget accounting and
native ML output/work. The QR oracle covers the prepared basis, not the original
family. No new public API, module dependency or scientific algorithm is added.

## Measured results

Base `9ba2c57f2b483f8f81a64e30b610f345b13c1784`, hosted Gale
`d03eb99bde389ce9bce21b8e0fc59bec9ac9b4aa`, no local provider override.
Exact added/consumed source hashes and artifact hashes are in `manifest.json`.
The host is Mac14,12, 10 CPUs, 32 GiB, macOS 15.1.1, Java 21.0.12.1 and
Node 24.21.0. This is **not** the historical M3 Max reference-host measurement.
See `environment.json` for the captured commands and JVM settings.

All successful preparations below preserve T=600, N=300, C=3, six nuisance
columns, default Cascade34 chart/horizon, 0.1-second sampling, the 9x9x7
training grid, tolerance 1e-3, max rank 32, 300 held-out points/seed 11,
lambda=1, AR(1)=0.3, onset seed 20260910 and response seed 20260911.
The diagnostic uses one worker and at most 256 attempted voxels; it is not the
100,000-voxel/eight-worker qualification cell.

| Configuration | Attempted / emitted | Refusals | Execution seconds |
| --- | ---: | --- | ---: |
| Dense compiler, eight-node bank | 0 / 0 | 2,727,270 training cells exceed cap 2,000,000 | Not run |
| Blocked compiler, eight nodes, baseline, exact | 256 / 0 | 93 budget; 163 curvature | 3.408 |
| Same, corrected readout | 256 / 0 | 93 budget; 163 curvature | 8.962 |
| Blocked compiler, regular schedule, baseline | 64 / 0 | 1 boundary; 23 budget; 40 curvature | 1.278 |
| Blocked compiler, eight nodes, expanded budget | 256 / 0 | 77 boundary; 16 budget; 163 curvature | 13.200 |
| Blocked compiler, 27 nodes, expanded budget | 256 / 5 | 87 boundary; 26 budget; 138 curvature | 19.065 |
| Blocked compiler, N=1,200 stress, four requested voxels | 0 / 0 | 7,200,000 convolved-term cells exceed cap 4,000,004 | Not run |

`baseline` is `DecodeBudget()` (2 Newton steps, 2 jets, 6 exact evaluations,
4 candidate attempts, stationarity tolerance 1e-9). `expanded` is an explicit
experiment with 6 steps, 8 jets and 2 exact evaluations, keeping the other
limits. The expanded bank is 3x3x3, compared with 2x2x2. These are diagnostic
variations, not amendments to the frozen B0 gates. `BlockedPartial(96)` uses
the existing provider path, attains rank 10 and keeps the held-out tolerance;
it is an explicit compilation change from the currently refused dense default.
The initial four-voxel expanded-budget probe is also retained and marked by
its `DecodeBudget` field; it predates the CLI's named budget options.

The baseline exact/corrected cells each charge 2,048 node scores, 355 jets,
136 exact evaluations, 99 terminal verifications and 235 continuous factors.
Neither reaches readout. Their wall times must not be interpreted as output
throughput or an exact/corrected speed ratio. Concurrent machine work, warmup,
repetitions and GC were not controlled.

All five expanded-bank outputs are off-node; the sink consumes 6,000 Float32
bytes. Maximum conversion error is 2.022e-7 and maximum measured prepared-basis
normal residual is 3.136e-13. The cell charges 680 continuous factors plus
5 exact readout factors. Its `trialWork.voxels=261` counts response encodings,
including five readout encodings; the attempted-voxel denominator is **256**.
These small residuals do not supply original-family error bounds.

Setup, input-fixture construction, basis compilation, execution including
reader/sink time, reader time and sink time are separate receipt fields.
Internal whitening/projection/selection/readout timings are not yet separated;
all remain in the execution total. `heapUsedAtEndBytes` includes resident sbt
and build state; it is neither peak engine memory nor an engine live-state bound.
The fixture also retains its expanded design for the oracle, outside the sink.

## Decision and next acceptance work

1. **Decoder admission precedes throughput tuning.** The eight-node bank consists
   entirely of chart corners. The present curvature fallback requires interior
   neighbors, which this bank cannot supply. This explains an unavailable recovery
   route, not all observed failures. Increasing only the budget did not fix the
   sample; 27 nodes plus the expanded budget emitted just 5/256. PHRF-33/29 should
   next compare an explicitly budgeted initialization/refinement design against
   a dense same-model shape oracle, preserving refusals and terminal diagnostics.
   Do not relabel boundary or curvature failures as successful fits.
2. **Make the stress geometry preparable within bounds.** Keep the blocked basis
   path explicit; investigate bounded lowering of the N=1,200 trial design before
   another stress campaign. Raising array caps is not an accepted remedy here.
3. **PHRF-11 must implement original-equation evidence.** The public
   `CertifiedOriginalEquations` request still unconditionally refuses before reads
   for penalized output; native ML also restricts intent to prepared-basis evidence.
   Binding a rigorous spectral lower bound and family/basis/whitening error terms
   remains necessary. The new residual oracle does not close that gap.
4. After admission and certification are available, run the actual frozen B0 and
   stress workloads, fixed-shape denominators, measured memory and complete phase
   receipts. Make PHRF-33's activation/engineering decision then. PHRF-14/15/16
   remain open; optional finite-state work stays deferred.

The old benchmark's historical `complete*` names and timings are retained for
comparison but now explicitly labeled as components. The cohort/evidence/v2
completion documents link to this corrected status.

## Bounded prerequisite closure

Close `bd-01M3RB3GQ2RBTTDB652CFZ1AJN` (constrained ML determinant jets).
The independent review accepted exact `4ec40483890c92457ad54523a9fe0d7f71562267`
on 2026-09-30, subsequently landed as `3ceba77c`. The current implementation
SHA-256 is unchanged: `32bcc75848be6631ef860e015bc134269a9382a85f0a7e622cde885a283e4ec2`.
Its accepted verification document is also byte-identical (`4ba5636536360b989f918062b6cadfafe25755aa18542fd8d59bf9191d5ab28f`).
The determinant suite differs only by an increased MUnit timeout.
Fresh default-pin tests pass on JVM and JS: 12 determinant laws and 7 conditional
solve laws per platform, covering factor identity, dense/finite-difference jets,
refusals and charged work. This closes that prerequisite only, not PHRF-11 or ML
scientific/performance admission.

## Validation and reproduction

46 distinct targeted test executions passed: 4 new controls plus 19 prerequisite
controls on each platform. `fitBenchJVM/compile` passes. No Scala compiler warnings
or errors; sbt reports its existing multiple-main discovery warning in JS tests.
The final gate log binds the final runner/suite sources. Earlier control and
prerequisite logs retain the initial implementation and probe history.

```sh
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/testOnly scalafim.fmri.laws.profile.DecodedTrialCheckpointSuite' \
  'firstLevelLawsJS/testOnly scalafim.fmri.laws.profile.DecodedTrialCheckpointSuite'
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'fitJVM/testOnly scalafim.fmri.fit.profile.TrialConstrainedLogDetJetSuite scalafim.fmri.fit.profile.TrialConditionalSolveSuite' \
  'fitJS/testOnly scalafim.fmri.fit.profile.TrialConstrainedLogDetJetSuite scalafim.fmri.fit.profile.TrialConditionalSolveSuite'
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.DecodedTrialCheckpointMain /tmp/b0.json dense-blocked 256 300 1 penalized exact checkout-under-test 2 baseline'
python3 -S docs/verification/phrf-checkpoint-20261007/verify.py
```

Other commands, exits and wrapper elapsed times are preserved in `final-gates.log.gz`
and `prerequisite-gates.log.gz`. Raw JSON receipts include the full plan/output
provenance and work counters. `verify.py` checks hashes and receipt consistency;
it deliberately cannot promote this packet to qualification.
