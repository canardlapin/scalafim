# First-order PHRF search — 2026-10-07

This follow-up to the [bounded refinement repair](../phrf-refinement-20261007/README.md)
reduces the cost of each search evaluation. PHRF-33 and the epic remain **open / not admitted**.
The frozen B0 performance, original-family and scientific acceptance gates are unchanged.

## Implementation

`FirstOrderShapeObjective` is an optional backend capability. The penalized
trial-banded backend supplies a value, gradient and amplitudes in
`ProfileGradientBuffer`. Its reference builds `1 + d` components: four rather
than ten in the Cascade34 chart. The first-order reduction reads no second
components and performs no curvature check. Full jets still supply the fresh
terminal verification and all subsequent Newton, curvature and identification
checks. The nine starts, per-start caps, tie rule, prior, scaling and terminal
tolerances are unchanged; default BankNode and ChartCenterProbe are unchanged.
Native ML and other backends retain full-jet search until they expose the capability.

The existing `maxJets` quota remains a cap on total derivative requests.
`DecoderCounters.jets` and public `ProfileDecoderWork.jets` include a separately
reported `firstOrderAttempts` subset, including failed calls. Backend receipts
separate first-order attempts/failures from full-jet attempts/failures and
aggregate all banded solves and RHS. The opt-in policy identity advances to
`bounded-multistart/v2`; older evidence remains bound to its original revision.
The backend still samples the HRF family full jet, projects only the requested
components, and builds the scalar determinant in each reference. The change
does not eliminate every computation unnecessary for first-order search.

<!-- final-results -->
Final uninstrumented JVM public execution took **125.382s**
versus **399.106s** in the preceding packet
(3.18x in these single diagnostic runs).
All 80 results and all 720 trajectories, including stop reasons and evaluation
counts, are identical to that JVM receipt. The 60 public outputs and their
checksums are identical; all 20 refusals remain. Of 32,336 derivative requests,
**32,256** are now first order and
**80** remain full jets. The complete independent
reference check still passes. See `comparison.json` for per-noise timings and statuses.

Final gates: warning-clean `scalafimCompileAll`; fit **697 JVM / 640 JS**;
public recovery/workflow **22 JVM**; the complete refinement suite **7 JS FullOpt**
and the other recovery/workflow suites **15 JS FastOpt**. The fixed-call probe
also passes on both platforms. Both shipping test compilations and restored
FastOptStage pass after removing the temporary probe. No hosted ScalaFIM CI run
or full-workload qualification is claimed.
<!-- end-final-results -->

## Equal-work measurements

The archived `paired-probe.scala` uses one B0 response (ratio 0.1, voxel 10)
and 16 fixed chart points. Each mode gets 64 warmup calls, followed by five
alternating-order repeats of 256 calls. Fixture/preparation time is excluded.
All values and gradients at the probe points agree exactly within each platform;
every repeat has the same checksum. JVM and JS checksums differ at rounding level.

| Platform | Full jet, median seconds | First order, median seconds | Speedup |
| --- | ---: | ---: | ---: |
| JVM | 3.250 | 0.908 | 3.58x |
| Scala.js FullOpt | 5.666 | 1.688 | 3.36x |

Each full call spends 21 banded solves / 103 RHS; first order spends 9 / 43.
These are fixed-work diagnostic measurements on Mac14,12 / Node 24.21.0 /
Java 21.0.12.1, with other host activity uncontrolled. They are not full B0
throughput or peak-memory measurements. `paired-timing.json` retains all repetitions.

The public FullOpt ratio-0.1 batch passed in 59.625 seconds versus the preceding
156.107-second full-jet batch (2.62x). Both are single diagnostic timings with
different warmup/host activity. The new batch includes all 16 public outputs,
the recovered interior counterexample and independent augmented-QR readout check.
The fixed-call comparison above is the cleaner estimate of oracle cost.

## CPU profiles and Gale scope

Both CPU samples use the verified worktree's Node child and a localhost inspector,
without pausing execution. The baseline sample occurs during a fixed 1,536-call
full-jet probe. The second sample occurs during the noise-0.5 public search using
first-order evaluation. The latter test timing includes profiling overhead;
noise 0 and 0.1 completed before attachment. The equal-work timing probe is uninstrumented.

| Self time | Full-jet fixed-point sample | First-order public-search sample |
| --- | ---: | ---: |
| Derivative-band assembly | 28.33% | 36.97% |
| Band product plus reference-solve assembly | 33.14% | 12.98% |
| Gale banded triangular solves | 14.86% | 22.76% |
| GC | 0.52% | 0.40% |

FullOpt can inline the band product into its caller, so those two functions
are grouped. Profiles sample different points and are not a matched timing
experiment; a rising percentage does not imply increasing absolute cost.
Raw profiles, mapped call-frame summaries and aggregate shares are retained.
Generated bundle/map hashes are recorded; their reproducible multi-megabyte
contents are excluded from the packet.

A packed band-by-block operation is still plausible Gale work, but the entire
sampled product/assembly group now accounts for about 13%. Even eliminating
that whole group would only imply about 1.15x overall under an unchanged profile;
that is a hypothetical upper bound, not a measured gain. No new Gale API or pin
change is included here. Reducing reference assembly/solve work and establishing
a cheaper search policy across broader recovery cohorts are the next performance
investigations. An envelope-gradient formulation may avoid differentiated solves;
it needs an independent derivation and the same parity/refusal checks before use.

## Validation and scope

`verify.py` reuses the preceding packet's independent 80-cell oracle checks,
then requires identical retained trajectories, terminal results and public
outputs against that packet. It separately checks first-order work, actual
solve/RHS reductions, timing repetitions, source/artifact hashes and restored
shipping test sources. Run it with:

```sh
python3 -S docs/verification/phrf-gradient-20261007/verify.py
```

The shipping tests add truncated first-order reduction and finite-difference
checks, exact full/first-order parity, failure charging, prior handling, fresh
terminal verification and receipt aggregation. Four additional generating HRF
shapes exercise first-order B0 gradients against full jets and finite differences.
Those are derivative checks, not a broader search-recovery or scientific calibration
campaign. No staged search-budget cut has been made.

Final gate counts, raw public output and end-to-end comparison are retained in
`validation.json`, `final.json.gz` and `comparison.json`. Logs bind the commands
and outcomes. The temporary fixed-call probe is archived and removed from the
shipping suite; the regular refinement suite remains complete.

The combined FullOpt invocation printed all eight tests passed, then exhausted
its sbt heap while reapplying `FastOptStage`. That server alone was stopped;
the invocation exited 1 during reset. An overlapping queued compile invocation
also exited 1 on disconnection without completing its gate. Both failed command
logs are retained. The full validation sequence was restarted on a fresh server;
its explicit stage check establishes restoration. Test success and command-reset
failure are recorded separately, rather than reporting the combined command
as successful.

To reproduce the fixed-call experiment, temporarily copy `paired-probe.scala`
to `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/profile/TrialOracleProfileSuite.scala`
and use `firstLevelLawsJVM/testOnly scalafim.fmri.laws.profile.TrialOracleProfileSuite`.
For JS, in a single sbt process set `firstLevelLawsJS / scalaJSStage := FullOptStage`,
run the same JS test selector, then restore `FastOptStage`. Remove the temporary
source and compile both test configurations afterward. The whole public sweep
runs with `TrialRefinementSuite` and its normal assertions; no case selector is used.

The next epic prerequisites remain bounded N=1200 preparation and PHRF-11
original-equation certification, followed by the complete B0/stress and scientific
gates. These diagnostic speedups do not close those tickets or activate PHRF-10.
