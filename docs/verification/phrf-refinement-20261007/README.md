# PHRF bounded refinement — 2026-10-07

The two retained search counterexamples are repaired by an opt-in bounded
multistart route. **PHRF-33 and the epic remain open and not admitted.** This
packet establishes recovery on the matched prepared-basis fixture. The cost,
original-equation evidence, stress preparation and scientific cohorts still
prevent qualification.

## Repair and provider boundary

`DecodeInitialization.BoundedMultistart` independently refines the center and
all quarter/three-quarter chart points (nine starts in B0). The generic solver
is Gale's projected BFGS, merged in
[Gale PR #21](https://github.com/canardlapin/gale/pull/21) and pinned at
`0a981febc163e539b019aa9379935e21d8987a20`. It uses a projected Armijo search,
resets its inverse metric when the active set changes, and can traverse regions
where the objective Hessian is indefinite. It is a local first-order solver;
neither its stationarity status nor multistart establishes global optimality.

ScalaFIM owns the chart/energy scaling, prior, starts, work allocation, selection
and terminal scientific policy. The bank is scanned once. Each search uses unit
chart coordinates and energy divided by `max(1, abs(best augmented bank energy))`.
The lowest observed final energy anchors a fixed 1e-12 scaled-energy tie window;
within that window, the smallest projected gradient wins. The selected point
then receives a fresh full jet and ordinary Newton terminal verification/polish.
Existing curvature, weak-identification, boundary and refusal checks still apply.
Every oracle call, including a failed call, spends a jet and candidate attempt.
The receipt retains every start, stop reason, last point and work count.

The experiment declares 16 Newton steps, 901 jets, 40 exact evaluations,
30 attempts per step and raw Newton-correction tolerance 1e-6. Each of nine
searches can use at most 90 evaluations (89 iterations), leaving at least
91 jets for terminal work. Their projected-gradient tolerance is 1e-10.
Unused search quota is available to terminal work. These are diagnostic settings,
not an amendment to frozen B0. Default `BankNode`, its numerical budget, and
the earlier `ChartCenterProbe` remain unchanged. The new mode is explicitly
identified as `bounded-multistart/v1` within the existing `decode-budget/v2`
provenance schema.

Gale also narrows `ExactSum.add`/`addAll` to their actual capacity error. New
normalization-only errors on current Gale main had otherwise broken exhaustive
downstream insertion matches. The fix changes the error type, not arithmetic;
the full ScalaFIM AR suites verify compatibility.

## Controlled result

The same 16 B0 voxels and five noise levels from the
[preceding recovery packet](../phrf-recovery-20261007/README.md) are reused without
regenerating or tuning the reference. T=600, N=300, C=3, six nuisance columns,
Cascade34, rank 10, lambda=1, AR(1)=0.3, `BlockedPartial(96)` and the eight-node
bank are unchanged. The dense analytic reference uses independent SciPy
L-BFGS-B starts, augmented QR and finite-difference curvature checks. Generating
coordinates are never search starts.

All 80 returned objective values agree with that reference within 1e-9 after
division by T times signal RMS squared. All 60 reference-interior fits emit
actual public trial readouts; all 20 boundary-preferred fits remain refused.
Strong-signal coordinates and both counterexamples are checked within 1e-5
chart widths. Two public amplitude/nuisance outputs are independently checked
against augmented QR, including the strong-signal counterexample.

| Noise ratio | Reference interior / boundary | New emitted / attempted |
| --- | ---: | ---: |
| 0 | 16 / 0 | 16 / 16 |
| 0.1 | 16 / 0 | 16 / 16 |
| 0.5 | 15 / 1 | 15 / 16 |
| 1 | 10 / 6 | 10 / 16 |
| 2 | 3 / 13 | 3 / 16 |

Ratio 0.1, voxel 10 now reaches the reference interior fit (energy approximately
1.7900486410), rather than stopping at energy 1.8835732663 with negative
curvature. Ratio 1, voxel 3 now selects the better boundary solution (energy
approximately 3.9263841234) and refuses output, rather than accepting the worse
local minimum at 3.9292657532. Raising output counts is not the admission rule.

Objective agreement is not coordinate recovery for every noisy case. Among
refused boundary fits, the largest difference is 0.581 chart widths while
energies still agree to numerical precision. For example, ratio 0.5 voxel 10
has a zero third coordinate in both solutions and different second coordinates.
In Cascade34, that zero undershoot weight removes the component whose rate is
controlled by the second coordinate, making that rate unidentifiable there.
The retained curvature refusal prevents this from becoming an admitted shape
estimate. `comparison.json` reports these coordinate differences explicitly.

`final.json.gz` retains all 80 records, 720 independent search trajectories,
attempted/emitted counts, charged work, execution times and consumed Float32
output counters. `comparison.json` computes errors and status counts against
the unchanged reference. The 60 emitted fits produce 18,000 Float32 values
(72,000 bytes consumed) through the actual public sink.

Final execution totals 399.106 seconds and 32,336 full jets across 80 attempts;
stage execution times are 99.352, 89.313, 78.281, 71.958 and 60.202 seconds.
The complete sbt runner command took 406.5 seconds including its setup.

## Cost and limits

This route evaluates hundreds of full jets per voxel. Each off-node evaluation
currently computes a full Hessian even though BFGS consumes only value and
gradient. Those continuous factors/solves are charged, not hidden. Execution
times in `comparison.json` cover each 16-voxel public run, including recording
and Float32 delivery, but exclude setup. They are diagnostic single-worker
measurements without controlled warmup, repetitions, peak memory or complete
internal phase timing. They cannot establish the 100,000-voxel/eight-worker
qualification gate or a reference-host speed ratio.

The fixture has one generating HRF and condition-constant trial amplitudes.
It does not establish original-family accuracy, random-effects calibration,
coverage, general identifiability or behavior across other HRF shapes. The tiny
native-ML test verifies terminal/readout coherence, not scientific qualification.
The twenty boundary solutions are properties of this criterion and data; their
scientific treatment remains an explicit design question.

The [Scala.js profile](JS-PROFILE.md) identifies the dominant loops and retains
two live V8 samples plus a checked kernel replay. It is a bounded investigation
of the platform gap, separate from qualification. Later JavaScript test timings
include the two 20-second sampling windows; the final JVM public receipt is
uninstrumented.

The next bounded work is to expose a charged value/gradient-only shape oracle
and evaluate a staged search budget against these retained counterexamples,
without weakening terminal admission. Broader generating-shape and scientific
cohorts must guard against overfitting this one fixture. Separately, bounded
N=1,200 lowering and PHRF-11 original-equation evidence remain prerequisites.
Only then can the full frozen B0/stress campaign decide PHRF-33 and PHRF-14/15/16.
Optional finite-state implementation stays deferred.

## Rejected experiments and provenance

Nine independent starts of the old Newton decoder still failed both retained
counterexamples; most stopped immediately at indefinite curvature.
`independent-newton-probe.zip` retains the removed diagnostic hook/harness,
receipt and independent Python projected-BFGS prototype. Its source snapshot
was captured after the bounded adapter was added; it preserves the Newton hook
used by the experiment, but is not asserted to be a byte-exact snapshot of the
whole checkout at the earlier run. These are exploratory artifacts only.

`strict-energy-selection-prototype.json.gz` records the first public bounded run,
before the explicit energy tie policy and final provider pin. All 80 energies
agreed, but a roundoff-level advantage selected a less stationary noiseless
trajectory and caused one terminal budget refusal. Anchoring the tie window
at the minimum of all observed energies, then preferring stationarity within
that fixed window, resolves this case without changing terminal tolerances.
The prototype receipt is not final-source evidence.

The final source base is ScalaFIM `95d2c66b39ae2ab94442168d744dcef940044f4f`.
`manifest.json` binds consumed sources, the exact provider archive, unchanged
reference/input, logs and final receipts. The provider merge tree equals the
locally tested PR head. Provider CI passed; the optional Bugbot scan was skipped
for its usage limit, while repository approval and security checks passed.
The historical checkpoint/recovery manifests are preserved and should be checked
at their original revisions; their source hashes deliberately differ now.

## Validation and reproduction

Validation results are recorded in `validation.json` and compressed logs.
Full provider suites passed 804 JVM / 792 JS tests. ScalaFIM full fit suites
cover 694 JVM / 637 JS tests, full AR suites cover 155 JVM / 153 JS tests, and
the four public recovery/workflow suites cover 21 tests per platform.
`scalafimCompileAll` checks both platforms. Final ScalaFIM gates and the final
receipt use the hosted merged Gale pin, with no local override.
There are no Scala compiler warnings; sbt reports its existing multiple-main
discovery warning when loading the diagnostic entry points.

The exhaustive public sweep is intentionally slow, especially on Scala.js.
The weekly generated-law calibration job now allows 60 minutes instead of 30
so both platforms retain all 80 cells. This is a CI job allowance; numerical
budgets, tolerances and qualification limits are unaffected. Hosted ScalaFIM CI
has not been run for this local branch; the recorded gates are local executions.

Host: Mac14,12, 10 CPUs, 32 GiB, macOS 15.1.1, Java 21.0.12.1 and Node 24.21.0.
The warm wrapper heap setting is 5g, while `.jvmopts` also supplies `-Xmx6g`.
Concurrent machine work was not excluded. This is not the historical M3 Max
reference host. Reference-generation versions and inputs are unchanged from
the preceding packet.

```sh
./tools/prepare-pinned-dependencies.sh
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm scalafimCompileAll \
  arJVM/test arJS/test fitJVM/test fitJS/test
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/testOnly scalafim.fmri.laws.profile.TrialRefinementSuite scalafim.fmri.laws.profile.TrialRecoverySuite scalafim.fmri.laws.profile.DecodedTrialCheckpointSuite scalafim.fmri.laws.profile.ConditionProfileFitSuite' \
  'firstLevelLawsJS/testOnly scalafim.fmri.laws.profile.TrialRefinementSuite scalafim.fmri.laws.profile.TrialRecoverySuite scalafim.fmri.laws.profile.DecodedTrialCheckpointSuite scalafim.fmri.laws.profile.ConditionProfileFitSuite'
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialBoundedSearchCandidate /tmp/phrf-bounded-final.json 901'
python3 -S docs/verification/phrf-refinement-20261007/verify.py
```

The verifier checks artifact/source integrity, all attempted cells, retained
searches, budgets, reference agreement and output consistency. It cannot
promote this diagnostic to PHRF qualification.
