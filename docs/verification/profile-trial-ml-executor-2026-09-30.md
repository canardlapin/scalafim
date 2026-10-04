# Native trial ML executor routing

Candidate base: `0edf75c213c123f5c538518d9ad9d9900cef406c`.
Original Mote: `bd-01M26A285YAQ3H8TGS26BK9T7J`; Fray thread 59.

One shared native ML bundle owns preparation and node determinant setup. Each
executor worker creates one native worker and one frozen-variance decoder. The
existing gather, ordered sink, cancellation and owned termination path is reused.
The backend forwarding method guards a successful response epoch and calls the
existing exact readout on that worker's objective. It neither reencodes a response
nor recreates preparation. Its work appears once in the 22-field legacy receipt;
ML evaluation attempts remain a separate overlapping receipt.

`ProfileVoxelResult.penalizedEnergy` remains raw E. Optional ML evidence contains
raw E/derivatives/amplitudes, D/derivatives, J/derivatives, score and frozen-variance
conditional covariance scale and HJ. Evidence is required at the actual returned
coordinates, including for budget-limited statuses. Unavailable or inconsistent
terminal evidence refuses before payload delivery; exact readout must agree with
raw terminal E and condition means. Conditional SD uses HJ, excluding the prior.
Provenance identifies criterion form, hexadecimal variance and native lambda,
conditional-SD rule and the terminal-evidence requirement. Legacy provenance is
unchanged. The independent raw-E public trial-output view explicitly refuses ML.

Example: construct `ProfileHrfPlan.fromTrialEvents(..., alpha = 0.4,
criterion = ProfileCriterion.TrialRandomEffectsML(0.05))`, prepare with the declared
shared AR whitening and decode policy, and call the existing `prepared.run` or
JVM `prepared.runParallel`. Inspect `criterionEvidence`, `setup.mlSetup` and
`progress.trialMl`; do not add the ML receipt to the legacy trial receipt.

This routing seam does not qualify scientific admission,
original-equation certification, performance or release, and does not close PHRF29.

## Verified candidate and evidence

The actual final shared-lock wrappers exited zero after 77.041063 s (JVM) and
370.508716 s (JS plus CompileAll). These are gate durations, not performance
measurements. JVM executed 134 tests: shared fit/public/native ML/criterion/readout/
output 92, full JVM parallel suite 33, TrialBanded laws 9. JS executed 101: the same
shared suites 92 and TrialBanded laws 9. Counts include inherited suite executions.
Full raw logs have no errors, skips or ignored tests. CompileAll has no compiler
warnings or errors. One sbt multiple-main-classes notice occurs during JS law-test
linking, before the law tests and CompileAll.

Actual commands, per-suite counts and raw/metadata hashes are retained in
`ROOT/logs/profile-ml-executor-parsed-final-gates.json`. The commands use
`ROOT/run-sbt.py`, no provider override, and the hosted Gale pin
`da38f8c429294657d30ec29f06eae3fab636d428`, verified against the used staged checkout.

Final JVM task order runs the full parallel suite as its own task, preserving its
actual executor concurrency. The 2048-voxel ML specimen exercises actual 1/2/8
workers at chunks 1/2/256 with exact ordered results, decoder/status totals,
22-field attempted totals and separate ML worker receipts. Shared checks compare
emitted terminal evidence to fresh coherent native evaluations, E to independent
augmented dense equations and D to the dense response-space covariance. An explicit
two-run AR recurrence verifies run resets and one encoding. A separate analytic
2x2 inverse verifies the frozen-HJ conditional SD rule. Epoch invalidation,
budget-limited status, unavailable terminal (real refused continuous derivatives),
pre-access unsupported routes/public view, cancellation and exact failure receipts
are checked. Existing rejected-candidate and prior/terminal laws run on both
platforms. Setup-refusal translation is a labeled synthetic error-translation
fixture; it does not claim a newly induced native setup numerical failure.

The first scoped check passed 17/18; its extreme-variance negative stimulus did
not make terminal evidence unavailable. It was replaced with the actual derivative
refusal fixture. Check2 passed 18/18. The initial final JVM attempt stopped on a
test comparison between two different typed error channels. Final2 executed 125,
with 124 passing and one new held-reader interruption latch timing out while the
independent suites ran together. Diagnostic clues were added; production cleanup
and the 40 ms/60 s cleanup controls were unchanged. The isolated eight ML checks
passed, then the complete split-task JVM gate passed. This is retained as timing
sensitivity evidence, not a claimed lifecycle repair. All raw/meta and intermediate
source snapshots remain in `ROOT/logs/profile-ml-executor-*`.

## Tested API example

The following calls are exercised using the typed fixture values in
`ProfileHrfFitSuite` (dataset, drive, baseline, config, basis, reader and sink):

```scala
val declared = ProfileHrfPlan.fromTrialEvents(
  dataset, drive, baseline, arConfig, basis, 0.4,
  ProfileCriterion.TrialRandomEffectsML(0.05))
val prepared = declared.left.map(_.message).flatMap: plan =>
  ProfileHrfFit.prepare(plan, selection, parallelWhitening, policy())
    .left.map(_.message)
val outcome = prepared.flatMap(_.run(reader, sink(values)).left.map(_.message))
```

`ProfileHrfFitParallelSuite` exercises the existing JVM extension with distinct
caller-owned readers. No independent raw-E public output view is admitted for ML.

## Source closure

Frozen gate manifest: `ROOT/logs/profile-ml-executor-latch-repro-source.json`.
Seven Scala paths match that tested snapshot exactly. The original qualified
backend, before the forwarding-only addition, has SHA-256
`72f90f44e90807a9c30716708fd9f475127e8c38dcb8c74a1c500f1c4c43dfa4`.
`TrialMlObjective.scala` differs only by making the work receipt public. No solver,
criterion, helper, model, build or provider algorithm/pin was changed.

Only this verification document was finalized after both wrappers stopped; the
full gated document is preserved as `ROOT/logs/profile-ml-executor-gated-document.md`.
`ROOT/profile-ml-executor-final-receipt.json` binds the exact local commit, all eight
final owned hashes, unchanged prerequisite hashes at base and HEAD, task/design
identities, frozen manifest and all actual raw/meta receipts. Independent review
and integration remain. No whole PHRF29/11/14 closure, scientific admission,
original-family certification, throughput qualification, release or publication
is claimed.

Final raw and metadata SHA-256 values:

- `profile-ml-executor-final3-jvm.log`: `326fcbe18beee9a875fe87c5819ec66efa3e89c6c0a5f19663fdccffe386728a`; metadata `1b64cf816996f8db874c7132c9b8c9e8e657f18e7a32b2d7e0d0abd61d084196`.
- `profile-ml-executor-final3-js-compile.log`: `9b9eaf836d7b324ef8bdf79e290f0f09c01cdf65af623829e48865998bd846f8`; metadata `ca1091b2abc5cf0a84a2a23c85fc27396825ced47f28e6bab5d44f5c2f8c1a1e`.

Frozen gate manifest SHA-256: `b9722b72818063a1108e0b1352492dd82eb091ed6bb3b31f7f70bfb3046eb66f`.
