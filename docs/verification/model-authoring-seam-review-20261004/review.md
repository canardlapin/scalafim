# Model-authoring scientific seam review, 2026-10-04

Verdict: **changes required at numerical boundary inputs**. Inspection found no
ordinary-scale semantic mismatch in the bounded seam. This is a fresh independent
read-only review, not a renewal obtained by updating the previous hashes.

The reviewed production snapshot was captured at 2026-10-04T11:42:04Z from the
dirty working tree based on `53097f3f43fa125cd166a99268f192f4a5b12311`.
`source-snapshot.tar.gz` and `snapshot-manifest.json` preserve the build inputs;
the base commit alone does not identify this candidate. The manifest contains
2,300 files from modules, project, tools, and root build/configuration files.
All 15 staged dependency repositories were clean at inspection; their exact
revisions are retained in `dependency-sources.json`. Supplemental review probes
are retained separately and were added only to the temporary test snapshot. Production source files were not changed.

The independent reviewer was the read-only `lean_expert` agent
`/root/seam_review`. Its review was performed without builds or reliance on the
old verdict. The parent independently inspected the relevant lowering and
transport functions, regenerated the stored numerical oracle in memory, and
ran the execution checks described in `execution.json`.

## Scope and supported contracts

- **Global basis transform.** For original columns X, divisors D, and the serial
  transform T, the returned design is X D^-1 T and the effective response basis
  is H D^-1 T. Resetting the output column divisors avoids transporting them
  twice. Effective HRFs belong to each cell/modulator group and are read per
  column; the term HRF retains the source identity. A response functional's
  transformed weights are f(H) D^-1 T. Tests compare response estimates and
  variances and refuse selection through the old basis identity.
- **Declared response discretization.** Point readouts report Exact evaluation.
  Windows require a registered primitive or an explicit trapezoid policy; the
  resulting receipt propagates through the DSL and structural T/F metadata.
  Window invariance is relative to the declared quadrature, not analytic
  integration. The unchecked grid-size boundary is finding R1 below.
- **SPM informed basis.** The canonical, temporal and dispersion columns use
  individually sum-normalized kernels, one-second temporal and 0.01 dispersion
  differences, then serial projection without mean removal on the TR/16 kernel
  grid. One common canonical sample-sum factor relates the SPM-normalized
  columns to ScalaFIM's continuous units. The kernel retains its full 32-second
  window. Formula lowering requires one TR across runs for this convention.
- **Per-run response scaling.** A run-local coefficient selects that run's
  unique contributing event divisor. A coefficient shared across runs requires
  one common divisor. Mixed event divisors, absent contributing events and
  ambiguous receipts are refusals. Scan-column and event-level divisors compose
  in the response weight. Coefficient-level readout is a different estimand and
  remains available where response-level readout is refused.
- **Observed-only formula orthogonalization.** The first modulator is centered
  on its observed rows within each run/cell. Each later target is residualized
  against an intercept and earlier streams on that target's observed rows.
  Missing target entries stay at zero; missing predecessor entries enter at
  that predecessor's zero reference. This is the declared missingness policy,
  not complete-case intersection across all streams.

## Findings

### R1: unchecked trapezoid interval count

`modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/Basis.scala:365` converts
`ceil(width / maxStep)` to Int without a bound. A publicly valid positive step
of 1e-20 seconds over a one-second window saturates at Int.MaxValue. On inspection, the implementation proceeds into kernel evaluation rather than
returning a typed refusal.
If allowed to finish, its effective step would exceed the declared maximum and
`intervals + 1` would overflow the receipt's sample count. The retained Scala probe is designed to stop at the first evaluation with a
sentinel exception; it compiled but did not run. No billion-iteration run was
attempted. Independent bounded Python arithmetic confirms a requested count of
1e20 intervals versus an Int capacity of 2,147,483,647, and an effective step of
4.6566e-10 seconds if saturated. Receipt overflow follows from the source
expression `intervals + 1`; it was not exercised through Scala.

Required correction: validate a finite, representable interval count and an
explicit work limit before conversion or kernel evaluation. Oversized requests
must return InvalidDiscretization. Accepted requests must have a positive,
representable sample count and respect the declared maximum step within a
documented floating-point tolerance.

### R2: nonzero collinear columns reported as an exact-zero group

`modules/design/shared/src/main/scala/scalafim/fmri/design/event/ConvolvedBasisOrthogonalization.scala:56`
uses Euclidean norms formed by unscaled sums of squares (`:132`). Public custom
kernels with collinear columns of amplitude 1e-200 produce finite nonzero design
entries, but every squared entry underflows. The implementation accepts this
group unchanged with identity transform and rank 0. Its documented exception is
for exactly zero columns; collinear nonzero columns should be refused.

Required correction: use scale-safe norms and check actual entries when deciding
whether a group is exactly zero. Verify that tiny nonzero identical columns
return a typed degeneracy error while genuinely zero groups retain rank 0.

### R3: reference SVD truncation can defeat orthogonality

`modules/design/shared/src/main/scala/scalafim/fmri/design/event/ConvolvedBasisOrthogonalization.scala:69`
sets a singular-value cutoff that can discard an earlier accepted reference.
The implementation does not refuse this rank loss and subsequently increments
the reported rank (`:95`). For columns x1 = 1e-18 e1, x2 = e2, and
x3 = e1 + e2 + e3, the third projection can drop x1, leaving e1 + e3. Its
normalized correlation with x1 is 1/sqrt(2), although the receipt says rank 3.
The supplemental Scala probe constructs this design through public Hrf.multi
and ordinary impulse convolution. It compiled but did not run. Independent
NumPy SVD with the declared cutoff returns the predicted residual and cosine
0.7071067811865475; actual Gale/Scala runtime confirmation remains outstanding.

Required correction: normalize the reference solve and transport coefficients
back, using the existing Gale capability, or return a typed refusal when an
accepted reference loses rank. Any successful result must satisfy normalized
pairwise orthogonality. This finding concerns highly unequal column scales;
ordinary-scale fixtures do not cover it.

## Evidence and qualification boundaries

`execution.json` records commands, actual statuses, suite counts and log hashes.
The existing JVM selections passed 61 tests (HRF 11, design 21, fit 29). The
first supplemental attempt timed out after 300 seconds waiting for the host
semaphore. A final combined supplemental JVM/JS attempt remained queued for
481.76 seconds and was cancelled after verifying the process belonged to this
session and had not started a build child. Both supplemental Scala suites
compiled on JVM during the initial gates, but their three probe tests and the
actual BasisGeometrySuite/SPM design selections did not run. All JS checks
remain unrun. These are environmental limitations, not test failures. The
scratch sbt server was stopped through its worktree-specific shutdown command.

The supplemental suites intentionally assert the predicted defects: a passing
probe would confirm a defect and would not qualify the implementation. Neither these
probes nor green ordinary fixtures change the scientific verdict.

The stored SPM fixture was independently regenerated using NumPy 2.4.3 and
SciPy 1.17.1. All 771 entries at TR 2 seconds / three columns and all 1,424
entries at TR 0.72 seconds / two columns reproduced exactly. It is a NumPy
reimplementation, **not output produced by MATLAB/SPM**. Its formula was checked
against upstream [spm_get_bf.m](https://raw.githubusercontent.com/spm/spm12/main/spm_get_bf.m)
and [spm_hrf.m](https://raw.githubusercontent.com/spm/spm12/main/spm_hrf.m).
The design fixtures establish grid-aligned kernel sampling and ScalaFIM's
boxcar convolution; they do not establish complete SPM design-matrix parity.

This review does not qualify general fit/inference calibration, performance,
all model-authoring features, unrelated dirty changes, the regex proposal, or a
repository-wide hash-checking CI gate. Additional discriminating evidence would
be needed for three or more modulators with distinct missingness masks after
subset/drop filtering, or for a claim of parity with actual SPM execution.

The old `model-authoring-20261002/review.json` is preserved in
`previous-review.json`; its live record is marked superseded. Four of its five
recorded source hashes differed at the start of this refresh. The new verdict
belongs only to the frozen source hashes in `review.json`. Live-source comparison
at completion is recorded in `current-source-comparison.json`.

## Replaying the evidence

Extract `source-snapshot.tar.gz` into a scratch directory. Copy the two retained
probe suites into their matching HRF/design shared test package directories;
this is the separately hashed overlay, not a production patch. Initialize scratch
Git metadata with the recorded base commit (without checking out over the
extracted files), or use an isolated checkout of that base and overlay the
archived files. Execute the command vectors in `execution.json` from the scratch
root. The oracle check accepts that scratch root as its sole argument.

The first JVM command included an unmatched `ResponseFunctionalSuite` selector;
only `TemporalDerivativeConventionSuite` matched it. The supplemental command
explicitly selects the actual `BasisGeometrySuite` window-functional tests.
Startup Git HEAD diagnostics in the first log came from initially missing
scratch metadata, which was populated with the frozen base before subsequent
checks. These diagnostics and a GC warning remain in the log; no warning-clean
whole-build claim is made. `boundary_analysis.py` is an independently executed
arithmetic/NumPy toy analysis, and its log is retained. It supports the review
reasoning without substituting for the blocked Scala/Gale or JS execution.
