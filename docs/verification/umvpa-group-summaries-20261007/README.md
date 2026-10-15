# UMVPA M5.03 — subject summaries and prediction

Ticket: `bd-01M2BNHY1PTWTMYZR1VQYBVRPJ`. Implementation began from local
`69c034042c419fb8e166685458367b4474664629`. The source manifest binds the
actual files and provider pins used for the final gates; concurrent PHRF work
is outside this packet. No provider revision or module dependency was changed.

## Scientific and implementation scope

`SubjectGroupSummary` consumes the existing covariance-preserving
`SubjectGroupInput` and delegates every inferential calculation to `group`.
The explicit population contract records independent subject sampling,
conditioning, first-level nuisance and multiplicity family. It returns
measurement-by-task means, SEs, pointwise unadjusted p-values, Q, I² and tau².
Individual expression and full joint covariance remain attached through the
original input. Empirical variance includes sampling noise and is separately
labelled from estimated heterogeneity.

Known-variance Gaussian common effects and approximate population means are
different typed scopes. The former requires immutable Known covariance origin
and fixes tau² at zero. The latter preserves the explicit native tau/reference
policy and approximation justification, including estimated covariance and df
provenance. No approximate calculation becomes a calibrated scientific claim.
The summary refuses any partial native fit rather than omitting a coordinate.
Task-linked operators retain their original task axis and operator units.

`HeldOutSubjectPrediction` adds a subject boundary to the admitted M4.06
`ComponentPredictionHeads` and `ComponentConfirmation.incremental` procedure.
It does not implement another trainer, prediction kernel, tuning protocol or CV
engine. Shared-coordinate learning, head training and assessment each carry
row-bound subject labels. Assessment subjects must be absent from shared
learning. Shared heads use learning subjects only; subject adaptation trains
OLS heads on separate rows/units of one held-out subject while preserving the
exact shared projections, support, rotation, preprocessing and loss metric.

The adapter retains actual brain/target evidence, fitted head identities and
exposure accounts. M4.06 unit losses are weighted by unit row count within a
subject, then subjects receive equal weight. Reduced heads are actually
refitted in training by M4.06. Negative improvements are preserved.
`SubjectPredictiveSummary.combine` requires the complete ordered unique external
cohort and matching shared-learning and adaptation contracts. Duplicate actual
assessment evidence cannot be relabelled as additional subjects. These
descriptive outputs do not produce population tests, intervals, prevalence or
an all-subject claim. Multiple CV folds cannot masquerade as independent people.

Separate subject fitting plus frozen transport remains the scope. Joint
hierarchical spatial optimization, joint covariance inference, calibrated
predictive population inference and prevalence are unavailable. Caller labels
and exposure snapshots cannot authenticate renamed physical observations or
unreported access.

## Independent numerical expectations

[`oracle.R`](oracle.R) uses independent base-R dense least squares, weighted
means and a scalar `uniroot` solution to Q(tau²) = n − 1. It imports no Scala
helper or fitted coefficient. [`expected.tsv`](expected.tsv) retains every
mean, SE, Q, I², tau² and p-value at two measurements and two task coordinates.
Portable tests embed these expectations with explicit tolerances.

The three-subject effects are `(1,3,5)` and `(2,4,6)`, with sampling variances
`(1,4,1)` and `(4,1,1)`. Measurement two scales effects by ten and variances by
nine. The first common-effect mean/SE is `(3, 2/3)`; its empirical variance is
4, Q is 8 and I² is .75. PM estimates tau² = 3, with mKH SE sqrt(14/9).
The second measurement's second task has PM tau² ≈ 377.655309153624.

The predictive fixture uses a correlated two-component Walsh design,
intercept/drift nuisance and target metric diag(1,2). Subjects have six and two
assessment rows. Their equal-subject mean improvements are
`(3.3066243270259355, 12.172649730810374)`, versus pooled-row
`(4.5, 12.75)`. Empirical subject improvement variances are
`(11.3931639747704, 2.66666666666667)`. The tests exercise actual M4.06 head
fitting and prediction, including subject adaptation; they do not accept
caller-supplied summary numbers as evidence of predictive execution.

Reproduce the independent fixture:

```sh
LC_ALL=C LANG=C Rscript \
  docs/verification/umvpa-group-summaries-20261007/oracle.R \
  docs/verification/umvpa-group-summaries-20261007
```

Fixed-effect expectations use 1e-10 absolute tolerance. PM means/SEs use 1e-8,
p-values 1e-9, and tau² uses 1e-9 times max(1, expected tau²). The test also
checks the defining Q equation to 2e-9, matching the native n=3 solver's
`|Q-df| <= 1e-9*df` stop. Predictive losses use 1e-11 and their empirical
variances 1e-10.

An initial expanded test used absolute tau² tolerance 1e-7 even at variance
377.7. The native root differed by 1.89634e-7, within its declared Q tolerance;
that invocation failed and stopped before JS. Its log and failure XML are
retained. The final test uses the variance-scaled tolerance and independently
checks the Q residual. No production numerical tolerance was changed.

## Engineering gates and handoff

The final execution receipt records exact commands, test counts, source hashes
and retained log hashes. The source covers the two new production units and
their suites, the consumed subject/group and component-procedure contracts,
the module guide and independent R fixture. All tests are shared and run on
both JVM and Scala.js. Opt-in scientific campaigns skipped by MVPA are recorded
as skips, never passes.

| Module | JVM passed | JS passed | Skips |
| --- | ---: | ---: | --- |
| `mvpaGroup` | 27 | 27 | None |
| `mvpa` | 456 | 456 | One opt-in campaign per platform |
| `group` | 75 | 74 | None |
| **Total** | **558** | **557** | **Two opt-in skips** |

All six final commands exited 0 with no compiler warnings or test errors.
The 27 group-adapter tests include 19 new tests and eight existing bridge tests.
The [execution receipt](execution.json) binds the [adapter log](logs/adapter.log),
[core JVM log](logs/core-jvm.log) and [core JS log](logs/core-js.log).
Two warm clients queued tasks on the same resident server; its core JS log also
echoes the adapter JVM completion. That duplicate output is not counted again.

Refusal controls cover learning-subject contamination, adaptation on learning
rows, another subject's calibration, incompatible common features, foreign
subject columns, borrowed head accounts, changed/unknown exposure, units
spanning subjects, incomplete/repeated/reordered cohorts, duplicate assessment
evidence, allocation limits, too few subjects, partial native map failure and
estimated-to-known uncertainty promotion. Positive controls verify source
application and that assessment does not refit heads.

This packet supplies deterministic numerical parity and engineering admission,
not Monte Carlo type-I error, interval coverage, power or independent scientific
review. M5.04 remains responsible for predeclared group calibration and
adverse-baseline qualification after M4.09. M5.06 can consume the API examples
in the [module guide](../../../modules/mvpa-group/README.md). Those tickets and
the M4/M5 release gates are not closed by this implementation.
