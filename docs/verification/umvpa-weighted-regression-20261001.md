# UMVPA M1.08 target-block weighting

## Contract

This completes the target-block weighting extension of the previously verified
scalar/separable multiresponse ridge adapter. The method and architecture notes
place block weights in target geometry, where they prevent a large feature block
dominating solely by coordinate count. They do not uniquely specify a weighted
predictive ridge objective. This change explicitly chooses a fixed diagonal
**selection and assessment utility**, retaining the existing provider objective.

For a disjoint, exhaustive target partition with block mass `a_b > 0`, each
coordinate in block `b` receives `w_j = a_b / size(b)`. Inner loss is

`sum_{u,i,j} w_j (y_ij - prediction_ij)^2 / (assessmentAppearances * sum_j w_j)`.

An appearance is one assessment row in one usable inner unit; repeated
assessment appearances remain in the denominator. The implementation stores
normalized weights, their normalized mass, the corresponding squared-error
numerator per candidate penalty and the explicit denominator. Uniform
coordinates preserve the original pooled mean-squared-error convention.
Per-output metrics and prediction values stay in their original units. Pooled
R-squared uses separately centered target SSTs with the same weights, and
preserves the conservative rule that any constant target reference makes it
undefined.

Each fixed-lambda provider ridge remains centered and unstandardized with an
unpenalized intercept and unscaled penalty. Its solutions also minimize
`sum_j w_j (SSE_j + lambda * ||beta_j||^2)` for positive fixed weights.
Weighting only residuals would imply different effective penalties and is not
implemented. No predictor scaling, correlated target covariance, GLS, learned
target normalization or pattern-first Gaussian fitting is implied.

`RidgeTargetGeometry` is immutable and constructed from the full identified
response axis. It rejects missing, multiply assigned, foreign or repeated target
keys, empty/duplicate blocks, blank declarations and nonpositive/nonfinite or
unrepresentable expanded/normalized weights. Block membership order is not
scientific identity; block names and membership are hashed canonically.
The exact geometry follows the fitted model, selection receipt, result,
standalone OOF rows, refit and domain prediction APIs. Axis and target names
derive from that geometry. Provider pipeline `run` retains its required raw
array contract; domain `predict` returns the identified values. Geometry enters
the actual plan fingerprint and audit configuration, even for a singleton
penalty grid whose fixed-lambda predictions do not change.

The API records externally **declared** fixed weights. It cannot authenticate
how the caller chose them. There is no learned-weight fitting route; target-aware
row preparation continues to fit only inside every scored training scope.
Any future learned scale/weight/covariance route must implement the same nested
training boundaries and retain its fit populations.

## Source, checks and evidence

Worktree: `/private/tmp/scalafim-umvpa-next-wave-20260930`, branch
`umvpa/predictive-relational-pattern-20260930`, base
`38f0e5febe693ea0a7751122ffddc7bc0b8788e7`. Initial frozen weighting source:
`c58f03c48945967021ad343958b9d7ac647fff83`. Final source after independent-review
repairs: `3753c062c0bbf1ed1da73f70ea2f695ab3778aab`. Only this documentation
is edited after that freeze.

The unchanged pins are Alder `e555bad92307af1c2cbc104aef398cb9d9de88f0`
and Gale `099832ff15c8a4a8fcf3398c7b779fb4bbc12434`. All solves use the actual
pinned Alder/Gale augmented-QR route. Builds use the existing two serial locks
and bounded JVM/JS invocations; unrelated checkouts and writers are preserved.

The local expert independently reviewed the contract against method notes,
architecture notes, the prior ridge adapter and its identity APIs, read-only at
`38f0e5fe`. This is design evidence, not compiled-source acceptance. Fray #143
@947 independently APPROVES source `3753c062` after review repairs: geometry
value equality/hashCode, canonical origin encoding, explicit normalized inner
numerators and a self-describing pooled metric with geometry and loss tag.
The reviewer withdrew the scope objection after reading the full method and
architecture passages: feature-block weights here refer to the target features
in axis-bound `Y`. Smart-construction `require` guards follow project guidance.

| Gate | Source | Result | Evidence |
| --- | --- | --- | --- |
| `mvpaDatasetJVM/test` | `3753c062` | 53 passed, warning-clean | `weighting-jvm-12.log` |
| `mvpaDatasetJS/test` | `3753c062` | 53 passed, warning-clean | `weighting-js-13.log` |
| `scalafimCompileAll` | `3753c062` | Exit 0, 89 tasks, warning-clean | `weighting-compile-all-14.log` |

These logs and exit metadata are retained in the existing evidence directory
`/private/tmp/scalafim-umvpa-next-wave-evidence-20260930`. The queued JVM command
started sbt only after the final review repairs were committed;
`jvm-gate-input.json`, `js-gate-input.json`, `compile-gate-input.json` and
`source-02.json` in the weighting evidence directory bind the unchanged three
source/suite files. Full raw logs were checked for warnings, errors, failures,
skipped and ignored tests; none were present. Earlier owning and consumer gates
are recorded in the previous frontier report; they are not new-source evidence.

Independent script and receipt:
`/private/tmp/scalafim-umvpa-weighting-evidence-20261001/weighted-oracle.py`
and `weighted-oracle-receipt.json`. NumPy centered normal-equation solves are
independent of production augmented QR. On a synthetic unequal-block fixture,
uniform coordinates select penalties `[0.1, 10, 2]`; equal block masses select
`[2, 10, 2]`. The full-row weighted refit selects `10`. The suite freezes loss
numerators/denominators, penalties, OOF predictions, weighted pooled metrics and
raw refit predictions at absolute tolerance `1e-10`.

Additional contracts exercise singleton-penalty prediction invariance while
plan identity changes, equal-weight parity through all previous frozen tests,
block-coordinate duplication and common-mass rescaling, consistent target
permutation, outer target-perturbation isolation, actual retained geometry, and
specific malformed/reordered-axis refusal before any encoder fit. Existing
inner target-aware leakage/population sentinels remain required.

This is local synthetic correctness and declared scientific-identity evidence.
No hosted checks, main merge, push, release, performance improvement, scientific
generalization, authenticated independence or inferential qualification is
claimed. Residual-covariance work is separately owned in the peer's M3.03
worktree and is not included in this weighting candidate.
