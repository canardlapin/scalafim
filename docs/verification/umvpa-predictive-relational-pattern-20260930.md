# UMVPA predictive, relational and pattern frontier

## Source and ownership

The candidate is local branch `umvpa/predictive-relational-pattern-20260930`
in `/private/tmp/scalafim-umvpa-next-wave-20260930`, based on
`22eafac801f781b2501e9e2c5241c5027a613cb0`. Tested source:
`a516b65dc98fc2e6d775b76b24f05daa4f38dd40`. The report is a subsequent
documentation-only commit. No merge, push, hosted check or release is claimed.
The dirty primary checkout and unrelated writers were preserved.

Four local delegated agents contributed Swift implementation, provider
assessment, ordered pairing and pattern artifacts. Runtime limits prevented
resuming those children; the parent completed integration and repairs. Existing
Fray peer `umvpa-helper-claude` implemented the regression scope and independently
reviewed the pattern source. Five participating agents were available, rather
than the requested six or seven. Mote held claims/reservations; Fray held review
and handoff discussion. Build/test execution was serialized by the existing
shared locks, with separate bounded Scala.js batches.

Provider pins are unchanged: Alder
`e555bad92307af1c2cbc104aef398cb9d9de88f0`, Gale
`099832ff15c8a4a8fcf3398c7b779fb4bbc12434`. The build adds only Alder
`modelsLinear` and `ridgeGale` dependencies to the two `mvpaDataset` platforms.
The tested build SHA-256 is
`37bf8917f6a31fb90f636ebf003f174a5a9baf5e9f2767d879734715d468353c`.

## Delivered behavior

* **M1.07:** Swift runs through actual Alder exhaustive holdouts bound to an
  exact-once `ValidationDesign`. One training-fitted Z-score stage, global class
  order, original sample-key OOF placement, training lineage, pooled accuracy
  denominators and confusion counts are retained. Missing training classes are
  explicitly refused. Frozen R parity and dense/operator whole/ROI/searchlight
  routes are exercised; preparation and validation receipts stay distinct.
* **M1.08, bounded implementation:** Scalar and separable multiresponse ridge
  use actual pinned Alder/Gale augmented-QR solves and component audits. One
  shared penalty is selected by pooled inner assessment loss. Preparation is
  cross-fitted separately inside every raw inner analysis scope before scoring
  its assessment rows. Named features/responses, keyed OOF predictions,
  per-output/pooled metrics, constant-target policy, finite-loss refusals and a
  separate declared domain-refit receipt are present.
* **M2.02:** Ordered partition edges retain endpoint identity and authoritative
  reducer weights. Shared acquisition/preparation, unknown support,
  endpoint-learned metrics and shared-partition dependence produce explicit
  claim reasons. Reduction preserves signed results; finite-weight/accumulator,
  duplicate/foreign/missing-edge and zero-denominator failures are refused.
* **M3.02:** Labelled factorized `A Cᵀ y` artifacts store target geometry,
  diagonal metric/prior declarations, factors, covariance declarations,
  centering/intercept receipts, gauge/rank evidence and training binding.
  Intercepts store actual neural means and are applied by `forwardMean`.
  Independent contractions and per-column scale/gauge tests exercise the model.
  The Alder attachment preserves the experimental artifact and actual audit.

## Verification

Raw logs, exit metadata, source manifests and the independent oracle are in
`/private/tmp/scalafim-umvpa-next-wave-evidence-20260930`.
`source-06.json` binds all 14 changed source/review files to the clean tested tree.

| Gate | Source | Result | Evidence |
| --- | --- | --- | --- |
| `mvpaDatasetJVM/test` | `a516b65d` | 45 passed | `dataset-consumers-jvm-08.log` |
| `mvpaJVM/test` | `a516b65d` | 239 passed | same log |
| `mvpaFitJVM/test` | `a516b65d` | 46 passed | same log |
| `mvpaSpatialJVM/test` | `a516b65d` | 27 passed | same log |
| `mvpaDatasetJS/test` | `a516b65d` | 45 passed | `dataset-core-js-09.log` |
| `mvpaJS/test` | `a516b65d` | 239 passed | same log |
| `mvpaFitJS/test` | `a516b65d` | 46 passed | `consumers-js-10.log` |
| `mvpaSpatialJS/test` | `a516b65d` | 26 passed | same log |
| `scalafimCompileAll` | `a516b65d` | Passed, warning-clean | `compile-all-11.log` |

Complete successful logs were inspected for warnings/errors/ignored tests.
The spatial count differs by one because its performance-receipt suite is JVM-only;
no comparative performance claim is made. There are 357 JVM and 356 JS test passes
in the final owning/consumer gates, without counting repeated earlier runs.
Earlier failures remain recorded: sandbox-only Ivy lock denial before compile;
initial test role error; pinned-provider `Seed` API mismatch; and one regression
test accessing `innerDesign` on the refit receipt instead of its selection
receipt. They were repaired before the final owning gates.

`ridge-oracle.py` independently recomputes centered normal-equation ridge using
NumPy 2.4.3, distinct from the production augmented-QR route. It reproduces
scalar/multiresponse OOF values and the separately selected all-row refit at
absolute tolerance `1e-10`; receipt `ridge-oracle-receipt.json`. The provider
tests also exercise outer/inner target-perturbation leakage sentinels and actual
target-aware encoder training populations. This is synthetic correctness
evidence, not performance or scientific generalization qualification.

## Independent review and limits

The local pattern agent's cross-scope source review is separately preserved in
`umvpa-m3-02-cross-scope-review-20260930.md`; it does not assert executed tests.
Fray #129 @873 approves the final pattern source after repeated independent
review and repairs. The final `PatternArtifact.scala` blob is `27114fc0`;
its difference from tested core `fd850b2c` was documentation only, followed by
the exact combined-source gates above. Parent review of the peer-authored ridge
found and repaired inner-preparation leakage, finite-selection/metric checks,
constant-target scope and overstated resource-budget wording (Fray #132).

M1.08 remains open: explicit feature-block weighting is not represented in the
ridge adapter. Unweighted named-response fixtures do not satisfy that complete
acceptance criterion. Its next consumer must carry identified block geometry
and weights through fitting, OOF results and prediction, with an independent
weighted oracle and training-only selection evidence. The current refit is a
separate domain declaration; it is not Alder application `Use.Refit` promotion.
Matrix-native ridge is explicitly refused. The solve budget counts planned
design/augmented-system shapes, excluding copies, vectors, encoder work and
read buffers; it is not a peak-memory or all-allocation bound.

Pattern artifacts remain `ExperimentalFitOnly`. The covariance cases declare
structure without stored loadings/Psi or a precision solve. Rank admission uses
normalized Gram Cholesky with an explicit tolerance, which squares conditioning
and can conservatively refuse ill-conditioned factors. Pending/declared rank
evidence does not become a verified rank proof. Target centering, training axes
and degeneracy are declarations; callers supply already-centered targets under
the receipt. The digest is checked against actual Alder training fingerprints,
but attachment is a carrier, not an authenticated numerical fitter. No decoding,
Bayesian inference, anatomical-network count, gauge identification or universal
parity claim follows from this artifact.

Pairing admits only explicitly declared endpoint-bound conditional reasoning.
It neither authenticates independence nor qualifies a naive standard error for
dependent pairs. Native measurement receipts do not prove IO confinement.
The epic and downstream inference/cutover packets remain open.
