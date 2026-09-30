# Unified analysis: supplementary document review

Recorded: 2026-09-12

Disposition: selected material incorporated into
[PRD v0.2](unified-mvpa-prd.md). This is a review record, not a competing
implementation plan or evidence that the proposed runtime exists.

## Sources reviewed

Read all four supplied Markdown files in `/Users/bbuchsbaum/Downloads`.
The SHA-256 values identify the local content reviewed, not scientific approval.
The originals were not modified or copied wholesale into the repository.

| File | SHA-256 |
| --- | --- |
| `acceptance.md` | `3fa85068cf7a049b9c62693c0b8e98e6e3d1149b5cb2a98106a862de27ca8c30` |
| `implementation-plan.md` | `327f154d1b66bfc9bfc3863d08c7509aca9183cd290b8e0e63a39fbc5d729734` |
| `evidence-and-reuse.md` | `90873b0333d32e75741dc2cbe708db64ad0f92d0434ed3b57e810ae99020533f` |
| `interaction-protocol.md` | `9bd5fac8893e1c75d0ad4d309614ea52c224bb7aa80fa188cf74ce573c65103f` |

These documents refer to an adjacent architecture, schemas, registry, checker,
fixtures, and guarded patch helper. Those resources were not supplied in this
review. Their existence, contents, and validation claims are not adopted as
runtime or repository evidence. No external patch/helper was executed.

## Adopted or adapted

| Source sections | Decision and reason | PRD location |
| --- | --- | --- |
| Acceptance Q1, Q2, Q4 | Separate semantic, numerical, protocol, resource, and scientific evidence. Keep one scenario truth value, with explicit qualification facts and expected-refusal checks. | Section 10 |
| Evidence E1 | Separate question, analysis specification, evidence binding, and realization. This complements coordinate/membership/value distinctions, without four new identity libraries. Distinguish asserted digests from verified content. | FND-06 |
| Protocol P1–P5; implementation I2/M1 | Make metadata-only inspection testable with poison sources; preserve unknowns, bounded views, dependency explanations, and scientifically classified repairs. Use native descriptors, not a new service or string DSL. | FND-08 |
| Evidence E2; protocol P5 | Treat reuse as checked derivation with exact scope and implementation dependencies. Explain retained/invalidated computations when targets, folds, metrics, or queries change. | FND-09 |
| Evidence E3 | Retain scoped low-rank-query and all-distinct-pair rewrite candidates, with direct oracles and cancellation checks. Do not claim feature additivity under dense metrics. | PERF-01 |
| Evidence E4; acceptance S09–S10 | Track acquisition/readout origins and metric-learning assumptions. Disjoint effect rows or a “training” label do not establish unbiased cross-products. Computational edges are not independent inferential units. | REL-02 |
| Evidence E6; protocol P6/P8 | Record mediated holdout exposure and adaptive selection outside the numerical core. A cost pilot can expose data. External exposure remains unknown; ordinary Scala callbacks are not a security sandbox. | INF-05, PERF-02 |
| Evidence E7; protocol P7 | Require shared transformations and family-complete max-statistic replicates. Recompute randomized dependencies, including split construction when required. Budget stops cannot silently change the inference procedure. | INF-01 |
| Evidence E8; protocol P2/P7 | Preserve validity states and reducer denominators; distinguish useful partial maps from complete inference. Qualify fit-free reopening only for supported estimate/archive profiles. | FND-05, OPS-01 |
| Protocol P6/P7 | Account for live memory, backend unknowns, read/fit budgets, and explicit probes. Deduplicate retries; admit checkpoint/resume only with compatible revisions and reducer state. | PERF-02, OPS-02 |
| Implementation I2/M2 | Preserve exact current method semantics during migration. Select the existing Swift centroid to exercise training-fitted scaling; preserve the current identity-metric signed RDM before extending its noise model. | PRED-01, REL-01, M1–M2 |
| Implementation I3/I5; acceptance Q5 | Use bounded work packets and actual analyst/agent tasks to test API usability. Reuse existing tracking/scheduling; avoid whole-repository process for a one-seam task. | Sections 10–11 |

### Why the scientific additions matter

The previous PRD already required leakage-safe fitting and explicit inference.
The follow-up material identifies failures that those broad requirements can
miss: preprocessing can couple disjoint beta rows, a learned metric can bias a
cross-product, an analyst can select after seeing outer scores, and a partial
max-statistic family can invalidate otherwise successful local computations.
The revised requirements make those cases explicit and testable.

The identity proposals describe different levels rather than rival taxonomies.
Ordered coordinates determine compatible numerical operations. Question/spec/
evidence/realization bindings determine what was asked, fitted, observed, and
executed. Keeping both distinctions avoids both false incompatibility and stale
reuse.

## Not adopted as proposed

- **Delayed M7 compatibility cutover.** It conflicts with the user's request
  for prompt deletion. Keep the PRD's per-workflow replacement, bounded private
  bridges, and removal of superseded machinery by the M3 foundation release.
  Test new results against captured baselines; do not preserve the old façade
  merely to make a compatibility scenario executable forever.
- **Two slices as sufficient architecture proof.** Keep them as the first
  migration tests, but retain the third whole-brain pattern-fit/ROI-prediction
  slice. Local classification and local geometry do not prove global artifact
  support.
- **A complete wire/service protocol as a foundation prerequisite.** Adopt
  typed native operations and generated bounded views. Versioned decoder
  conformance applies when such adapters are offered; a network server,
  remote authority layer, or separate protocol registry is not required now.
- **Automatic promotion of design-package checks to runtime qualification.**
  Document/schema checks do not establish Scala compilation, JVM/JS parity,
  resource bounds, statistical calibration, or deployed services.
- **A general durable execution/campaign platform.** Use existing workflow,
  provenance, scheduler, and estimate/archive facilities. Recovery and exposure
  semantics must be explicit, but need not create another infrastructure system.

## Live source check and limits

Read-only inspection confirmed the relevant method baselines:

- [Classification.scala](../../modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/Classification.scala)
  contains Swift centroid's training-fitted scaling, class priors and softmax
  convention, and the separate correlation-centroid method. These are distinct
  algorithms, not interchangeable adapters.
- [OperatorRsa.scala](../../modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/OperatorRsa.scala)
  uses foldwise sufficient statistics and signed cross-validated geometry with
  optional feature normalization; that computation does not itself estimate a
  residual precision matrix.
- [Rdm.scala](../../modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/Rdm.scala)
  already uses the sum-product-minus-self-products identity for its all-distinct
  pairing. The proposal therefore calls for preserving and qualifying a useful
  kernel, not claiming that the algebra is newly implemented.

This review changed documentation only. Source inspection does not establish
current test success; no Scala build, simulation calibration, performance run,
provider admission, or publication was performed as part of this review.
